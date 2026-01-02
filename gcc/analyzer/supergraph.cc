/* "Supergraph" classes that combine CFGs and callgraph into one digraph.
   Copyright (C) 2019-2026 Free Software Foundation, Inc.
   Contributed by David Malcolm <dmalcolm@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define INCLUDE_DEQUE
#include "analyzer/common.h"

#include "timevar.h"
#include "gimple-pretty-print.h"
#include "ordered-hash-map.h"
#include "options.h"
#include "cgraph.h"
#include "cfg.h"
#include "digraph.h"
#include "tree-cfg.h"
#include "cfganal.h"
#include "except.h"

#include "diagnostics/file-cache.h"

#include "analyzer/supergraph.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/region-model.h"
#include "analyzer/exploded-graph.h"

#if ENABLE_ANALYZER

namespace ana {

/* Get the function of the ultimate alias target being called at EDGE,
   if any.  */

function *
get_ultimate_function_for_cgraph_edge (cgraph_edge *edge)
{
  cgraph_node *ultimate_node = edge->callee->ultimate_alias_target ();
  if (!ultimate_node)
    return nullptr;
  return ultimate_node->get_fun ();
}

/* class saved_uids.

   In order to ensure consistent results without relying on the ordering
   of pointer values we assign a uid to each gimple stmt, globally unique
   across all functions.

   Normally, the stmt uids are a scratch space that each pass can freely
   assign its own values to.  However, in the case of LTO, the uids are
   used to associate call stmts with callgraph edges between the WPA phase
   (where the analyzer runs in LTO mode) and the LTRANS phase; if the
   analyzer changes them in the WPA phase, it leads to errors when
   streaming the code back in at LTRANS.
   lto_prepare_function_for_streaming has code to renumber the stmt UIDs
   when the code is streamed back out, but for some reason this isn't
   called for clones.

   Hence, as a workaround, this class has responsibility for tracking
   the original uids and restoring them once the pass is complete
   (in the supergraph dtor).  */

/* Give STMT a globally unique uid, storing its original uid so it can
   later be restored.  */

void
saved_uids::make_uid_unique (gimple *stmt)
{
  unsigned next_uid = m_old_stmt_uids.length ();
  unsigned old_stmt_uid = stmt->uid;
  stmt->uid = next_uid;
  m_old_stmt_uids.safe_push
    (std::pair<gimple *, unsigned> (stmt, old_stmt_uid));
}

/* Restore the saved uids of all stmts.  */

void
saved_uids::restore_uids () const
{
  unsigned i;
  std::pair<gimple *, unsigned> *pair;
  FOR_EACH_VEC_ELT (m_old_stmt_uids, i, pair)
    pair->first->uid = pair->second;
}

/* When building the supergraph, should STMT be handled
   along each out-edge in the CFG, or as separate superedge
   "within" the BB.  */

static bool
control_flow_stmt_p (const gimple &stmt)
{
  switch (gimple_code (&stmt))
    {
    case GIMPLE_COND:
    case GIMPLE_EH_DISPATCH:
    case GIMPLE_GOTO:
    case GIMPLE_SWITCH:
      return true;

    case GIMPLE_ASM:
    case GIMPLE_ASSIGN:
    case GIMPLE_CALL:
    case GIMPLE_DEBUG:
    case GIMPLE_LABEL:
    case GIMPLE_NOP:
    case GIMPLE_PREDICT:
    case GIMPLE_RESX:
    case GIMPLE_RETURN:
      return false;

    /* We don't expect to see any other statement kinds in the analyzer.  */
    default:
      internal_error ("unexpected gimple stmt code: %qs",
		      gimple_code_name[gimple_code (&stmt)]);
      break;
    }
}

/* supergraph's ctor.  Walk the callgraph, building supernodes for each
   CFG basic block, splitting the basic blocks at statements.  Join
   together the supernodes with interprocedural superedges as appropriate.
   Assign UIDs to the gimple stmts.  */

supergraph::supergraph (region_model_manager &mgr,
			logger *logger)
: m_next_snode_id (0)
{
  auto_timevar tv (TV_ANALYZER_SUPERGRAPH);

  LOG_FUNC (logger);

  /* For each BB, if present, the stmt that terminates it.  */
  typedef ordered_hash_map<basic_block, gimple *> bb_to_stmt_t;
  bb_to_stmt_t control_stmt_ending_bbs;

  /* First pass: make supernodes (and assign UIDs to the gimple stmts).  */
  {
    /* Sort the cgraph_nodes?  */
    cgraph_node *node;
    FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
      {
	function *fun = node->get_fun ();

	log_nesting_level log_sentinel (logger, "function: %qD", fun->decl);

	/* Ensure that EDGE_DFS_BACK is correct for every CFG edge in
	   the supergraph (by doing it per-function).  */
	auto_cfun cfun_sentinel (fun);
	mark_dfs_back_edges ();

	const int start_id = m_nodes.length ();

	basic_block bb;
	FOR_ALL_BB_FN (bb, fun)
	  if (gimple *final_control_stmt
		= populate_for_basic_block (bb, fun, logger))
	    control_stmt_ending_bbs.put (bb, final_control_stmt);

	const unsigned num_snodes = m_nodes.length () - start_id;
	m_function_to_num_snodes.put (fun, num_snodes);

	if (logger)
	  {
	    const int end_id = m_nodes.length () - 1;
	    logger->log ("SN: %i...%i: function %qD",
			 start_id, end_id, fun->decl);
	  }
      }
  }

  /* Second pass: make superedges between basic blocks.  */
  {
    /* Make superedges for CFG edges.  */
    for (bb_to_node_t::iterator iter = m_bb_to_final_node.begin ();
	 iter != m_bb_to_final_node.end ();
	 ++iter)
      {
	basic_block bb = (*iter).first;
	supernode *src_supernode = (*iter).second;

	gimple *control_stmt_ending_bb = nullptr;
	if (auto control_stmt_iter = control_stmt_ending_bbs.get (bb))
	  control_stmt_ending_bb = *control_stmt_iter;

	::edge cfg_edge;
	int idx;
	if (bb->succs)
	  FOR_EACH_VEC_ELT (*bb->succs, idx, cfg_edge)
	    {
	      basic_block dest_cfg_block = cfg_edge->dest;
	      supernode *dest_supernode
		= *m_bb_to_initial_node.get (dest_cfg_block);
	      add_sedges_for_cfg_edge (src_supernode,
				       dest_supernode,
				       cfg_edge,
				       control_stmt_ending_bb,
				       mgr,
				       logger);
	    }
      }
  }
}

/* Create a run of supernodes and superedges for the BB within FUN
   expressing all of the stmts apart from the final control flow stmt (if any).
   Return the control stmt that ends this bb, if any.  */

gimple *
supergraph::populate_for_basic_block (basic_block bb,
				      function *fun,
				      logger *logger)
{
  log_nesting_level sentinel (logger, "bb %i", bb->index);

  supernode *initial_snode_in_bb = add_node (fun, bb, logger);
  m_bb_to_initial_node.put (bb, initial_snode_in_bb);

  if (bb->index == ENTRY_BLOCK)
    /* Use the decl's location, rather than fun->function_start_locus,
       which leads to more readable output.  */
    initial_snode_in_bb->m_loc = DECL_SOURCE_LOCATION (fun->decl);
  else if (bb->index == EXIT_BLOCK)
    initial_snode_in_bb->m_loc = fun->function_end_locus;
  else if (gsi_end_p (gsi_start_bb (bb)))
    {
      /* BB has no stmts, and isn't the ENTRY or EXIT node.
	 Try to find a source location for it.  */
      if (bb->succs->length () == 1)
	{
	  auto outedge = (*bb->succs)[0];
	  if (useful_location_p (outedge->goto_locus))
	    {
	      /* We have an empty basic block with one out-edge,
		 perhaps part of an empty infinite loop.  */
	      if (logger)
		logger->log ("using location 0x%lx from outedge",
			     outedge->goto_locus);
	      initial_snode_in_bb->m_loc = outedge->goto_locus;
	    }
	}
    }

  initial_snode_in_bb->m_state_merger_node = true;

  gimple *final_control_flow_stmt = nullptr;

  /* Create a run of supernodes for the stmts in BB,
     connected by stmt_superedge.  */
  gimple_stmt_iterator gsi;
  supernode *prev_snode_in_bb = initial_snode_in_bb;
  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *next_stmt = gsi_stmt (gsi);

      if (logger)
	{
	  logger->start_log_line ();
	  logger->log_partial ("next_stmt: ");
	  pp_gimple_stmt_1 (logger->get_printer (), next_stmt,
			    0, (dump_flags_t)0);
	  logger->end_log_line ();
	}
      prev_snode_in_bb->m_loc = get_stmt_location (next_stmt, fun);
      prev_snode_in_bb->m_stmt_loc = next_stmt->location;
      m_node_for_stmt.insert ({next_stmt, prev_snode_in_bb});

      m_stmt_uids.make_uid_unique (next_stmt);

      if (auto glabel_ = dyn_cast<const glabel *>(next_stmt))
	{
	  /* Associate the GIMPLE_LABEL with its snode.  */
	  prev_snode_in_bb->m_label = gimple_label_label (glabel_);

	  /* Only create an snode for the label if it has location
	     information.  */
	  if (glabel_->location == UNKNOWN_LOCATION)
	    continue;
	}

      // handle control flow stmts on the edges
      if (control_flow_stmt_p (*next_stmt))
	{
	  final_control_flow_stmt = next_stmt;
	  break;
	}

      supernode *snode_after_next_stmt = add_node (fun, bb, logger);
      if (prev_snode_in_bb)
	{
	  std::unique_ptr<operation> op;
	  switch (gimple_code (next_stmt))
	    {
	    default:
	      gcc_unreachable ();
	      break;
	    case GIMPLE_ASM:
	      op = std::make_unique<gasm_op>
		(*as_a <const gasm *> (next_stmt));
	      break;
	    case GIMPLE_ASSIGN:
	      op = std::make_unique<gassign_op>
		(*as_a <const gassign *> (next_stmt));
	      break;
	    case GIMPLE_CALL:
	      op = call_and_return_op::make
		(*as_a <const gcall *> (next_stmt));
	      break;
	    case GIMPLE_PREDICT:
	      op = std::make_unique<predict_op> (*next_stmt);
	      break;
	    case GIMPLE_RESX:
	      op = std::make_unique<resx_op>
		(*as_a <const gresx *> (next_stmt));
	      break;
	    case GIMPLE_RETURN:
	      op = std::make_unique<greturn_op>
		(*as_a <const greturn *> (next_stmt));
	      break;

	    case GIMPLE_DEBUG:
	    case GIMPLE_LABEL:
	    case GIMPLE_NOP:
	      /* Treat all of these as no-ops within analyzer; though
		 perhaps we care about their locations.  */
	      break;
	    }

	  superedge *sedge
	    = new superedge (prev_snode_in_bb,
			     snode_after_next_stmt,
			     std::move (op),
			     nullptr);
	  add_edge (sedge);
	}
      prev_snode_in_bb = snode_after_next_stmt;
    }

  m_bb_to_final_node.put (bb, prev_snode_in_bb);

  return final_control_flow_stmt;
}

/* supergraph's dtor.  Reset stmt uids.  */

supergraph::~supergraph ()
{
  m_stmt_uids.restore_uids ();
}

/* Dump this graph in .dot format to PP, using DUMP_ARGS.
   Cluster the supernodes by function, then by BB from original CFG.  */

void
supergraph::dump_dot_to_pp (pretty_printer *pp,
			    const dump_args_t &dump_args) const
{
  graphviz_out gv (pp);

  pp_string (pp, "digraph \"");
  pp_write_text_to_stream (pp);
  pp_string (pp, "supergraph");
  pp_write_text_as_dot_label_to_stream (pp, /*for_record=*/false);
  pp_string (pp, "\" {\n");
  gv.indent ();

  gv.println ("overlap=false;");
  gv.println ("compound=true;");

  /* TODO: maybe (optionally) sub-subdivide by TU, for LTO; see also:
     https://gcc-python-plugin.readthedocs.io/en/latest/_images/sample-supergraph.png
  */

  /* Break out the supernodes into clusters by function.  */
  {
    cgraph_node *node;
    FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
    {
      function *fun = node->get_fun ();
      gcc_assert (fun);
      auto_cfun sentinel (fun);

      const char *funcname = function_name (fun);
      gv.println ("subgraph \"cluster_%s\" {",
		  funcname);
      gv.indent ();
      pp_printf (pp,
		 ("style=\"dashed\";"
		  " color=\"black\";"
		  " label=\"%s\";\n"),
		 funcname);

      if (loops_for_fn (fun))
	dump_dot_to_gv_for_loop (gv, dump_args, get_loop (fun, 0), fun);
      else
	{
	  basic_block bb;
	  FOR_ALL_BB_FN (bb, fun)
	    dump_dot_to_gv_for_bb (gv, dump_args, bb, fun);
	}

      /* Terminate per-function "subgraph" */
      gv.outdent ();
      gv.println ("}");
    }
  }

  /* Superedges.  */
  int i;
  superedge *e;
  FOR_EACH_VEC_ELT (m_edges, i, e)
    e->dump_dot (&gv, dump_args);

  if (dump_args.m_node_annotator)
    dump_args.m_node_annotator->add_extra_objects (&gv);

  /* Terminate "digraph" */
  gv.outdent ();
  gv.println ("}");
}

/* Recursively dump all the snodes within LOOP and the loops
   within it.  */

void
supergraph::dump_dot_to_gv_for_loop (graphviz_out &gv,
				     const dump_args_t &dump_args,
				     class loop *loop,
				     function *fun) const
{
  pretty_printer *pp = gv.get_pp ();

  basic_block *body;
  unsigned int i;
  // Adapted from graph.cc:draw_cfg_nodes_for_loop
  const char *fillcolors[3] = { "grey88", "grey77", "grey66" };

  if (loop->header != NULL
      && loop->latch != EXIT_BLOCK_PTR_FOR_FN (cfun))
    pp_printf (pp,
	       "\tsubgraph cluster_%d_%d {\n"
	       "\tstyle=\"filled\";\n"
	       "\tcolor=\"darkgreen\";\n"
	       "\tfillcolor=\"%s\";\n"
	       "\tlabel=\"loop %d\";\n"
	       "\tlabeljust=l;\n"
	       "\tpenwidth=2;\n",
	       fun->funcdef_no, loop->num,
	       fillcolors[(loop_depth (loop) - 1) % 3],
	       loop->num);

  // Recurse
  for (class loop *inner = loop->inner; inner; inner = inner->next)
    dump_dot_to_gv_for_loop (gv, dump_args, inner, fun);

  if (loop->header == NULL)
    return;

  if (loop->latch == EXIT_BLOCK_PTR_FOR_FN (cfun))
    body = get_loop_body (loop);
  else
    body = get_loop_body_in_bfs_order (loop);

  for (i = 0; i < loop->num_nodes; i++)
    {
      basic_block bb = body[i];
      if (bb->loop_father == loop)
	dump_dot_to_gv_for_bb (gv, dump_args, bb, fun);
    }

  free (body);

  if (loop->latch != EXIT_BLOCK_PTR_FOR_FN (cfun))
    pp_printf (pp, "\t}\n");
}

/* Dump all the snodes from BB.  */

void
supergraph::dump_dot_to_gv_for_bb (graphviz_out &gv,
				   const dump_args_t &dump_args,
				   basic_block bb,
				   function *fun) const
{
  pretty_printer *pp = gv.get_pp ();

  if (dump_args.m_flags & SUPERGRAPH_DOT_SHOW_BBS)
    {
      const char *funcname = function_name (fun);
      gv.println ("subgraph \"cluster_%s_bb_%i\" {",
		  funcname, bb->index);
      gv.indent ();
      pp_printf (pp,
		 ("style=\"dashed\";"
		  " color=\"black\";"
		  " label=\"bb: %i\";\n"),
		 bb->index);
    }

  // TODO: maybe keep an index per-function/per-bb to speed this up???
  int i;
  supernode *n;
  FOR_EACH_VEC_ELT (m_nodes, i, n)
    if (n->m_fun == fun && n->m_bb == bb)
      n->dump_dot (&gv, dump_args);

  if (dump_args.m_flags & SUPERGRAPH_DOT_SHOW_BBS)
    {
      /* Terminate per-bb "subgraph" */
      gv.outdent ();
      gv.println ("}");
    }
}


/* Dump this graph in .dot format to FP, using DUMP_ARGS.  */

void
supergraph::dump_dot_to_file (FILE *fp, const dump_args_t &dump_args) const
{
  std::unique_ptr<pretty_printer> pp (global_dc->clone_printer ());
  pp_show_color (pp.get ()) = 0;
  /* %qE in logs for SSA_NAMEs should show the ssa names, rather than
     trying to prettify things by showing the underlying var.  */
  pp_format_decoder (pp.get ()) = default_tree_printer;

  pp->set_output_stream (fp);
  dump_dot_to_pp (pp.get (), dump_args);
  pp_flush (pp.get ());
}

/* Dump this graph in .dot format to PATH, using DUMP_ARGS.  */

void
supergraph::dump_dot (const char *path, const dump_args_t &dump_args) const
{
  FILE *fp = fopen (path, "w");
  dump_dot_to_file (fp, dump_args);
  fclose (fp);
}

/* Return a new json::object of the form
   {"nodes" : [objs for snodes],
    "edges" : [objs for sedges]}.  */

std::unique_ptr<json::object>
supergraph::to_json () const
{
  auto sgraph_obj = std::make_unique<json::object> ();

  /* Nodes.  */
  {
    auto nodes_arr = std::make_unique<json::array> ();
    unsigned i;
    supernode *n;
    FOR_EACH_VEC_ELT (m_nodes, i, n)
      nodes_arr->append (n->to_json ());
    sgraph_obj->set ("nodes", std::move (nodes_arr));
  }

  /* Edges.  */
  {
    auto edges_arr = std::make_unique<json::array> ();
    unsigned i;
    superedge *n;
    FOR_EACH_VEC_ELT (m_edges, i, n)
      edges_arr->append (n->to_json ());
    sgraph_obj->set ("edges", std::move (edges_arr));
  }

  return sgraph_obj;
}

/* Create a supernode within BB within FUN and add it to this supergraph.  */

supernode *
supergraph::add_node (function *fun, basic_block bb, logger *logger)
{
  supernode *n = new supernode (fun, bb, m_next_snode_id++);
  m_snode_by_id.push_back (n);
  m_nodes.safe_push (n);
  if (logger)
    logger->log ("created SN %i", n->m_id);
  return n;
}

void
supergraph::delete_nodes (const std::set<supernode *> &snodes_to_delete)
{
  /* Remove nodes from m_nodes.  */
  unsigned read_index, write_index;
  supernode **elem_ptr;
  VEC_ORDERED_REMOVE_IF
    (m_nodes, read_index, write_index, elem_ptr,
     snodes_to_delete.find (*elem_ptr) != snodes_to_delete.end ()
     );

  /* Remove nodes from m_snode_by_id, and delete them.  */
  for (auto iter : snodes_to_delete)
    {
      gcc_assert (iter->m_preds.length () == 0);
      gcc_assert (iter->m_succs.length () == 0);
      m_snode_by_id[iter->m_id] = nullptr;
      delete iter;
    }
}

/* Create a chain of nodes and edges in this supergraph from SRC to DEST
   to handle CFG_EDGE in the underlying CFG:

    +---+
    |SRC|
    +---+
      |
      | optional edge for ctrlflow_stmt (if CTRLFLOW_STMT is non-null)
      | (e.g. checking conditions on a GIMPLE_COND)
      |
      V
   +------+
   |(node)|
   +------+
      |
      | optional edge for any phi nodes in the destination basic block
      |
      V
   +------+
   |(node)|
   +------+
      |
      | optional edge for state merging at CFG join points
      |
      V
   +----+
   |DEST|
   +----+

   adding nodes where necessary between the edges, and adding a no-op edge
   for the case where there is no CTRLFLOW_STMT, phi nodes, or
   state merging.  */

void
supergraph::add_sedges_for_cfg_edge (supernode *src,
				     supernode *dest,
				     ::edge cfg_edge,
				     gimple *ctrlflow_stmt,
				     region_model_manager &mgr,
				     logger *logger)
{
  log_nesting_level sentinel (logger,
			      "edge: bb %i -> bb %i",
			      cfg_edge->src->index,
			      cfg_edge->dest->index);
  std::unique_ptr<operation> ctrlflow_op;
  if (ctrlflow_stmt)
    switch (gimple_code (ctrlflow_stmt))
      {
      default:
	gcc_unreachable ();
	break;
      case GIMPLE_COND:
	ctrlflow_op = std::make_unique<gcond_edge_op>
	  (cfg_edge,
	   *as_a <gcond *> (ctrlflow_stmt));
	break;
      case GIMPLE_EH_DISPATCH:
	ctrlflow_op
	  = eh_dispatch_edge_op::make (src, dest,
				       cfg_edge,
				       *as_a <geh_dispatch *> (ctrlflow_stmt));
	break;
      case GIMPLE_GOTO:
	{
	  ctrlflow_op = std::make_unique<ggoto_edge_op>
	    (cfg_edge,
	     *as_a <ggoto *> (ctrlflow_stmt),
	     dest->m_label);
	}
	break;
      case GIMPLE_SWITCH:
	ctrlflow_op = std::make_unique<switch_case_op>
	  (*src->get_function (),
	   cfg_edge,
	   *as_a <gswitch *> (ctrlflow_stmt),
	   *mgr.get_range_manager ());
	break;
      }
  else
    {
      if (cfg_edge->flags & EDGE_ABNORMAL)
	/* Don't create superedges for such CFG edges (though
	   computed gotos are handled by the GIMPLE_GOTO clause above).  */
	return;
    }

  /* Determine a location to use for any snodes within the CFG edge.  */
  location_t dest_loc = dest->m_loc;
  if (useful_location_p (cfg_edge->goto_locus))
    dest_loc = cfg_edge->goto_locus;

  /* If the dest is a control flow join point, then for each CFG in-edge
     add an extra snode/sedge before DEST and route to it.
     We hope this will help state-merging keep the
     different in-edges separately.  */
  if (cfg_edge->dest->preds->length () > 1
      && cfg_edge->dest->index != EXIT_BLOCK)
    {
      auto extra_snode = add_node (src->get_function (),
				   cfg_edge->dest,
				   logger);
      extra_snode->m_loc = dest_loc;
      extra_snode->m_preserve_p = true;
      extra_snode->m_state_merger_node = true;
      add_edge (new superedge (extra_snode, dest, nullptr, nullptr));
      dest = extra_snode;
    }

  std::unique_ptr<operation> phi_op;
  if (phi_nodes (cfg_edge->dest))
    phi_op = phis_for_edge_op::maybe_make (cfg_edge);
  if (phi_op)
    {
      superedge *edge_for_phis_op;
      if (ctrlflow_op)
	{
	  /* We have two ops; add an edge for each:
	     SRC --{ctrlflow_op}--> before_phi_nodes --{phi_op}--> DEST.  */
	  supernode *before_phi_nodes
	    = add_node (src->get_function (), cfg_edge->src, logger);
	  before_phi_nodes->m_loc = dest_loc;
	  add_edge (new superedge (src, before_phi_nodes,
				   std::move (ctrlflow_op),
				   cfg_edge));
	  edge_for_phis_op = new superedge (before_phi_nodes, dest,
					    std::move (phi_op),
					    cfg_edge);
	}
      else
	/* We just have a phi_op; add: SRC --{phi_op}--> DEST.  */
	edge_for_phis_op
	  = new superedge (src, dest, std::move (phi_op), cfg_edge);
      add_edge (edge_for_phis_op);
      m_edges_for_phis.insert ({cfg_edge, edge_for_phis_op});
    }
  else
    /* We don't have a phi op, create this edge:
       SRC --{ctrlflow_op}--> DEST
       where ctrlflow_op might be nullptr (for a no-op edge).  */
    add_edge (new superedge (src, dest, std::move (ctrlflow_op), cfg_edge));
}

// class supernode : public dnode<supergraph_traits>

/* Implementation of dnode::dump_dot vfunc for supernodes.

   Write a cluster for the node, and within it a .dot node showing
   the phi nodes and stmts.  Call into any node annotator from ARGS to
   potentially add other records to the cluster.  */

void
supernode::dump_dot (graphviz_out *gv, const dump_args_t &args) const
{
  pretty_printer *pp = gv->get_pp ();

  gv->write_indent ();
  dump_dot_id (pp);

  pp_printf (pp,
	     " [shape=none,margin=0,style=filled,label=<");
  pp_string (pp, "<TABLE BORDER=\"0\">");
  pp_write_text_to_stream (pp);

  pp_printf (pp, "<TR><TD>sn: %i (bb: %i)",
	     m_id, m_bb->index);
  if (args.m_eg)
    pp_printf (pp, "; scc: %i", args.m_eg->get_scc_id (*this));
  pp_string (pp, "</TD></TR>");
  pp_newline (pp);

  if (m_preserve_p)
    {
      pp_string (pp, "<TR><TD>(preserve)</TD></TR>");
      pp_newline (pp);
    }
  if (m_state_merger_node)
    {
      pp_string (pp, "<TR><TD BGCOLOR=\"#ec7a08\">"
		 "<FONT COLOR=\"white\">STATE MERGER</FONT></TD></TR>");
      pp_newline (pp);
    }
  if (entry_p ())
    {
      pp_string (pp, "<TR><TD>ENTRY</TD></TR>");
      pp_newline (pp);
    }
  else if (exit_p ())
    {
      pp_string (pp, "<TR><TD>EXIT</TD></TR>");
      pp_newline (pp);
    }

  /* Source location.  */
  /* Highlight nodes where we're missing source location information.
     Ideally this all gets fixed up by supergraph::fixup_locations.  */
  if (m_loc == UNKNOWN_LOCATION)
    pp_string (pp, "<TR><TD>UNKNOWN_LOCATION</TD></TR>");
  else if (get_pure_location (m_loc) == UNKNOWN_LOCATION)
    {
      pp_printf (pp, "<TR><TD>location: 0x%lx</TD></TR>", m_loc);
      pp_string (pp, "<TR><TD>UNKNOWN_LOCATION</TD></TR>");
    }
  else
    {
      /* Show the source location, but skip it for the case where it's
	 the same as all previous snodes (and there's a single in-edge).  */
      bool show_location = true;
      location_t prev_loc = UNKNOWN_LOCATION;
      if (m_preds.length () == 1)
	{
	  prev_loc = m_preds[0]->m_src->m_loc;
	  if (prev_loc == m_loc)
	    show_location = false;
	}
      if (show_location)
	{
	  pp_printf (pp, "<TR><TD>location: 0x%lx</TD></TR>", m_loc);

	  /* Print changes to the expanded location (or all of it if
	     we have multiple in-edges).  */
	  auto prev_exploc = expand_location (prev_loc);
	  auto exploc = expand_location (m_loc);

	  if ((exploc.file != prev_exploc.file)
	      && exploc.file)
	    {
	      pp_string (pp, "<TR><TD>");
	      pp_flush (pp);
	      pp_printf (pp, "%s", exploc.file);
	      /* Escape, to handle cases like "<built-in>".  */
	      pp_write_text_as_html_like_dot_to_stream (pp);
	      pp_printf (pp, ":%i:</TD></TR>", exploc.line);
	    }
	  if (exploc.line != prev_exploc.line)
	    if (const diagnostics::char_span line
		= global_dc->get_file_cache ().get_source_line (exploc.file,
								exploc.line))
	      {
		/* Print the source line.  */
		pp_string (pp, "<TR><TD>");
		pp_string (pp, "<TABLE BORDER=\"0\"><TR>");

		// Line number:
		pp_printf (pp, ("<TD ALIGN=\"RIGHT\" BGCOLOR=\"#0088ce\">"
				"<FONT COLOR=\"white\"> %i</FONT></TD>"),
			   exploc.line);

		// Line contents:
		pp_string (pp, "<TD ALIGN=\"LEFT\" BGCOLOR=\"white\">");
		pp_flush (pp);
		for (size_t i = 0; i < line.length (); ++i)
		  pp_character (pp, line[i]);
		pp_write_text_as_html_like_dot_to_stream (pp);
		pp_string (pp, "</TD>");

		pp_string (pp, "</TR></TABLE>");
		pp_string (pp, "</TD></TR>");
	      }
	}
    }

  pp_flush (pp);

  if (args.m_node_annotator)
    args.m_node_annotator->add_node_annotations (gv, *this);

  pp_printf (pp, "<TR><TD>m_stmt_loc: 0x%lx</TD></TR>", m_stmt_loc);

  pp_string (pp, "</TABLE>>];\n\n");
  pp_flush (pp);
}

/* Write an ID for this node to PP, for use in .dot output.  */

void
supernode::dump_dot_id (pretty_printer *pp) const
{
  pp_printf (pp, "node_%i", m_id);
}

/* Return a new json::object of the form
   {"id": int,
    "fun": optional str
    "bb_idx": int}.  */

std::unique_ptr<json::object>
supernode::to_json () const
{
  auto snode_obj = std::make_unique<json::object> ();

  snode_obj->set_integer ("id", m_id);
  snode_obj->set_integer ("bb_idx", m_bb->index);
  if (function *fun = get_function ())
    snode_obj->set_string ("fun", function_name (fun));

  return snode_obj;
}

/* Dump this superedge to PP.  */

void
superedge::dump (pretty_printer *pp) const
{
  pp_printf (pp, "edge: SN: %i -> SN: %i", m_src->m_id, m_dest->m_id);
  label_text desc (get_description (false));
  if (strlen (desc.get ()) > 0)
    {
      pp_space (pp);
      pp_string (pp, desc.get ());
    }
}

/* Dump this superedge to stderr.  */

DEBUG_FUNCTION void
superedge::dump () const
{
  tree_dump_pretty_printer pp (stderr);
  dump (&pp);
  pp_newline (&pp);
}

/* Implementation of dedge::dump_dot for superedges.
   Write a .dot edge to GV representing this superedge.  */

void
superedge::dump_dot (graphviz_out *gv, const dump_args_t &) const
{
  const char *style = "\"solid,bold\"";
  const char *color = "black";
  int weight = 10;
  const char *constraint = "true";

  /* Adapted from graph.cc:draw_cfg_node_succ_edges.  */
  if (::edge cfg_edge = get_any_cfg_edge ())
    {
      if (cfg_edge->flags & EDGE_FAKE)
	{
	  style = "dotted";
	  color = "green";
	  weight = 0;
	}
      else if (cfg_edge->flags & EDGE_DFS_BACK)
	{
	  style = "\"dotted,bold\"";
	  color = "blue";
	  weight = 10;
	}
      else if (cfg_edge->flags & EDGE_FALLTHRU)
	{
	  color = "blue";
	  weight = 100;
	}

      if (cfg_edge->flags & EDGE_ABNORMAL)
	color = "red";
    }

  gv->write_indent ();

  pretty_printer *pp = gv->get_pp ();

  m_src->dump_dot_id (pp);
  pp_string (pp, " -> ");
  m_dest->dump_dot_id (pp);
  pp_printf (pp,
	     (" [style=%s, color=%s, weight=%d, constraint=%s,"
	      " headlabel=\""),
	     style, color, weight, constraint);
  pp_flush (pp);

  dump_label_to_pp (pp, false);
  pp_write_text_as_dot_label_to_stream (pp, false);

  pp_printf (pp, "\"];\n");
}

/* Return a new json::object of the form
   {"src_id": int, the index of the source supernode,
    "dst_id": int, the index of the destination supernode} */

std::unique_ptr<json::object>
superedge::to_json () const
{
  auto sedge_obj = std::make_unique<json::object> ();
  sedge_obj->set_integer ("src_id", m_src->m_id);
  sedge_obj->set_integer ("dst_id", m_dest->m_id);
  return sedge_obj;
}

/* Return true iff this edge needs to be preserved during simplification.  */

bool
superedge::preserve_p () const
{
  if (m_cfg_edge)
    if (m_cfg_edge->flags & (EDGE_EH | EDGE_DFS_BACK))
      {
	/* We use EDGE_EH in get_eh_outedge, and EDGE_DFS_BACK
	   for detecting infinite loops.  */
	return true;
      }
  return false;
}

/* Build a description of this superedge (e.g. "true" for the true
   edge of a conditional, or "case 42:" for a switch case).

   If USER_FACING is false, the result also contains any underlying
   CFG edge flags. e.g. " (flags FALLTHRU | DFS_BACK)".  */

label_text
superedge::get_description (bool user_facing) const
{
  pretty_printer pp;
  pp_format_decoder (&pp) = default_tree_printer;
  dump_label_to_pp (&pp, user_facing);
  return label_text::take (xstrdup (pp_formatted_text (&pp)));
}

void
superedge::dump_label_to_pp (pretty_printer *pp, bool user_facing) const
{
  if (get_op ())
    get_op ()->print_as_edge_label (pp, user_facing);
  else
    pp_printf (pp, "no-op");

  if (user_facing)
    return;

  if (::edge cfg_edge = get_any_cfg_edge ())
    {
      if (cfg_edge->flags)
	{
	  pp_string (pp, " (flags ");
	  bool seen_flag = false;
#define DEF_EDGE_FLAG(NAME,IDX)			\
	  do {						\
	    if (cfg_edge->flags & EDGE_##NAME)		\
	      {						\
		if (seen_flag)					\
		  pp_string (pp, " | ");			\
		pp_printf (pp, "%s", (#NAME));			\
		seen_flag = true;				\
	      }						\
	  } while (0);
#include "cfg-flags.def"
#undef DEF_EDGE_FLAG
	  pp_string (pp, ")");
	}
      if (cfg_edge->goto_locus > BUILTINS_LOCATION)
	pp_printf (pp, " (has goto_locus: 0x%lx)", cfg_edge->goto_locus);
    }
}

bool
superedge::supports_bulk_merge_p () const
{
  if (!m_op)
    return true;
  return m_op->supports_bulk_merge_p ();
}

} // namespace ana

#endif /* #if ENABLE_ANALYZER */
