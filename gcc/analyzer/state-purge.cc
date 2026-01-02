/* Classes for purging state at function_points.
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

#define INCLUDE_SET
#include "analyzer/common.h"

#include "timevar.h"
#include "gimple-pretty-print.h"
#include "tree-vrp.h"
#include "gimple-ssa.h"
#include "stringpool.h"
#include "tree-ssanames.h"
#include "tree-phinodes.h"
#include "options.h"
#include "ssa-iterators.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "cgraph.h"

#include "analyzer/call-string.h"
#include "analyzer/supergraph.h"
#include "analyzer/program-point.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/state-purge.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"

#if ENABLE_ANALYZER

/* Given NODE at an access, determine if this access is within
   a decl that could be consider for purging, and if so, return the decl.  */

static tree
get_candidate_for_purging (tree node)
{
  tree iter = node;
  while (1)
    switch (TREE_CODE (iter))
      {
      default:
	return NULL_TREE;

      case ADDR_EXPR:
      case MEM_REF:
      case COMPONENT_REF:
	iter = TREE_OPERAND (iter, 0);
	continue;

      case VAR_DECL:
	if (is_global_var (iter))
	  return NULL_TREE;
	else
	  return iter;

      case PARM_DECL:
      case RESULT_DECL:
	return iter;
      }
}

/* Class-based handler for walk_stmt_load_store_addr_ops at a particular
   ana::operation, for populating the worklists within a state_purge_map.  */

class gimple_op_visitor : public log_user
{
public:
  gimple_op_visitor (state_purge_map *map,
		     const superedge &sedge)
  : log_user (map->get_logger ()),
    m_map (map),
    m_sedge (sedge),
    m_fun (sedge.m_src->get_function ())
  {
    gcc_assert (m_fun);
  }

  bool on_load (gimple *stmt, tree base, tree op)
  {
    LOG_FUNC (get_logger ());
    if (get_logger ())
      {
	pretty_printer pp;
	pp_gimple_stmt_1 (&pp, stmt, 0, (dump_flags_t)0);
	log ("on_load: %s; base: %qE, op: %qE",
	     pp_formatted_text (&pp), base, op);
      }
    if (tree node = get_candidate_for_purging (base))
      add_needed (node);
    return true;
  }

  bool on_store (gimple *stmt, tree base, tree op)
  {
    LOG_FUNC (get_logger ());
    if (get_logger ())
      {
	pretty_printer pp;
	pp_gimple_stmt_1 (&pp, stmt, 0, (dump_flags_t)0);
	log ("on_store: %s; base: %qE, op: %qE",
	     pp_formatted_text (&pp), base, op);
      }
    return true;
  }

  bool on_addr (gimple *stmt, tree base, tree op)
  {
    LOG_FUNC (get_logger ());
    if (get_logger ())
      {
	pretty_printer pp;
	pp_gimple_stmt_1 (&pp, stmt, 0, (dump_flags_t)0);
	log ("on_addr: %s; base: %qE, op: %qE",
	     pp_formatted_text (&pp), base, op);
      }
    if (TREE_CODE (op) != ADDR_EXPR)
      return true;
    if (tree node = get_candidate_for_purging (base))
      {
	add_needed (node);
	add_pointed_to (node);
      }
    return true;
  }

private:
  void add_needed (tree decl)
  {
    gcc_assert (get_candidate_for_purging (decl) == decl);
    state_purge_per_decl &data
      = get_or_create_data_for_decl (decl);
    data.add_needed_at (*m_sedge.m_src);
  }

  void add_pointed_to (tree decl)
  {
    gcc_assert (get_candidate_for_purging (decl) == decl);
    get_or_create_data_for_decl (decl).add_pointed_to_at (*m_sedge.m_src);
  }

  state_purge_per_decl &
  get_or_create_data_for_decl (tree decl)
  {
    return m_map->get_or_create_data_for_decl (*m_fun, decl);
  }

  state_purge_map *m_map;
  const superedge &m_sedge;
  const function *m_fun;
};

static bool
my_load_cb  (gimple *stmt, tree base, tree op, void *user_data)
{
  gimple_op_visitor *x = (gimple_op_visitor *)user_data;
  return x->on_load (stmt, base, op);
}

static bool
my_store_cb  (gimple *stmt, tree base, tree op, void *user_data)
{
  gimple_op_visitor *x = (gimple_op_visitor *)user_data;
  return x->on_store (stmt, base, op);
}

static bool
my_addr_cb  (gimple *stmt, tree base, tree op, void *user_data)
{
  gimple_op_visitor *x = (gimple_op_visitor *)user_data;
  return x->on_addr (stmt, base, op);
}

/* state_purge_map's ctor.  Walk all SSA names in all functions, building
   a state_purge_per_ssa_name instance for each.
   Also, walk all loads and address-taken ops of local variables, building
   a state_purge_per_decl as appropriate.  */

state_purge_map::state_purge_map (const supergraph &sg,
				  region_model_manager *mgr,
				  logger *logger)
: log_user (logger), m_sg (sg)
{
  LOG_FUNC (logger);

  auto_timevar tv (TV_ANALYZER_STATE_PURGE);

  cgraph_node *node;
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
  {
    function *fun = node->get_fun ();
    gcc_assert (fun);
    if (logger)
      log ("function: %s", function_name (fun));
    tree name;
    unsigned int i;;
    FOR_EACH_SSA_NAME (i, name, fun)
      {
	/* For now, don't bother tracking the .MEM SSA names.  */
	if (tree var = SSA_NAME_VAR (name))
	  if (TREE_CODE (var) == VAR_DECL)
	    if (VAR_DECL_IS_VIRTUAL_OPERAND (var))
	      continue;
	m_ssa_map.put (name, new state_purge_per_ssa_name (*this, name, *fun));
      }
  }

  /* Find all uses of local vars.
     We iterate through all operations, finding loads, stores, and
     address-taken operations on locals, building a pair of worklists.  */
  for (auto sedge : sg.m_edges)
    {
      if (logger)
	log ("edge: SN %i -> SN %i",
	     sedge->m_src->m_id,
	     sedge->m_dest->m_id);
      if (auto op = sedge->get_op ())
	{
	  gimple_op_visitor v (this, *sedge);
	  op->walk_load_store_addr_ops (&v,
					my_load_cb, my_store_cb, my_addr_cb);
	}
    }

  /* Now iterate through the m_decl_map, processing each pair of worklists.  */
  for (state_purge_map::decl_iterator iter = begin_decls ();
       iter != end_decls ();
       ++iter)
    {
      state_purge_per_decl *per_decl_data = (*iter).second;
      per_decl_data->process_worklists (*this, mgr);
    }
}

/* state_purge_map's dtor.  */

state_purge_map::~state_purge_map ()
{
  for (auto iter : m_ssa_map)
    delete iter.second;
  for (auto iter : m_decl_map)
    delete iter.second;
}

/* Get the state_purge_per_decl for local DECL within FUN, creating it
   if necessary.  */

state_purge_per_decl &
state_purge_map::get_or_create_data_for_decl (const function &fun, tree decl)
{
  if (state_purge_per_decl **slot
      = const_cast <decl_map_t&> (m_decl_map).get (decl))
    return **slot;
  state_purge_per_decl *result = new state_purge_per_decl (*this, decl, fun);
  m_decl_map.put (decl, result);
  return *result;
}

void
state_purge_map::on_duplicated_node (const supernode &old_snode,
				     const supernode &new_snode)
{
  for (auto iter : m_ssa_map)
    iter.second->on_duplicated_node (old_snode, new_snode);
  for (auto iter : m_decl_map)
    iter.second->on_duplicated_node (old_snode, new_snode);
}

/* class state_purge_per_ssa_name : public state_purge_per_tree.  */

/* state_purge_per_ssa_name's ctor.

   Locate all uses of VAR within FUN.
   Walk backwards from each use, marking program points, until
   we reach the def stmt, populating m_points_needing_var.

   We have to track program points rather than
   just stmts since there could be empty basic blocks on the way.  */

state_purge_per_ssa_name::state_purge_per_ssa_name (const state_purge_map &map,
						    tree name,
						    const function &fun)
: state_purge_per_tree (fun), m_snodes_needing_name (), m_name (name)
{
  LOG_FUNC (map.get_logger ());

  if (map.get_logger ())
    {
      map.log ("SSA name: %qE within %qD", name, fun.decl);

      /* Show def stmt.  */
      const gimple *def_stmt = SSA_NAME_DEF_STMT (name);
      pretty_printer pp;
      pp_gimple_stmt_1 (&pp, def_stmt, 0, (dump_flags_t)0);
      map.log ("def stmt: %s", pp_formatted_text (&pp));
    }

  auto_vec<const supernode *> worklist;

  /* Add all immediate uses of name to the worklist.
     Compare with debug_immediate_uses.  */
  imm_use_iterator iter;
  use_operand_p use_p;
  FOR_EACH_IMM_USE_FAST (use_p, iter, name)
    {
      if (USE_STMT (use_p))
	{
	  const gimple *use_stmt = USE_STMT (use_p);
	  if (map.get_logger ())
	    {
	      pretty_printer pp;
	      pp_gimple_stmt_1 (&pp, use_stmt, 0, (dump_flags_t)0);
	      map.log ("used by stmt: %s", pp_formatted_text (&pp));
	    }

	  if (is_gimple_debug (use_stmt))
	    {
	      /* We skipped debug stmts when building the supergraph,
		 so ignore them now.  */
	      if (map.get_logger ())
		map.log ("skipping debug stmt");
	      continue;
	    }

	  /* If it's a use within a phi node, then we care about
	     which in-edge we came from.  */
	  if (use_stmt->code == GIMPLE_PHI)
	    {
	      const gphi *phi = as_a <const gphi *> (use_stmt);
	      /* Find arguments (and thus CFG in-edges) which use NAME.  */
	      for (unsigned arg_idx = 0;
		   arg_idx < gimple_phi_num_args (phi);
		   ++arg_idx)
		{
		  if (name == gimple_phi_arg (phi, arg_idx)->def)
		    {
		      edge in_edge = gimple_phi_arg_edge (phi, arg_idx);
		      const superedge *in_sedge
			= map.get_sg ().get_superedge_for_phis (in_edge);
		      if (in_sedge)
			{
			  add_to_worklist (*in_sedge->m_src,
					   &worklist,
					   map.get_logger ());
			  m_snodes_needing_name.add (in_sedge->m_src);
			}
		      else
			{
			  /* Should only happen for abnormal edges, which
			     get skipped in supergraph construction.  */
			  gcc_assert (in_edge->flags & EDGE_ABNORMAL);
			}
		    }
		}
	    }
	  else
	    {
	      const supernode *snode
		= map.get_sg ().get_supernode_for_stmt (use_stmt);
	      add_to_worklist (*snode, &worklist, map.get_logger ());
	      m_snodes_needing_name.add (snode);
	    }
	}
    }

  /* Process worklist by walking backwards until we reach the def stmt.  */
  {
    log_scope s (map.get_logger (), "processing worklist");
    while (worklist.length () > 0)
      {
	const supernode *snode = worklist.pop ();
	gcc_assert (snode);
	process_supernode (*snode, &worklist, map);
    }
  }

  if (map.get_logger ())
    {
      map.log ("%qE in %qD is needed to process:", name, fun.decl);
      /* Log m_snodes_needing_name, sorting it to avoid churn when comparing
	 dumps.  */
      std::set<int> indices;
      auto_vec<const supernode *> snodes;
      for (auto iter : m_snodes_needing_name)
	indices.insert (iter->m_id);
      for (auto iter : indices)
	map.get_logger ()->log ("  SN %i", iter);
    }
}

/* Return true if the SSA name is needed at POINT.  */

bool
state_purge_per_ssa_name::needed_at_supernode_p (const supernode *snode) const
{
  return const_cast <point_set_t &> (m_snodes_needing_name).contains (snode);
}

/* Add SNODE to *WORKLIST if the supernode has not already been seen.
   Subroutine of ctor.  */

void
state_purge_per_ssa_name::add_to_worklist (const supernode &snode,
					   auto_vec<const supernode *> *worklist,
					   logger *logger)
{
  LOG_FUNC (logger);

  gcc_assert (snode.get_function () == &get_function ());

  if (m_snodes_needing_name.contains (&snode))
    {
      if (logger)
	logger->log ("SN %i already seen for %qE", snode.m_id, m_name);
    }
  else
    {
      if (logger)
	logger->log ("not seen; adding SN %i to worklist for %qE",
		     snode.m_id, m_name);
      m_snodes_needing_name.add (&snode);
      worklist->safe_push (&snode);
    }
}

/* Process SNODE, popped from WORKLIST.
   Iterate over predecessors of SNODE, adding to WORKLIST.  */

void
state_purge_per_ssa_name::process_supernode (const supernode &snode,
					     auto_vec<const supernode *> *worklist,
					     const state_purge_map &map)
{
  logger *logger = map.get_logger ();
  LOG_FUNC (logger);
  if (logger)
    logger->log ("considering SN %i for %qE", snode.m_id, m_name);

  for (auto in_edge : snode.m_preds)
    {
      if (logger)
	logger->log ("considering edge from SN %i", in_edge->m_src->m_id);
      bool defines_ssa_name = false;
      if (auto op = in_edge->get_op ())
	if (op->defines_ssa_name_p (m_name))
	  defines_ssa_name = true;
      if (defines_ssa_name)
	{
	  if (logger)
	    logger->log ("op defines %qE", m_name);
	}
      else
	add_to_worklist (*in_edge->m_src, worklist, logger);
    }
}

void
state_purge_per_ssa_name::on_duplicated_node (const supernode &old_snode,
					      const supernode &new_snode)
{
  if (m_snodes_needing_name.contains (&old_snode))
    m_snodes_needing_name.add (&new_snode);
}

/* class state_purge_per_decl : public state_purge_per_tree.  */

/* state_purge_per_decl's ctor.  */

state_purge_per_decl::state_purge_per_decl (const state_purge_map &map,
					    tree decl,
					    const function &fun)
: state_purge_per_tree (fun),
  m_decl (decl)
{
  /* The RESULT_DECL is always needed at the end of its function. */
  if (TREE_CODE (decl) == RESULT_DECL)
    {
      supernode *exit_snode = map.get_sg ().get_node_for_function_exit (fun);
      add_needed_at (*exit_snode);
    }
}

/* Mark the value of the decl (or a subvalue within it) as being needed
   at SNODE.  */

void
state_purge_per_decl::add_needed_at (const supernode &snode)
{
  m_snodes_needing_decl.add (&snode);
}

/* Mark that a pointer to the decl (or a region within it) is taken
   at SNODE.  */

void
state_purge_per_decl::add_pointed_to_at (const supernode &snode)
{
  m_snodes_taking_address.add (&snode);
}

/* Process the worklists for this decl:
   (a) walk backwards from snodes where we know the value of the decl
   is needed, marking snodes until we get to a stmt that fully overwrites
   the decl.
   (b) walk forwards from snodes where the address of the decl is taken,
   marking snodes as potentially needing the value of the decl.  */

void
state_purge_per_decl::process_worklists (const state_purge_map &map,
					 region_model_manager *mgr)
{
  logger *logger = map.get_logger ();
  LOG_SCOPE (logger);
  if (logger)
    logger->log ("decl: %qE within %qD", m_decl, get_fndecl ());

  /* Worklist for walking backwards from uses.  */
  {
    auto_vec<const supernode *> worklist;
    point_set_t seen;

    /* Add all uses of the decl to the worklist.  */
    for (auto iter : m_snodes_needing_decl)
      worklist.safe_push (iter);

    region_model model (mgr);
    model.push_frame (get_function (), nullptr, nullptr, nullptr);

    /* Process worklist by walking backwards until we reach a stmt
       that fully overwrites the decl.  */
    {
      log_scope s (logger, "processing worklist");
      while (worklist.length () > 0)
	{
	  const supernode *snode = worklist.pop ();
	  process_supernode_backwards (*snode, &worklist, &seen, map, model);
	}
    }
  }

  /* Worklist for walking forwards from address-taken snodes.  */
  {
    auto_vec<const supernode *> worklist;
    point_set_t seen;

    /* Add all uses of the decl to the worklist.  */
    for (auto iter : m_snodes_taking_address)
      {
	worklist.safe_push (iter);

	/* Add to m_snodes_needing_decl (now that we traversed
	   it above for the backward worklist).  */
	m_snodes_needing_decl.add (iter);
      }

    /* Process worklist by walking forwards. */
    {
      log_scope s (logger, "processing worklist");
      while (worklist.length () > 0)
	{
	  const supernode *snode = worklist.pop ();
	  process_supernode_forwards (*snode, &worklist, &seen, map);
	}
    }
  }
}

/* Add SNODE to *WORKLIST if the point is not already in *seen.
   Add newly seen supernodes to *SEEN and to m_snodes_needing_name.  */

void
state_purge_per_decl::add_to_worklist (const supernode &snode,
				       auto_vec<const supernode *> *worklist,
				       point_set_t *seen,
				       logger *logger)
{
  LOG_FUNC (logger);
  if (logger)
    logger->log ("SN %i for worklist for %qE", snode.m_id, m_decl);

  gcc_assert (snode.get_function () == &get_function ());

  if (seen->contains (&snode))
    {
      if (logger)
	logger->log ("already seen for %qE", m_decl);
    }
  else
    {
      if (logger)
	logger->log ("not seen; adding to worklist for %qE", m_decl);
      m_snodes_needing_decl.add (&snode);
      seen->add (&snode);
      worklist->safe_push (&snode);
    }
}

/* Determine if REG_A and REG_B express writing to exactly the same
   set of bits.
   For example, when "s.field" is the only field of "s", and there's no
   padding, this should return true.  */

static bool
same_binding_p (const region *reg_a, const region *reg_b,
		store_manager *store_mgr)
{
  if (reg_a->get_base_region () != reg_b->get_base_region ())
    return false;
  if (reg_a->empty_p ())
    return false;
  const binding_key *bind_key_a = binding_key::make (store_mgr, reg_a);
  if (reg_b->empty_p ())
    return false;
  const binding_key *bind_key_b = binding_key::make (store_mgr, reg_b);
  return bind_key_a == bind_key_b;
}

/* Return true if STMT fully overwrites DECL.  */

static bool
fully_overwrites_p (const gimple *stmt, tree decl,
		    const region_model &model)
{
  if (tree lhs = gimple_get_lhs (stmt))
    {
      /* Determine if LHS fully overwrites DECL.
	 We can't just check for equality; consider the case of
	 "s.field = EXPR;" where the stmt writes to the only field
	 of "s", and there's no padding.  */
      const region *lhs_reg = model.get_lvalue (lhs, nullptr);
      const region *decl_reg = model.get_lvalue (decl, nullptr);
      if (same_binding_p (lhs_reg, decl_reg,
			  model.get_manager ()->get_store_manager ()))
	return true;
    }
  return false;
}

/* Process SNODE, popped from *WORKLIST.
   Iterate over predecessors of SNODE, adding to *WORKLIST and *SEEN,
   until we get to a stmt that fully overwrites the decl.  */

void
state_purge_per_decl::
process_supernode_backwards (const supernode &snode,
			     auto_vec<const supernode *> *worklist,
			     point_set_t *seen,
			     const state_purge_map &map,
			     const region_model &model)
{
  logger *logger = map.get_logger ();
  LOG_FUNC (logger);
  if (logger)
    logger->log ("considering SN %i for %qE", snode.m_id, m_decl);

  for (auto in_edge : snode.m_preds)
    {
      if (logger)
	logger->log ("considering edge from SN %i", in_edge->m_src->m_id);

      bool fully_overwrites_decl = false;
      if (auto op = in_edge->get_op ())
	{
	  /* This is somewhat equivalent to how the SSA case handles
	     def-stmts.  */
	  if (auto stmt = op->maybe_get_stmt ())
	    if (fully_overwrites_p (stmt, m_decl, model)
	      /* ...but we mustn't be at a point that also consumes the
		 current value of the decl when it's generating the new
		 value, for cases such as
		   struct st s;
		   s = foo ();
		   s = bar (s);
		 where we want to make sure that we don't stop at the:
		   s = bar (s);
		 since otherwise we would erroneously purge the state of "s"
		 after:
		   s = foo ();
	      */
	      && !m_snodes_needing_decl.contains (&snode))
	    fully_overwrites_decl = true;
	}
      if (fully_overwrites_decl)
	{
	  if (logger)
	    logger->log ("op fully overwrites %qE; terminating", m_decl);
	}
      else
	add_to_worklist (*in_edge->m_src, worklist, seen, logger);
    }
}

/* Process SNODE, popped from *WORKLIST.
   Iterate over successors of SNODE, adding to *WORKLIST and *SEEN.  */

void
state_purge_per_decl::
process_supernode_forwards (const supernode &snode,
			    auto_vec<const supernode *> *worklist,
			    point_set_t *seen,
			    const state_purge_map &map)
{
  logger *logger = map.get_logger ();
  LOG_FUNC (logger);
  if (logger)
    logger->log ("considering SN %i for %qE", snode.m_id, m_decl);

  for (auto out_edge : snode.m_succs)
    add_to_worklist (*out_edge->m_dest, worklist, seen, logger);
}

/* Return true if the decl is needed at SNODE.  */

bool
state_purge_per_decl::needed_at_supernode_p (const supernode *snode) const
{
  return const_cast <point_set_t &> (m_snodes_needing_decl).contains (snode);
}

void
state_purge_per_decl::on_duplicated_node (const supernode &old_snode,
					      const supernode &new_snode)
{
  if (m_snodes_needing_decl.contains (&old_snode))
    m_snodes_needing_decl.add (&new_snode);
  if (m_snodes_taking_address.contains (&old_snode))
    m_snodes_taking_address.add (&new_snode);
}
/* class state_purge_annotator : public dot_annotator.  */

/* Implementation of dot_annotator::add_node_annotations vfunc for
   state_purge_annotator.

   Add an additional record showing which names are purged on entry
   to the supernode N.  */

/* Print V to GV as a comma-separated list in braces within a <TR>,
   titling it with TITLE.

   Subroutine of state_purge_annotator::print_needed.  */

static void
print_vec_of_names (graphviz_out *gv, const char *title,
		    const auto_vec<tree> &v)
{
  pretty_printer *pp = gv->get_pp ();
  tree name;
  unsigned i;
  gv->begin_trtd ();
  pp_printf (pp, "%s: {", title);
  FOR_EACH_VEC_ELT (v, i, name)
    {
      if (i > 0)
	pp_string (pp, ", ");
      pp_printf (pp, "%qE", name);
    }
  pp_printf (pp, "}");
  pp_write_text_as_html_like_dot_to_stream (pp);
  gv->end_tdtr ();
  pp_newline (pp);
}

void
state_purge_annotator::add_node_annotations (graphviz_out *gv,
					     const supernode &snode) const
{
  if (m_map == nullptr)
    return;

  auto_vec<tree> needed;
  auto_vec<tree> not_needed;
  for (state_purge_map::ssa_iterator iter = m_map->begin_ssas ();
       iter != m_map->end_ssas ();
       ++iter)
    {
      tree name = (*iter).first;
      state_purge_per_ssa_name *per_name_data = (*iter).second;
      if (&per_name_data->get_function () == snode.get_function ())
	{
	  if (per_name_data->needed_at_supernode_p (&snode))
	    needed.safe_push (name);
	  else
	    not_needed.safe_push (name);
	}
    }
  for (state_purge_map::decl_iterator iter = m_map->begin_decls ();
       iter != m_map->end_decls ();
       ++iter)
    {
      tree decl = (*iter).first;
      state_purge_per_decl *per_decl_data = (*iter).second;
      if (&per_decl_data->get_function () == snode.get_function ())
	{
	  if (per_decl_data->needed_at_supernode_p (&snode))
	    needed.safe_push (decl);
	  else
	    not_needed.safe_push (decl);
	}
    }

  print_vec_of_names (gv, "needed here", needed);
  print_vec_of_names (gv, "not needed here", not_needed);
}

#endif /* #if ENABLE_ANALYZER */
