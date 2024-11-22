/* Classes for purging state at function_points.
   Copyright (C) 2019-2024 Free Software Foundation, Inc.
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

#include "config.h"
#define INCLUDE_VECTOR
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "timevar.h"
#include "tree-ssa-alias.h"
#include "function.h"
#include "basic-block.h"
#include "gimple.h"
#include "stringpool.h"
#include "tree-vrp.h"
#include "gimple-ssa.h"
#include "tree-ssanames.h"
#include "tree-phinodes.h"
#include "options.h"
#include "ssa-iterators.h"
#include "diagnostic-core.h"
#include "gimple-pretty-print.h"
#include "analyzer/analyzer.h"
#include "analyzer/call-string.h"
#include "analyzer/supergraph.h"
#include "analyzer/program-point.h"
#include "analyzer/analyzer-logging.h"
#include "analyzer/state-purge.h"
#include "analyzer/store.h"
#include "analyzer/region-model.h"
#include "gimple-walk.h"
#include "cgraph.h"

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
   function_point, for populating the worklists within a state_purge_map.  */

class gimple_op_visitor : public log_user
{
public:
  gimple_op_visitor (state_purge_map *map,
		     const function_point &point,
		     const function &fun)
  : log_user (map->get_logger ()),
    m_map (map),
    m_point (point),
    m_fun (fun)
  {}

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
    data.add_needed_at (m_point);

    /* Handle calls: if we're seeing a use at a call, then add a use at the
       "after-supernode" point (in case of interprocedural call superedges).  */
    if (m_point.final_stmt_p ())
      data.add_needed_at (m_point.get_next ());
  }

  void add_pointed_to (tree decl)
  {
    gcc_assert (get_candidate_for_purging (decl) == decl);
    get_or_create_data_for_decl (decl).add_pointed_to_at (m_point);
  }

  state_purge_per_decl &
  get_or_create_data_for_decl (tree decl)
  {
    return m_map->get_or_create_data_for_decl (m_fun, decl);
  }

  state_purge_map *m_map;
  const function_point &m_point;
  const function &m_fun;
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
     We iterate through all function points, finding loads, stores, and
     address-taken operations on locals, building a pair of worklists.  */
  for (auto snode : sg.m_nodes)
    {
      if (logger)
	log ("SN: %i", snode->m_index);
      /* We ignore m_returning_call and phi nodes.  */
      gimple *stmt;
      unsigned i;
      FOR_EACH_VEC_ELT (snode->m_stmts, i, stmt)
	{
	  function *fun = snode->get_function ();
	  gcc_assert (fun);
	  function_point point (function_point::before_stmt (snode, i));
	  gimple_op_visitor v (this, point, *fun);
	  walk_stmt_load_store_addr_ops (stmt, &v,
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
: state_purge_per_tree (fun), m_points_needing_name (), m_name (name)
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

  auto_vec<function_point> worklist;

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

	  const supernode *snode
	    = map.get_sg ().get_supernode_for_stmt (use_stmt);

	  /* If it's a use within a phi node, then we care about
	     which in-edge we came from.  */
	  if (use_stmt->code == GIMPLE_PHI)
	    {
	      for (gphi_iterator gpi
		     = const_cast<supernode *> (snode)->start_phis ();
		   !gsi_end_p (gpi); gsi_next (&gpi))
		{
		  gphi *phi = gpi.phi ();
		  if (phi == use_stmt)
		    {
		      /* Find arguments (and thus in-edges) which use NAME.  */
		      for (unsigned arg_idx = 0;
			   arg_idx < gimple_phi_num_args (phi);
			   ++arg_idx)
			{
			  if (name == gimple_phi_arg (phi, arg_idx)->def)
			    {
			      edge in_edge = gimple_phi_arg_edge (phi, arg_idx);
			      const superedge *in_sedge
				= map.get_sg ().get_edge_for_cfg_edge (in_edge);
			      function_point point
				= function_point::before_supernode
				(snode, in_sedge);
			      add_to_worklist (point, &worklist,
					       map.get_logger ());
			      m_points_needing_name.add (point);
			    }
			}
		    }
		}
	    }
	  else
	    {
	      function_point point = before_use_stmt (map, use_stmt);
	      add_to_worklist (point, &worklist, map.get_logger ());
	      m_points_needing_name.add (point);

	      /* We also need to add uses for conditionals and switches,
		 where the stmt "happens" at the after_supernode, for filtering
		 the out-edges.  */
	      if (use_stmt == snode->get_last_stmt ())
		{
		  if (map.get_logger ())
		    map.log ("last stmt in BB");
		  function_point point
		    = function_point::after_supernode (snode);
		  add_to_worklist (point, &worklist, map.get_logger ());
		  m_points_needing_name.add (point);
		}
	      else
		if (map.get_logger ())
		  map.log ("not last stmt in BB");
	    }
	}
    }

  /* Process worklist by walking backwards until we reach the def stmt.  */
  {
    log_scope s (map.get_logger (), "processing worklist");
    while (worklist.length () > 0)
      {
	function_point point = worklist.pop ();
	process_point (point, &worklist, map);
    }
  }

  if (map.get_logger ())
    {
      map.log ("%qE in %qD is needed to process:", name, fun.decl);
      /* Log m_points_needing_name, sorting it to avoid churn when comparing
	 dumps.  */
      auto_vec<function_point> points;
      for (point_set_t::iterator iter = m_points_needing_name.begin ();
	   iter != m_points_needing_name.end ();
	   ++iter)
	points.safe_push (*iter);
      points.qsort (function_point::cmp_ptr);
      unsigned i;
      function_point *point;
      FOR_EACH_VEC_ELT (points, i, point)
	{
	  map.start_log_line ();
	  map.get_logger ()->log_partial ("  point: ");
	  point->print (map.get_logger ()->get_printer (), format (false));
	  map.end_log_line ();
	}
    }
}

/* Return true if the SSA name is needed at POINT.  */

bool
state_purge_per_ssa_name::needed_at_point_p (const function_point &point) const
{
  return const_cast <point_set_t &> (m_points_needing_name).contains (point);
}

/* Get the function_point representing immediately before USE_STMT.
   Subroutine of ctor.  */

function_point
state_purge_per_ssa_name::before_use_stmt (const state_purge_map &map,
					   const gimple *use_stmt)
{
  gcc_assert (use_stmt->code != GIMPLE_PHI);

  const supernode *supernode
    = map.get_sg ().get_supernode_for_stmt (use_stmt);
  unsigned int stmt_idx = supernode->get_stmt_index (use_stmt);
  return function_point::before_stmt (supernode, stmt_idx);
}

/* Add POINT to *WORKLIST if the point has not already been seen.
   Subroutine of ctor.  */

void
state_purge_per_ssa_name::add_to_worklist (const function_point &point,
					   auto_vec<function_point> *worklist,
					   logger *logger)
{
  LOG_FUNC (logger);
  if (logger)
    {
      logger->start_log_line ();
      logger->log_partial ("point: '");
      point.print (logger->get_printer (), format (false));
      logger->log_partial ("' for worklist for %qE", m_name);
      logger->end_log_line ();
    }

  gcc_assert (point.get_function () == &get_function ());
  if (point.get_from_edge ())
    gcc_assert (point.get_from_edge ()->get_kind () == SUPEREDGE_CFG_EDGE);

  if (m_points_needing_name.contains (point))
    {
      if (logger)
	logger->log ("already seen for %qE", m_name);
    }
  else
    {
      if (logger)
	logger->log ("not seen; adding to worklist for %qE", m_name);
      m_points_needing_name.add (point);
      worklist->safe_push (point);
    }
}

/* Return true iff NAME is used by any of the phi nodes in SNODE
   when processing the in-edge with PHI_ARG_IDX.  */

static bool
name_used_by_phis_p (tree name, const supernode *snode,
		     size_t phi_arg_idx)
{
  gcc_assert (TREE_CODE (name) == SSA_NAME);

  for (gphi_iterator gpi
	 = const_cast<supernode *> (snode)->start_phis ();
       !gsi_end_p (gpi); gsi_next (&gpi))
    {
      gphi *phi = gpi.phi ();
      if (gimple_phi_arg_def (phi, phi_arg_idx) == name)
	return true;
    }
  return false;
}

/* Process POINT, popped from WORKLIST.
   Iterate over predecessors of POINT, adding to WORKLIST.  */

void
state_purge_per_ssa_name::process_point (const function_point &point,
					 auto_vec<function_point> *worklist,
					 const state_purge_map &map)
{
  logger *logger = map.get_logger ();
  LOG_FUNC (logger);
  if (logger)
    {
      logger->start_log_line ();
      logger->log_partial ("considering point: '");
      point.print (logger->get_printer (), format (false));
      logger->log_partial ("' for %qE", m_name);
      logger->end_log_line ();
    }

  gimple *def_stmt = SSA_NAME_DEF_STMT (m_name);

  const supernode *snode = point.get_supernode ();

  switch (point.get_kind ())
    {
    default:
      gcc_unreachable ();

    case PK_ORIGIN:
      break;

    case PK_BEFORE_SUPERNODE:
      {
	for (gphi_iterator gpi
	       = const_cast<supernode *> (snode)->start_phis ();
	     !gsi_end_p (gpi); gsi_next (&gpi))
	  {
	    gcc_assert (point.get_from_edge ());
	    const cfg_superedge *cfg_sedge
	      = point.get_from_edge ()->dyn_cast_cfg_superedge ();
	    gcc_assert (cfg_sedge);

	    gphi *phi = gpi.phi ();
	    /* Are we at the def-stmt for m_name?  */
	    if (phi == def_stmt)
	      {
		if (name_used_by_phis_p (m_name, snode,
					 cfg_sedge->get_phi_arg_idx ()))
		  {
		    if (logger)
		      logger->log ("name in def stmt used within phis;"
				   " continuing");
		  }
		else
		  {
		    if (logger)
		      logger->log ("name in def stmt not used within phis;"
				   " terminating");
		    return;
		  }
	      }
	  }

	/* Add given pred to worklist.  */
	if (point.get_from_edge ())
	  {
	    gcc_assert (point.get_from_edge ()->m_src);
	    add_to_worklist
	      (function_point::after_supernode (point.get_from_edge ()->m_src),
	       worklist, logger);
	  }
	else
	  {
	    /* Add any intraprocedually edge for a call.  */
	    if (snode->m_returning_call)
	    {
	      gcall *returning_call = snode->m_returning_call;
	      cgraph_edge *cedge
		  = supergraph_call_edge (snode->m_fun,
					  returning_call);
	      if(cedge)
	        {
		  superedge *sedge
		    = map.get_sg ().get_intraprocedural_edge_for_call (cedge);
		  gcc_assert (sedge);
		  add_to_worklist
		    (function_point::after_supernode (sedge->m_src),
		     worklist, logger);
	        }
	      else
	        {
	          supernode *callernode
	            = map.get_sg ().get_supernode_for_stmt (returning_call);

	          gcc_assert (callernode);
	          add_to_worklist
	            (function_point::after_supernode (callernode),
		     worklist, logger);
	         }
	    }
	  }
      }
      break;

    case PK_BEFORE_STMT:
      {
	if (def_stmt == point.get_stmt ())
	  {
	    if (logger)
	      logger->log ("def stmt; terminating");
	    return;
	  }
	if (point.get_stmt_idx () > 0)
	  add_to_worklist (function_point::before_stmt
			     (snode, point.get_stmt_idx () - 1),
			   worklist, logger);
	else
	{
	  /* Add before_supernode to worklist.  This captures the in-edge,
	     so we have to do it once per in-edge.  */
	  unsigned i;
	  superedge *pred;
	  FOR_EACH_VEC_ELT (snode->m_preds, i, pred)
	    add_to_worklist (function_point::before_supernode (snode,
							       pred),
			     worklist, logger);
	}
      }
      break;

    case PK_AFTER_SUPERNODE:
      {
	if (snode->m_stmts.length ())
	  add_to_worklist
	    (function_point::before_stmt (snode,
					  snode->m_stmts.length () - 1),
	     worklist, logger);
	else
	  {
	    /* Add before_supernode to worklist.  This captures the in-edge,
	       so we have to do it once per in-edge.  */
	    unsigned i;
	    superedge *pred;
	    FOR_EACH_VEC_ELT (snode->m_preds, i, pred)
	      add_to_worklist (function_point::before_supernode (snode,
								 pred),
			       worklist, logger);
	    /* If it's the initial BB, add it, to ensure that we
	       have "before supernode" for the initial ENTRY block, and don't
	       erroneously purge SSA names for initial values of parameters.  */
	    if (snode->entry_p ())
	      {
		add_to_worklist
		  (function_point::before_supernode (snode, NULL),
		   worklist, logger);
	      }
	  }
      }
      break;
    }
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
      add_needed_at (function_point::after_supernode (exit_snode));
    }
}

/* Mark the value of the decl (or a subvalue within it) as being needed
   at POINT.  */

void
state_purge_per_decl::add_needed_at (const function_point &point)
{
  m_points_needing_decl.add (point);
}

/* Mark that a pointer to the decl (or a region within it) is taken
   at POINT.  */

void
state_purge_per_decl::add_pointed_to_at (const function_point &point)
{
  m_points_taking_address.add (point);
}

/* Process the worklists for this decl:
   (a) walk backwards from points where we know the value of the decl
   is needed, marking points until we get to a stmt that fully overwrites
   the decl.
   (b) walk forwards from points where the address of the decl is taken,
   marking points as potentially needing the value of the decl.  */

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
    auto_vec<function_point> worklist;
    point_set_t seen;

    /* Add all uses of the decl to the worklist.  */
    for (auto iter : m_points_needing_decl)
      worklist.safe_push (iter);

    region_model model (mgr);
    model.push_frame (get_function (), NULL, NULL);

    /* Process worklist by walking backwards until we reach a stmt
       that fully overwrites the decl.  */
    {
      log_scope s (logger, "processing worklist");
      while (worklist.length () > 0)
	{
	  function_point point = worklist.pop ();
	  process_point_backwards (point, &worklist, &seen, map, model);
	}
    }
  }

  /* Worklist for walking forwards from address-taken points.  */
  {
    auto_vec<function_point> worklist;
    point_set_t seen;

    /* Add all uses of the decl to the worklist.  */
    for (auto iter : m_points_taking_address)
      {
	worklist.safe_push (iter);

	/* Add to m_points_needing_decl (now that we traversed
	   it above for the backward worklist).  */
	m_points_needing_decl.add (iter);
      }

    /* Process worklist by walking forwards. */
    {
      log_scope s (logger, "processing worklist");
      while (worklist.length () > 0)
	{
	  function_point point = worklist.pop ();
	  process_point_forwards (point, &worklist, &seen, map);
	}
    }
  }
}

/* Add POINT to *WORKLIST if the point is not already in *seen.
   Add newly seen points to *SEEN and to m_points_needing_name.  */

void
state_purge_per_decl::add_to_worklist (const function_point &point,
				       auto_vec<function_point> *worklist,
				       point_set_t *seen,
				       logger *logger)
{
  LOG_FUNC (logger);
  if (logger)
    {
      logger->start_log_line ();
      logger->log_partial ("point: '");
      point.print (logger->get_printer (), format (false));
      logger->log_partial ("' for worklist for %qE", m_decl);
      logger->end_log_line ();
    }

  gcc_assert (point.get_function () == &get_function ());
  if (point.get_from_edge ())
    gcc_assert (point.get_from_edge ()->get_kind () == SUPEREDGE_CFG_EDGE);

  if (seen->contains (point))
    {
      if (logger)
	logger->log ("already seen for %qE", m_decl);
    }
  else
    {
      if (logger)
	logger->log ("not seen; adding to worklist for %qE", m_decl);
      m_points_needing_decl.add (point);
      seen->add (point);
      worklist->safe_push (point);
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
      const region *lhs_reg = model.get_lvalue (lhs, NULL);
      const region *decl_reg = model.get_lvalue (decl, NULL);
      if (same_binding_p (lhs_reg, decl_reg,
			  model.get_manager ()->get_store_manager ()))
	return true;
    }
  return false;
}

/* Process POINT, popped from *WORKLIST.
   Iterate over predecessors of POINT, adding to *WORKLIST and *SEEN,
   until we get to a stmt that fully overwrites the decl.  */

void
state_purge_per_decl::
process_point_backwards (const function_point &point,
			 auto_vec<function_point> *worklist,
			 point_set_t *seen,
			 const state_purge_map &map,
			 const region_model &model)
{
  logger *logger = map.get_logger ();
  LOG_FUNC (logger);
  if (logger)
    {
      logger->start_log_line ();
      logger->log_partial ("considering point: '");
      point.print (logger->get_printer (), format (false));
      logger->log_partial ("' for %qE", m_decl);
      logger->end_log_line ();
    }
  const supernode *snode = point.get_supernode ();

  switch (point.get_kind ())
    {
    default:
      gcc_unreachable ();

    case PK_ORIGIN:
      break;

    case PK_BEFORE_SUPERNODE:
      {
	/* Add given pred to worklist.  */
	if (point.get_from_edge ())
	  {
	    gcc_assert (point.get_from_edge ()->m_src);
	    add_to_worklist
	      (function_point::after_supernode (point.get_from_edge ()->m_src),
	       worklist, seen, logger);
	  }
	else
	  {
	    /* Add any intraprocedually edge for a call.  */
	    if (snode->m_returning_call)
	    {
	      gcall *returning_call = snode->m_returning_call;
	      cgraph_edge *cedge
		  = supergraph_call_edge (snode->m_fun,
					  returning_call);
	      if(cedge)
		{
		  superedge *sedge
		    = map.get_sg ().get_intraprocedural_edge_for_call (cedge);
		  gcc_assert (sedge);
		  add_to_worklist
		    (function_point::after_supernode (sedge->m_src),
		     worklist, seen, logger);
		}
	      else
		{
		  supernode *callernode
		    = map.get_sg ().get_supernode_for_stmt (returning_call);

		  gcc_assert (callernode);
		  add_to_worklist
		    (function_point::after_supernode (callernode),
		     worklist, seen, logger);
		}
	    }
	  }
      }
      break;

    case PK_BEFORE_STMT:
      {
	/* This is somewhat equivalent to how the SSA case handles
	   def-stmts.  */
	if (fully_overwrites_p (point.get_stmt (), m_decl, model)
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
	    && !m_points_needing_decl.contains (point))
	  {
	    if (logger)
	      logger->log ("stmt fully overwrites %qE; terminating", m_decl);
	    return;
	  }
	if (point.get_stmt_idx () > 0)
	  add_to_worklist (function_point::before_stmt
			     (snode, point.get_stmt_idx () - 1),
			   worklist, seen, logger);
	else
	{
	  /* Add before_supernode to worklist.  This captures the in-edge,
	     so we have to do it once per in-edge.  */
	  unsigned i;
	  superedge *pred;
	  FOR_EACH_VEC_ELT (snode->m_preds, i, pred)
	    add_to_worklist (function_point::before_supernode (snode,
							       pred),
			     worklist, seen, logger);
	}
      }
      break;

    case PK_AFTER_SUPERNODE:
      {
	if (snode->m_stmts.length ())
	  add_to_worklist
	    (function_point::before_stmt (snode,
					  snode->m_stmts.length () - 1),
	     worklist, seen, logger);
	else
	  {
	    /* Add before_supernode to worklist.  This captures the in-edge,
	       so we have to do it once per in-edge.  */
	    unsigned i;
	    superedge *pred;
	    FOR_EACH_VEC_ELT (snode->m_preds, i, pred)
	      add_to_worklist (function_point::before_supernode (snode,
								 pred),
			       worklist, seen, logger);
	  }
      }
      break;
    }

}

/* Process POINT, popped from *WORKLIST.
   Iterate over successors of POINT, adding to *WORKLIST and *SEEN.  */

void
state_purge_per_decl::
process_point_forwards (const function_point &point,
			auto_vec<function_point> *worklist,
			point_set_t *seen,
			const state_purge_map &map)
{
  logger *logger = map.get_logger ();
  LOG_FUNC (logger);
  if (logger)
    {
      logger->start_log_line ();
      logger->log_partial ("considering point: '");
      point.print (logger->get_printer (), format (false));
      logger->log_partial ("' for %qE", m_decl);
      logger->end_log_line ();
    }
  const supernode *snode = point.get_supernode ();

  switch (point.get_kind ())
    {
    default:
    case PK_ORIGIN:
      gcc_unreachable ();

    case PK_BEFORE_SUPERNODE:
      {
	function_point next = point.get_next ();
	add_to_worklist (next, worklist, seen, logger);
      }
      break;

    case PK_BEFORE_STMT:
      {
	/* Perhaps we should stop at a clobber of the decl,
	   but that ought to purge state for us explicitly.  */
	function_point next = point.get_next ();
	add_to_worklist (next, worklist, seen, logger);
      }
      break;

    case PK_AFTER_SUPERNODE:
      {
	/* Look at interprocedural out-edges.  */
	unsigned i;
	superedge *succ;
	FOR_EACH_VEC_ELT (snode->m_succs, i, succ)
	  {
	    enum edge_kind kind = succ->get_kind ();
	    if (kind == SUPEREDGE_CFG_EDGE
		|| kind == SUPEREDGE_INTRAPROCEDURAL_CALL)
	      add_to_worklist (function_point::before_supernode (succ->m_dest,
								 succ),
			       worklist, seen, logger);
	  }
      }
      break;
    }
}

/* Return true if the decl is needed at POINT.  */

bool
state_purge_per_decl::needed_at_point_p (const function_point &point) const
{
  return const_cast <point_set_t &> (m_points_needing_decl).contains (point);
}

/* class state_purge_annotator : public dot_annotator.  */

/* Implementation of dot_annotator::add_node_annotations vfunc for
   state_purge_annotator.

   Add an additional record showing which names are purged on entry
   to the supernode N.  */

bool
state_purge_annotator::add_node_annotations (graphviz_out *gv,
					     const supernode &n,
					     bool within_table) const
{
  if (m_map == NULL)
    return false;

  if (within_table)
    return false;

  pretty_printer *pp = gv->get_pp ();

   pp_printf (pp, "annotation_for_node_%i", n.m_index);
   pp_printf (pp, " [shape=none,margin=0,style=filled,fillcolor=%s,label=\"",
	      "lightblue");
   pp_write_text_to_stream (pp);

   /* Different in-edges mean different names need purging.
      Determine which points to dump.  */
   auto_vec<function_point> points;
   if (n.entry_p () || n.m_returning_call)
     points.safe_push (function_point::before_supernode (&n, NULL));
   else
     for (auto inedge : n.m_preds)
       points.safe_push (function_point::before_supernode (&n, inedge));
   points.safe_push (function_point::after_supernode (&n));

   for (auto & point : points)
     {
       point.print (pp, format (true));
       pp_newline (pp);
       print_needed (gv, point, false);
       pp_newline (pp);
     }

   pp_string (pp, "\"];\n\n");
   pp_flush (pp);
   return false;
}

/* Print V to GV as a comma-separated list in braces, titling it with TITLE.
   If WITHIN_TABLE is true, print it within a <TR>

   Subroutine of state_purge_annotator::print_needed.  */

static void
print_vec_of_names (graphviz_out *gv, const char *title,
		    const auto_vec<tree> &v, bool within_table)
{
  pretty_printer *pp = gv->get_pp ();
  tree name;
  unsigned i;
  if (within_table)
    gv->begin_trtd ();
  pp_printf (pp, "%s: {", title);
  FOR_EACH_VEC_ELT (v, i, name)
    {
      if (i > 0)
	pp_string (pp, ", ");
      pp_printf (pp, "%qE", name);
    }
  pp_printf (pp, "}");
  if (within_table)
    {
      pp_write_text_as_html_like_dot_to_stream (pp);
      gv->end_tdtr ();
    }
  pp_newline (pp);
}

/* Implementation of dot_annotator::add_stmt_annotations for
   state_purge_annotator.

   Add text showing which names are purged at STMT.  */

void
state_purge_annotator::add_stmt_annotations (graphviz_out *gv,
					     const gimple *stmt,
					     bool within_row) const
{
  if (within_row)
    return;

  if (m_map == NULL)
    return;

  if (stmt->code == GIMPLE_PHI)
    return;

  pretty_printer *pp = gv->get_pp ();

  pp_newline (pp);

  const supernode *supernode = m_map->get_sg ().get_supernode_for_stmt (stmt);
  unsigned int stmt_idx = supernode->get_stmt_index (stmt);
  function_point before_stmt
    (function_point::before_stmt (supernode, stmt_idx));

  print_needed (gv, before_stmt, true);
}

/* Get the decls and SSA names needed and not-needed at POINT, and
   print them to GV.
   If WITHIN_TABLE is true, print them within <TR> elements.  */

void
state_purge_annotator::print_needed (graphviz_out *gv,
				     const function_point &point,
				     bool within_table) const
{
  auto_vec<tree> needed;
  auto_vec<tree> not_needed;
  for (state_purge_map::ssa_iterator iter = m_map->begin_ssas ();
       iter != m_map->end_ssas ();
       ++iter)
    {
      tree name = (*iter).first;
      state_purge_per_ssa_name *per_name_data = (*iter).second;
      if (&per_name_data->get_function () == point.get_function ())
	{
	  if (per_name_data->needed_at_point_p (point))
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
      if (&per_decl_data->get_function () == point.get_function ())
	{
	  if (per_decl_data->needed_at_point_p (point))
	    needed.safe_push (decl);
	  else
	    not_needed.safe_push (decl);
	}
    }

  print_vec_of_names (gv, "needed here", needed, within_table);
  print_vec_of_names (gv, "not needed here", not_needed, within_table);
}

#endif /* #if ENABLE_ANALYZER */
