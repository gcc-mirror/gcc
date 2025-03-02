/* Strongly-connected copy propagation pass for the GNU compiler.
   Copyright (C) 2023-2025 Free Software Foundation, Inc.
   Contributed by Filip Kastl <fkastl@suse.cz>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define INCLUDE_ALGORITHM
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-iterator.h"
#include "vec.h"
#include "hash-set.h"
#include "ssa-iterators.h"
#include "gimple-fold.h"
#include "gimplify.h"
#include "tree-cfg.h"
#include "tree-eh.h"
#include "builtins.h"
#include "tree-ssa-dce.h"
#include "fold-const.h"

/* Strongly connected copy propagation pass.

   This is a lightweight copy propagation pass that is also able to eliminate
   redundant PHI statements.  The pass considers the following types of copy
   statements:

   1 An assignment statement with a single argument.

   _3 = _2;
   _4 = 5;

   2 A degenerate PHI statement.  A degenerate PHI is a PHI that only refers to
     itself or one other value.

   _5 = PHI <_1>;
   _6 = PHI <_6, _6, _1, _1>;
   _7 = PHI <16, _7>;

   3 A set of PHI statements that only refer to each other or to one other
     value.

   _8 = PHI <_9, _10>;
   _9 = PHI <_8, _10>;
   _10 = PHI <_8, _9, _1>;

   All of these statements produce copies and can be eliminated from the
   program.  For a copy statement we identify the value it creates a copy of
   and replace references to the statement with the value -- we propagate the
   copy.

   _3 = _2; // Replace all occurences of _3 by _2

   _8 = PHI <_9, _10>;
   _9 = PHI <_8, _10>;
   _10 = PHI <_8, _9, _1>; // Replace all occurences of _8, _9 and _10 by _1

   To find all three types of copy statements we use an algorithm based on
   strongly-connected components (SCCs) in dataflow graph.  The algorithm was
   introduced in an article from 2013[1]. We describe the algorithm bellow.

   To identify SCCs we implement the Robert Tarjan's SCC algorithm.  For the
   SCC computation we wrap potential copy statements in the 'vertex' struct.
   To each of these statements we also assign a vertex number ('vxnum'). Since
   the main algorithm has to be able to compute SCCs of subgraphs of the whole
   dataflow graph we use GIMPLE stmt flags to prevent Tarjan's algorithm from
   leaving the subgraph.

   References:

     [1] Simple and Efficient Construction of Static Single Assignmemnt Form,
     Braun, Buchwald, Hack, Leissa, Mallon, Zwinkau, 2013, LNCS vol. 7791,
     Section 3.2.  */

namespace {

/* State of vertex during SCC discovery.

   unvisited  Vertex hasn't yet been popped from worklist.
   vopen      DFS has visited vertex for the first time.  Vertex has been put
	      on Tarjan stack.
   closed     DFS has backtracked through vertex.  At this point, vertex
	      doesn't have any unvisited neighbors.
   in_scc     Vertex has been popped from Tarjan stack.  */

enum vstate
{
  unvisited,
  vopen,
  closed,
  in_scc
};

/* Information about a vertex.  Used by SCC discovery.  */

struct vertex
{
  bool active; /* scc_discovery::compute_sccs () only considers a subgraph of
		  the whole dataflow graph.  It uses this flag so that it knows
		  which vertices are part of this subgraph.  */
  vstate state;
  unsigned index;
  unsigned lowlink;
};

/* SCC discovery.

   Used to find SCCs in a dataflow graph.  Implements Tarjan's SCC
   algorithm.  */

class scc_discovery
{
public:
  scc_discovery ();
  ~scc_discovery ();
  auto_vec<vec<gimple *>> compute_sccs (vec<gimple *> &stmts);

private:
  vertex* vertices; /* Indexed by SSA_NAME_VERSION.  */
  auto_vec<unsigned> worklist; /* DFS stack.  */
  auto_vec<unsigned> stack; /* Tarjan stack.  */

  void visit_neighbor (tree neigh_tree, unsigned parent_vxnum);
};

scc_discovery::scc_discovery ()
{
  /* Create vertex struct for each SSA name.  */
  vertices = XNEWVEC (struct vertex, num_ssa_names);
  unsigned i = 0;
  for (i = 0; i < num_ssa_names; i++)
    vertices[i].active = false;
}

scc_discovery::~scc_discovery ()
{
  XDELETEVEC (vertices);
}

/* Part of 'scc_discovery::compute_sccs ()'.  */

void
scc_discovery::visit_neighbor (tree neigh_tree, unsigned parent_version)
{
  if (TREE_CODE (neigh_tree) != SSA_NAME)
    return; /* Skip any neighbor that isn't an SSA name.  */
  unsigned neigh_version = SSA_NAME_VERSION (neigh_tree);

  /* Skip neighbors outside the subgraph that Tarjan currently works
     with.  */
  if (!vertices[neigh_version].active)
    return;

  vstate neigh_state = vertices[neigh_version].state;
  vstate parent_state = vertices[parent_version].state;
  if (parent_state == vopen) /* We're currently opening parent.  */
    {
      /* Put unvisited neighbors on worklist.  Update lowlink of parent
	 vertex according to indices of neighbors present on stack.  */
      switch (neigh_state)
	{
	case unvisited:
	  worklist.safe_push (neigh_version);
	  break;
	case vopen:
	case closed:
	  vertices[parent_version].lowlink
	    = std::min (vertices[parent_version].lowlink,
			vertices[neigh_version].index);
	  break;
	case in_scc:
	  /* Ignore these edges.  */
	  break;
	}
    }
  else if (parent_state == closed) /* We're currently closing parent.  */
    {
      /* Update lowlink of parent vertex according to lowlinks of
	 children of parent (in terms of DFS tree).  */
      if (neigh_state == closed)
	{
	  vertices[parent_version].lowlink
	    = std::min (vertices[parent_version].lowlink,
			vertices[neigh_version].lowlink);
	}
    }
}

/* Compute SCCs in dataflow graph on given statements 'stmts'.  Ignore
   statements outside 'stmts'.  Return the SCCs in a reverse topological
   order.

   stmt_may_generate_copy () must be true for all statements from 'stmts'!  */

auto_vec<vec<gimple *>>
scc_discovery::compute_sccs (vec<gimple *> &stmts)
{
  auto_vec<vec<gimple *>> sccs;

  for (gimple *stmt : stmts)
    {
      unsigned i;
      switch (gimple_code (stmt))
	{
	  case GIMPLE_ASSIGN:
	    i = SSA_NAME_VERSION (gimple_assign_lhs (stmt));
	    break;
	  case GIMPLE_PHI:
	    i = SSA_NAME_VERSION (gimple_phi_result (stmt));
	    break;
	  default:
	    gcc_unreachable ();
	}

      vertices[i].index = 0;
      vertices[i].lowlink = 0;
      vertices[i].state = unvisited;
      vertices[i].active = true; /* Mark the subgraph we'll be working on so
				    that we don't leave it.  */

      worklist.safe_push (i);
    }

  /* Worklist loop.  */
  unsigned curr_index = 0;
  while (!worklist.is_empty ())
    {
      unsigned i = worklist.pop ();
      gimple *stmt = SSA_NAME_DEF_STMT (ssa_name (i));
      vstate state = vertices[i].state;

      if (state == unvisited)
	{
	  vertices[i].state = vopen;

	  /* Assign index to this vertex.  */
	  vertices[i].index = curr_index;
	  vertices[i].lowlink = curr_index;
	  curr_index++;

	  /* Put vertex on stack and also on worklist to be closed later.  */
	  stack.safe_push (i);
	  worklist.safe_push (i);
	}
      else if (state == vopen)
	vertices[i].state = closed;

      /* Visit neighbors of this vertex.  */
      tree op;
      gphi *phi;
      switch (gimple_code (stmt))
	{
	  case GIMPLE_PHI:
	    phi = as_a <gphi *> (stmt);
	    unsigned j;
	    for (j = 0; j < gimple_phi_num_args (phi); j++)
	      {
		op = gimple_phi_arg_def (phi, j);
		visit_neighbor (op, i);
	      }
	    break;
	  case GIMPLE_ASSIGN:
	    op = gimple_assign_rhs1 (stmt);
	    visit_neighbor (op, i);
	    break;
	  default:
	    gcc_unreachable ();
	}

      /* If we've just closed a root vertex of an scc, pop scc from stack.  */
      if (state == vopen && vertices[i].lowlink == vertices[i].index)
	{
	  vec<gimple *> scc = vNULL;

	  unsigned j;
	  do
	    {
	      j = stack.pop ();
	      scc.safe_push (SSA_NAME_DEF_STMT (ssa_name (j)));
	      vertices[j].state = in_scc;
	    }
	  while (j != i);

	  sccs.safe_push (scc);
	}
    }

  if (!stack.is_empty ())
    gcc_unreachable ();

  /* Clear 'active' flags.  */
  for (gimple *stmt : stmts)
    {
      unsigned i;
      switch (gimple_code (stmt))
	{
	  case GIMPLE_ASSIGN:
	    i = SSA_NAME_VERSION (gimple_assign_lhs (stmt));
	    break;
	  case GIMPLE_PHI:
	    i = SSA_NAME_VERSION (gimple_phi_result (stmt));
	    break;
	  default:
	    gcc_unreachable ();
	}

      vertices[i].active = false;
    }

  return sccs;
}

} // anon namespace

/* Could this statement potentially be a copy statement?

   This pass only considers statements for which this function returns 'true'.
   Those are basically PHI functions and assignment statements similar to

   _2 = _1;
   or
   _2 = 5;  */

static bool
stmt_may_generate_copy (gimple *stmt)
{
  /* A PHI may generate a copy.  */
  if (gimple_code (stmt) == GIMPLE_PHI)
    {
      gphi *phi = as_a <gphi *> (stmt);

      /* No OCCURS_IN_ABNORMAL_PHI SSA names in lhs nor rhs.  */
      if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (gimple_phi_result (phi)))
	return false;

      unsigned i;
      for (i = 0; i < gimple_phi_num_args (phi); i++)
	{
	  tree op = gimple_phi_arg_def (phi, i);
	  if (TREE_CODE (op) == SSA_NAME
	      && SSA_NAME_OCCURS_IN_ABNORMAL_PHI (op))
	    return false;
	}

      /* If PHI has more than one unique non-SSA arguments, it won't generate a
	 copy.  */
      tree const_op = NULL_TREE;
      for (i = 0; i < gimple_phi_num_args (phi); i++)
	{
	  tree op = gimple_phi_arg_def (phi, i);
	  if (TREE_CODE (op) != SSA_NAME)
	    {
	      if (const_op && !operand_equal_p (op, const_op))
		return false;
	      const_op = op;
	    }
	}

      return true;
    }

  /* Or a statement of type _2 = _1; OR _2 = 5; may generate a copy.  */

  if (!gimple_assign_single_p (stmt))
    return false;

  tree lhs = gimple_assign_lhs (stmt);
  tree rhs = gimple_assign_rhs1 (stmt);

  if (TREE_CODE (lhs) != SSA_NAME)
    return false;

  /* lhs shouldn't flow through any abnormal edges.  */
  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (lhs))
    return false;

  if (is_gimple_min_invariant (rhs))
    return true;  /* A statement of type _2 = 5;.  */

  if (TREE_CODE (rhs) != SSA_NAME)
    return false;

  /* rhs shouldn't flow through any abnormal edges.  */
  if (SSA_NAME_OCCURS_IN_ABNORMAL_PHI (rhs))
    return false;

  /* It is possible that lhs has more alignment or value range information.  By
     propagating we would lose this information.  So in the case that alignment
     or value range information differs, we are conservative and do not
     propagate.

     FIXME: Propagate alignment and value range info the same way copy-prop
     does.  */
  if (POINTER_TYPE_P (TREE_TYPE (lhs))
      && POINTER_TYPE_P (TREE_TYPE (rhs))
      && SSA_NAME_PTR_INFO (lhs) != SSA_NAME_PTR_INFO (rhs))
    return false;
  if (!POINTER_TYPE_P (TREE_TYPE (lhs))
      && !POINTER_TYPE_P (TREE_TYPE (rhs))
      && SSA_NAME_RANGE_INFO (lhs) != SSA_NAME_RANGE_INFO (rhs))
    return false;

  return true;  /* A statement of type _2 = _1;.  */
}

/* Return all statements in cfun that could generate copies.  All statements
   for which stmt_may_generate_copy returns 'true'.  */

static auto_vec<gimple *>
get_all_stmt_may_generate_copy (void)
{
  auto_vec<gimple *> result;

  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    {
      gimple_stmt_iterator gsi;
      for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *s = gsi_stmt (gsi);
	  if (stmt_may_generate_copy (s))
	    result.safe_push (s);
	}

      gphi_iterator pi;
      for (pi = gsi_start_phis (bb); !gsi_end_p (pi); gsi_next (&pi))
	{
	  gimple *s = pi.phi ();
	  if (stmt_may_generate_copy (s))
	    result.safe_push (s);
	}
    }

  return result;
}

/* SCC copy propagation

   'scc_copy_prop::propagate ()' is the main function of this pass.  */

class scc_copy_prop
{
public:
  scc_copy_prop ();
  ~scc_copy_prop ();
  void propagate ();

private:
  /* Bitmap tracking statements which were propagated so that they can be
     removed at the end of the pass.  */
  bitmap dead_stmts;

  void visit_op (tree op, hash_set<tree> &outer_ops,
				hash_set<gimple *> &scc_set, bool &is_inner,
				tree &last_outer_op);
  void replace_scc_by_value (vec<gimple *> scc, tree val);
};

/* For each statement from given SCC, replace its usages by value
   VAL.  */

void
scc_copy_prop::replace_scc_by_value (vec<gimple *> scc, tree val)
{
  for (gimple *stmt : scc)
    {
      tree name = gimple_get_lhs (stmt);
      replace_uses_by (name, val);
      bitmap_set_bit (dead_stmts, SSA_NAME_VERSION (name));
    }

  if (dump_file)
    fprintf (dump_file, "Replacing SCC of size %d\n", scc.length ());
}

/* Part of 'scc_copy_prop::propagate ()'.  */

void
scc_copy_prop::visit_op (tree op, hash_set<tree> &outer_ops,
			 hash_set<gimple *> &scc_set, bool &is_inner,
			 tree &last_outer_op)
{
  bool op_in_scc = false;

  if (TREE_CODE (op) == SSA_NAME)
    {
      gimple *op_stmt = SSA_NAME_DEF_STMT (op);
      if (scc_set.contains (op_stmt))
	op_in_scc = true;
    }

  if (!op_in_scc)
    {
      outer_ops.add (op);
      last_outer_op = op;
      is_inner = false;
    }
}

/* Main function of this pass.  Find and propagate all three types of copy
   statements (see pass description above).

   This is an implementation of an algorithm from the paper Simple and
   Efficient Construction of Static Single Assignmemnt Form[1].  It is based
   on strongly-connected components (SCCs) in dataflow graph.  The original
   algorithm only considers PHI statements.  We extend it to also consider
   assignment statements of type _2 = _1;.

   The algorithm is based on this definition of a set of redundant PHIs[1]:

     A non-empty set P of PHI functions is redundant iff the PHI functions just
     reference each other or one other value

   It uses this lemma[1]:

     Let P be a redundant set of PHI functions.  Then there is a
     strongly-connected component S subset of P that is also redundant.

   The algorithm works in this way:

     1 Find SCCs
     2 For each SCC S in topological order:
     3   Construct set 'inner' of statements that only have other statements
	 from S on their right hand side
     4   Construct set 'outer' of values that originate outside S and appear on
	 right hand side of some statement from S
     5   If |outer| = 1, outer only contains a value v.  Statements in S only
	 refer to each other or to v -- they are redundant.  Propagate v.
	 Else, recurse on statements in inner.

   The implementation is non-recursive.

   References:

     [1] Simple and Efficient Construction of Static Single Assignmemnt Form,
     Braun, Buchwald, Hack, Leissa, Mallon, Zwinkau, 2013, LNCS vol. 7791,
     Section 3.2.  */

void
scc_copy_prop::propagate ()
{
  auto_vec<gimple *> useful_stmts = get_all_stmt_may_generate_copy ();
  scc_discovery discovery;

  auto_vec<vec<gimple *>> worklist = discovery.compute_sccs (useful_stmts);

  while (!worklist.is_empty ())
    {
      vec<gimple *> scc = worklist.pop ();

      /* When we do 'replace_scc_by_value' it may happen that some EH edges
	 get removed.  That means parts of CFG get removed.  Those may
	 contain copy statements.  For that reason we prune SCCs here.  */
      unsigned i;
      for (i = 0; i < scc.length (); i++)
	if (gimple_bb (scc[i]) == NULL)
	  scc.unordered_remove (i);
      if (scc.is_empty ())
	{
	  scc.release ();
	  continue;
	}

      auto_vec<gimple *> inner;
      hash_set<tree> outer_ops;
      tree last_outer_op = NULL_TREE;

      /* Prepare hash set of PHIs in scc to query later.  */
      hash_set<gimple *> scc_set;
      for (gimple *stmt : scc)
	scc_set.add (stmt);

      for (gimple *stmt : scc)
	{
	  bool is_inner = true;

	  gphi *phi;
	  tree op;

	  switch (gimple_code (stmt))
	    {
	      case GIMPLE_PHI:
		phi = as_a <gphi *> (stmt);
		unsigned j;
		for (j = 0; j < gimple_phi_num_args (phi); j++)
		  {
		    op = gimple_phi_arg_def (phi, j);
		    visit_op (op, outer_ops, scc_set, is_inner, last_outer_op);
		  }
		break;
	      case GIMPLE_ASSIGN:
		op = gimple_assign_rhs1 (stmt);
		visit_op (op, outer_ops, scc_set, is_inner, last_outer_op);
		break;
	      default:
		gcc_unreachable ();
	    }

	  if (is_inner)
	    inner.safe_push (stmt);
	}

      if (outer_ops.elements () == 1)
	{
	  /* The only operand in outer_ops.  */
	  tree outer_op = last_outer_op;
	  replace_scc_by_value (scc, outer_op);
	}
      else if (outer_ops.elements () > 1)
	{
	  /* Add inner sccs to worklist.  */
	  auto_vec<vec<gimple *>> inner_sccs
	    = discovery.compute_sccs (inner);
	  for (vec<gimple *> inner_scc : inner_sccs)
	    worklist.safe_push (inner_scc);
	}
      else
	gcc_unreachable ();

      scc.release ();
    }
}

scc_copy_prop::scc_copy_prop ()
{
  /* For propagated statements.  */
  dead_stmts = BITMAP_ALLOC (NULL);
}

scc_copy_prop::~scc_copy_prop ()
{
  /* Remove all propagated statements.  */
  simple_dce_from_worklist (dead_stmts);
  BITMAP_FREE (dead_stmts);

  /* Propagating a constant may create dead eh edges.  */
  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    gimple_purge_dead_eh_edges (bb);
}

namespace {

const pass_data pass_data_sccopy =
{
  GIMPLE_PASS, /* type */
  "sccopy", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  ( PROP_cfg | PROP_ssa ), /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_update_ssa | TODO_cleanup_cfg, /* todo_flags_finish */
};

class pass_sccopy : public gimple_opt_pass
{
public:
  pass_sccopy (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_sccopy, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return true; }
  virtual unsigned int execute (function *);
  opt_pass * clone () final override { return new pass_sccopy (m_ctxt); }
}; // class pass_sccopy

unsigned
pass_sccopy::execute (function *)
{
  scc_copy_prop sccopy;
  sccopy.propagate ();
  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_sccopy (gcc::context *ctxt)
{
  return new pass_sccopy (ctxt);
}
