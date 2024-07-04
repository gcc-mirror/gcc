/* -Winfinite-recursion support.
   Copyright (C) 2021-2024 Free Software Foundation, Inc.
   Contributed by Martin Sebor <msebor@redhat.com>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "ssa.h"
#include "diagnostic-core.h"
// #include "tree-dfa.h"
#include "attribs.h"
#include "gimple-iterator.h"

namespace {

const pass_data warn_recursion_data =
{
  GIMPLE_PASS, /* type */
  "*infinite-recursion", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_warn_recursion : public gimple_opt_pass
{
public:
  pass_warn_recursion (gcc::context *);

private:
  bool gate (function *) final override { return warn_infinite_recursion; }

  unsigned int execute (function *) final override;

  bool find_function_exit (basic_block);

  /* Recursive calls found in M_FUNC.  */
  vec<gimple *> *m_calls;
  /* Basic blocks already visited in the current function.  */
  bitmap m_visited;
  /* The current function.  */
  function *m_func;
  /* The current function code if it's (also) a built-in.  */
  built_in_function m_built_in;
  /* True if M_FUNC is a noreturn function.  */
  bool noreturn_p;
};

/* Initialize the pass and its members.  */

pass_warn_recursion::pass_warn_recursion (gcc::context *ctxt)
  : gimple_opt_pass (warn_recursion_data, ctxt),
    m_calls (), m_visited (), m_func (), m_built_in (), noreturn_p ()
{
}

/* Return true if there is path from BB to M_FUNC exit point along which
   there is no (recursive) call to M_FUNC.  */

bool
pass_warn_recursion::find_function_exit (basic_block bb)
{
  if (!bitmap_set_bit (m_visited, bb->index))
    return false;

  if (bb == EXIT_BLOCK_PTR_FOR_FN (m_func))
    return true;

  /* Iterate over statements in BB, looking for a call to FNDECL.  */
  for (auto si = gsi_start_bb (bb); !gsi_end_p (si); gsi_next_nondebug (&si))
    {
      gimple *stmt = gsi_stmt (si);
      if (!is_gimple_call (stmt))
	continue;

      if (gimple_call_builtin_p (stmt, BUILT_IN_LONGJMP))
	/* A longjmp breaks infinite recursion.  */
	return true;

      if (tree fndecl = gimple_call_fndecl (stmt))
	{
	  /* A throw statement breaks infinite recursion.  */
	  tree id = DECL_NAME (fndecl);
	  const char *name = IDENTIFIER_POINTER (id);
	  if (startswith (name, "__cxa_throw"))
	    return true;
	  /* As does a call to POSIX siglongjmp.  */
	  if (!strcmp (name, "siglongjmp"))
	    return true;

	  if (m_built_in
	      && gimple_call_builtin_p (stmt, BUILT_IN_NORMAL)
	      && m_built_in == DECL_FUNCTION_CODE (fndecl))
	    {
	      const char *cname
		= IDENTIFIER_POINTER (DECL_NAME (current_function_decl));
	      /* Don't warn about gnu_inline extern inline function
		 like strcpy calling __builtin_strcpy, that is fine,
		 if some call is made (the builtin isn't expanded inline),
		 a call is made to the external definition.  */
	      if (!(DECL_DECLARED_INLINE_P (current_function_decl)
		    && DECL_EXTERNAL (current_function_decl))
		  || strcmp (name, cname) == 0)
		{
		  /* The call is being made from the definition of a built-in
		     (e.g., in a replacement of one) to itself.  */
		  m_calls->safe_push (stmt);
		  return false;
		}
	    }
	}

      if (noreturn_p)
	{
	  /* A noreturn call breaks infinite recursion.  */
	  int flags = gimple_call_flags (stmt);
	  if (flags & ECF_NORETURN)
	    return true;
	}

      tree callee = gimple_call_fndecl (stmt);
      if (!callee || m_func->decl != callee)
	continue;

      /* Add the recursive call to the vector and return false.  */
      m_calls->safe_push (stmt);
      return false;
    }

  /* If no call to FNDECL has been found search all BB's successors.  */
  edge e;
  edge_iterator ei;
  FOR_EACH_EDGE (e, ei, bb->succs)
    if (find_function_exit (e->dest))
      return true;

  return false;
}


/* Search FUNC for unconditionally infinitely recursive calls to self
   and issue a warning if it is such a function.  */

unsigned int
pass_warn_recursion::execute (function *func)
{
  auto_bitmap visited;
  auto_vec<gimple *> calls;

  m_visited = visited;
  m_calls = &calls;
  m_func = func;

  /* Avoid diagnosing an apparently infinitely recursive function that
     doesn't return where the infinite recursion might be avoided by
     a call to another noreturn function.  */
  noreturn_p = lookup_attribute ("noreturn", DECL_ATTRIBUTES (m_func->decl));

  if (fndecl_built_in_p (m_func->decl, BUILT_IN_NORMAL))
    m_built_in = DECL_FUNCTION_CODE (m_func->decl);
  else
    m_built_in = BUILT_IN_NONE;

  basic_block entry_bb = ENTRY_BLOCK_PTR_FOR_FN (func);

  if (find_function_exit (entry_bb) || m_calls->length () == 0)
    return 0;

  if (warning_at (DECL_SOURCE_LOCATION (func->decl),
		  OPT_Winfinite_recursion,
		  "infinite recursion detected"))
    for (auto stmt: *m_calls)
      {
	location_t loc = gimple_location (stmt);
	if (loc == UNKNOWN_LOCATION)
	  continue;

	inform (loc, "recursive call");
      }

  return 0;
}

} // namespace

gimple_opt_pass *
make_pass_warn_recursion (gcc::context *ctxt)
{
  return new pass_warn_recursion (ctxt);
}
