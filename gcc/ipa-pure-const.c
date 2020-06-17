/* Callgraph based analysis of static variables.
   Copyright (C) 2004-2020 Free Software Foundation, Inc.
   Contributed by Kenneth Zadeck <zadeck@naturalbridge.com>

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

/* This file marks functions as being either const (TREE_READONLY) or
   pure (DECL_PURE_P).  It can also set a variant of these that
   are allowed to loop indefinitely (DECL_LOOPING_CONST_PURE_P).

   This must be run after inlining decisions have been made since
   otherwise, the local sets will not contain information that is
   consistent with post inlined state.  The global sets are not prone
   to this problem since they are by definition transitive.  */

/* The code in this module is called by the ipa pass manager. It
   should be one of the later passes since it's information is used by
   the rest of the compilation. */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "tree.h"
#include "gimple.h"
#include "tree-pass.h"
#include "tree-streamer.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "calls.h"
#include "cfganal.h"
#include "tree-eh.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-niter.h"
#include "langhooks.h"
#include "ipa-utils.h"
#include "gimple-pretty-print.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "intl.h"
#include "opts.h"
#include "ssa.h"
#include "alloc-pool.h"
#include "symbol-summary.h"
#include "ipa-prop.h"
#include "ipa-fnsummary.h"

/* Lattice values for const and pure functions.  Everything starts out
   being const, then may drop to pure and then neither depending on
   what is found.  */
enum pure_const_state_e
{
  IPA_CONST,
  IPA_PURE,
  IPA_NEITHER
};

static const char *pure_const_names[3] = {"const", "pure", "neither"};

enum malloc_state_e
{
  STATE_MALLOC_TOP,
  STATE_MALLOC,
  STATE_MALLOC_BOTTOM
};

static const char *malloc_state_names[] = {"malloc_top", "malloc", "malloc_bottom"};

/* Holder for the const_state.  There is one of these per function
   decl.  */
class funct_state_d
{
public:
  funct_state_d (): pure_const_state (IPA_NEITHER),
    state_previously_known (IPA_NEITHER), looping_previously_known (true),
    looping (true), can_throw (true), can_free (true),
    malloc_state (STATE_MALLOC_BOTTOM) {}

  funct_state_d (const funct_state_d &s): pure_const_state (s.pure_const_state),
    state_previously_known (s.state_previously_known),
    looping_previously_known (s.looping_previously_known),
    looping (s.looping), can_throw (s.can_throw), can_free (s.can_free),
    malloc_state (s.malloc_state) {}

  /* See above.  */
  enum pure_const_state_e pure_const_state;
  /* What user set here; we can be always sure about this.  */
  enum pure_const_state_e state_previously_known;
  bool looping_previously_known;

  /* True if the function could possibly infinite loop.  There are a
     lot of ways that this could be determined.  We are pretty
     conservative here.  While it is possible to cse pure and const
     calls, it is not legal to have dce get rid of the call if there
     is a possibility that the call could infinite loop since this is
     a behavioral change.  */
  bool looping;

  bool can_throw;

  /* If function can call free, munmap or otherwise make previously
     non-trapping memory accesses trapping.  */
  bool can_free;

  enum malloc_state_e malloc_state;
};

typedef class funct_state_d * funct_state;

/* The storage of the funct_state is abstracted because there is the
   possibility that it may be desirable to move this to the cgraph
   local info.  */

class funct_state_summary_t:
  public fast_function_summary <funct_state_d *, va_heap>
{
public:
  funct_state_summary_t (symbol_table *symtab):
    fast_function_summary <funct_state_d *, va_heap> (symtab) {}

  virtual void insert (cgraph_node *, funct_state_d *state);
  virtual void duplicate (cgraph_node *src_node, cgraph_node *dst_node,
			  funct_state_d *src_data,
			  funct_state_d *dst_data);
};

static funct_state_summary_t *funct_state_summaries = NULL;

static bool gate_pure_const (void);

namespace {

const pass_data pass_data_ipa_pure_const =
{
  IPA_PASS, /* type */
  "pure-const", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_IPA_PURE_CONST, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_ipa_pure_const : public ipa_opt_pass_d
{
public:
  pass_ipa_pure_const(gcc::context *ctxt);

  /* opt_pass methods: */
  bool gate (function *) { return gate_pure_const (); }
  unsigned int execute (function *fun);

  void register_hooks (void);

private:
  bool init_p;
}; // class pass_ipa_pure_const

} // anon namespace

/* Try to guess if function body will always be visible to compiler
   when compiling the call and whether compiler will be able
   to propagate the information by itself.  */

static bool
function_always_visible_to_compiler_p (tree decl)
{
  return (!TREE_PUBLIC (decl) || DECL_DECLARED_INLINE_P (decl)
	  || DECL_COMDAT (decl));
}

/* Emit suggestion about attribute ATTRIB_NAME for DECL.  KNOWN_FINITE
   is true if the function is known to be finite.  The diagnostic is
   controlled by OPTION.  WARNED_ABOUT is a hash_set<tree> unique for
   OPTION, this function may initialize it and it is always returned
   by the function.  */

static hash_set<tree> *
suggest_attribute (int option, tree decl, bool known_finite,
		   hash_set<tree> *warned_about,
		   const char * attrib_name)
{
  if (!option_enabled (option, lang_hooks.option_lang_mask (), &global_options))
    return warned_about;
  if (TREE_THIS_VOLATILE (decl)
      || (known_finite && function_always_visible_to_compiler_p (decl)))
    return warned_about;

  if (!warned_about)
    warned_about = new hash_set<tree>;
  if (warned_about->contains (decl))
    return warned_about;
  warned_about->add (decl);
  warning_at (DECL_SOURCE_LOCATION (decl),
	      option,
	      known_finite
	      ? G_("function might be candidate for attribute %qs")
	      : G_("function might be candidate for attribute %qs"
		   " if it is known to return normally"), attrib_name);
  return warned_about;
}

/* Emit suggestion about __attribute_((pure)) for DECL.  KNOWN_FINITE
   is true if the function is known to be finite.  */

static void
warn_function_pure (tree decl, bool known_finite)
{
  /* Declaring a void function pure makes no sense and is diagnosed
     by -Wattributes because calling it would have no effect.  */
  if (VOID_TYPE_P (TREE_TYPE (TREE_TYPE (decl))))
    return;

  static hash_set<tree> *warned_about;
  warned_about
    = suggest_attribute (OPT_Wsuggest_attribute_pure, decl,
			 known_finite, warned_about, "pure");
}

/* Emit suggestion about __attribute_((const)) for DECL.  KNOWN_FINITE
   is true if the function is known to be finite.  */

static void
warn_function_const (tree decl, bool known_finite)
{
  /* Declaring a void function const makes no sense is diagnosed
     by -Wattributes because calling it would have no effect.  */
  if (VOID_TYPE_P (TREE_TYPE (TREE_TYPE (decl))))
    return;

  static hash_set<tree> *warned_about;
  warned_about
    = suggest_attribute (OPT_Wsuggest_attribute_const, decl,
			 known_finite, warned_about, "const");
}

/* Emit suggestion about __attribute__((malloc)) for DECL.  */

static void
warn_function_malloc (tree decl)
{
  static hash_set<tree> *warned_about;
  warned_about
    = suggest_attribute (OPT_Wsuggest_attribute_malloc, decl,
			 true, warned_about, "malloc");
}

/* Emit suggestion about __attribute__((noreturn)) for DECL.  */

static void
warn_function_noreturn (tree decl)
{
  tree original_decl = decl;

  static hash_set<tree> *warned_about;
  if (!lang_hooks.missing_noreturn_ok_p (decl)
      && targetm.warn_func_return (decl))
    warned_about 
      = suggest_attribute (OPT_Wsuggest_attribute_noreturn, original_decl,
			   true, warned_about, "noreturn");
}

void
warn_function_cold (tree decl)
{
  tree original_decl = decl;

  static hash_set<tree> *warned_about;
  warned_about 
    = suggest_attribute (OPT_Wsuggest_attribute_cold, original_decl,
			 true, warned_about, "cold");
}

/* Check to see if the use (or definition when CHECKING_WRITE is true)
   variable T is legal in a function that is either pure or const.  */

static inline void
check_decl (funct_state local,
	    tree t, bool checking_write, bool ipa)
{
  /* Do not want to do anything with volatile except mark any
     function that uses one to be not const or pure.  */
  if (TREE_THIS_VOLATILE (t))
    {
      local->pure_const_state = IPA_NEITHER;
      if (dump_file)
        fprintf (dump_file, "    Volatile operand is not const/pure\n");
      return;
    }

  /* Do not care about a local automatic that is not static.  */
  if (!TREE_STATIC (t) && !DECL_EXTERNAL (t))
    return;

  /* If the variable has the "used" attribute, treat it as if it had a
     been touched by the devil.  */
  if (DECL_PRESERVE_P (t))
    {
      local->pure_const_state = IPA_NEITHER;
      if (dump_file)
        fprintf (dump_file, "    Used static/global variable is not const/pure\n");
      return;
    }

  /* In IPA mode we are not interested in checking actual loads and stores;
     they will be processed at propagation time using ipa_ref.  */
  if (ipa)
    return;

  /* Since we have dealt with the locals and params cases above, if we
     are CHECKING_WRITE, this cannot be a pure or constant
     function.  */
  if (checking_write)
    {
      local->pure_const_state = IPA_NEITHER;
      if (dump_file)
        fprintf (dump_file, "    static/global memory write is not const/pure\n");
      return;
    }

  if (DECL_EXTERNAL (t) || TREE_PUBLIC (t))
    {
      /* Readonly reads are safe.  */
      if (TREE_READONLY (t))
	return; /* Read of a constant, do not change the function state.  */
      else
	{
          if (dump_file)
            fprintf (dump_file, "    global memory read is not const\n");
	  /* Just a regular read.  */
	  if (local->pure_const_state == IPA_CONST)
	    local->pure_const_state = IPA_PURE;
	}
    }
  else
    {
      /* Compilation level statics can be read if they are readonly
	 variables.  */
      if (TREE_READONLY (t))
	return;

      if (dump_file)
	fprintf (dump_file, "    static memory read is not const\n");
      /* Just a regular read.  */
      if (local->pure_const_state == IPA_CONST)
	local->pure_const_state = IPA_PURE;
    }
}


/* Check to see if the use (or definition when CHECKING_WRITE is true)
   variable T is legal in a function that is either pure or const.  */

static inline void
check_op (funct_state local, tree t, bool checking_write)
{
  t = get_base_address (t);
  if (t && TREE_THIS_VOLATILE (t))
    {
      local->pure_const_state = IPA_NEITHER;
      if (dump_file)
	fprintf (dump_file, "    Volatile indirect ref is not const/pure\n");
      return;
    }
  else if (t
  	   && (INDIRECT_REF_P (t) || TREE_CODE (t) == MEM_REF)
	   && TREE_CODE (TREE_OPERAND (t, 0)) == SSA_NAME
	   && !ptr_deref_may_alias_global_p (TREE_OPERAND (t, 0)))
    {
      if (dump_file)
	fprintf (dump_file, "    Indirect ref to local memory is OK\n");
      return;
    }
  else if (checking_write)
    {
      local->pure_const_state = IPA_NEITHER;
      if (dump_file)
	fprintf (dump_file, "    Indirect ref write is not const/pure\n");
      return;
    }
  else
    {
      if (dump_file)
	fprintf (dump_file, "    Indirect ref read is not const\n");
      if (local->pure_const_state == IPA_CONST)
	local->pure_const_state = IPA_PURE;
    }
}

/* compute state based on ECF FLAGS and store to STATE and LOOPING.  */

static void
state_from_flags (enum pure_const_state_e *state, bool *looping,
	          int flags, bool cannot_lead_to_return)
{
  *looping = false;
  if (flags & ECF_LOOPING_CONST_OR_PURE)
    {
      *looping = true;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, " looping\n");
    }
  if (flags & ECF_CONST)
    {
      *state = IPA_CONST;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, " const\n");
    }
  else if (flags & ECF_PURE)
    {
      *state = IPA_PURE;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, " pure\n");
    }
  else if (cannot_lead_to_return)
    {
      *state = IPA_PURE;
      *looping = true;
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, " ignoring side effects->pure looping\n");
    }
  else
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, " neither\n");
      *state = IPA_NEITHER;
      *looping = true;
    }
}

/* Merge STATE and STATE2 and LOOPING and LOOPING2 and store
   into STATE and LOOPING better of the two variants.
   Be sure to merge looping correctly.  IPA_NEITHER functions
   have looping 0 even if they don't have to return.  */

static inline void
better_state (enum pure_const_state_e *state, bool *looping,
	      enum pure_const_state_e state2, bool looping2)
{
  if (state2 < *state)
    {
      if (*state == IPA_NEITHER)
	*looping = looping2;
      else
	*looping = MIN (*looping, looping2);
      *state = state2;
    }
  else if (state2 != IPA_NEITHER)
    *looping = MIN (*looping, looping2);
}

/* Merge STATE and STATE2 and LOOPING and LOOPING2 and store
   into STATE and LOOPING worse of the two variants.
   N is the actual node called.  */

static inline void
worse_state (enum pure_const_state_e *state, bool *looping,
	     enum pure_const_state_e state2, bool looping2,
	     struct symtab_node *from,
	     struct symtab_node *to)
{
  /* Consider function:

     bool a(int *p)
     {
       return *p==*p;
     }

     During early optimization we will turn this into:

     bool a(int *p)
     {
       return true;
     }

     Now if this function will be detected as CONST however when interposed it
     may end up being just pure.  We always must assume the worst scenario here.
   */
  if (*state == IPA_CONST && state2 == IPA_CONST
      && to && !TREE_READONLY (to->decl) && !to->binds_to_current_def_p (from))
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Dropping state to PURE because call to %s may not "
		 "bind to current def.\n", to->dump_name ());
      state2 = IPA_PURE;
    }
  *state = MAX (*state, state2);
  *looping = MAX (*looping, looping2);
}

/* Recognize special cases of builtins that are by themselves not pure or const
   but function using them is.  */
static bool
special_builtin_state (enum pure_const_state_e *state, bool *looping,
		       tree callee)
{
  if (DECL_BUILT_IN_CLASS (callee) == BUILT_IN_NORMAL)
    switch (DECL_FUNCTION_CODE (callee))
      {
      case BUILT_IN_RETURN:
      case BUILT_IN_UNREACHABLE:
      CASE_BUILT_IN_ALLOCA:
      case BUILT_IN_STACK_SAVE:
      case BUILT_IN_STACK_RESTORE:
      case BUILT_IN_EH_POINTER:
      case BUILT_IN_EH_FILTER:
      case BUILT_IN_UNWIND_RESUME:
      case BUILT_IN_CXA_END_CLEANUP:
      case BUILT_IN_EH_COPY_VALUES:
      case BUILT_IN_FRAME_ADDRESS:
      case BUILT_IN_APPLY_ARGS:
      case BUILT_IN_ASAN_BEFORE_DYNAMIC_INIT:
      case BUILT_IN_ASAN_AFTER_DYNAMIC_INIT:
	*looping = false;
	*state = IPA_CONST;
	return true;
      case BUILT_IN_PREFETCH:
	*looping = true;
	*state = IPA_CONST;
	return true;
      default:
	break;
      }
  return false;
}

/* Check the parameters of a function call to CALL_EXPR to see if
   there are any references in the parameters that are not allowed for
   pure or const functions.  Also check to see if this is either an
   indirect call, a call outside the compilation unit, or has special
   attributes that may also effect the purity.  The CALL_EXPR node for
   the entire call expression.  */

static void
check_call (funct_state local, gcall *call, bool ipa)
{
  int flags = gimple_call_flags (call);
  tree callee_t = gimple_call_fndecl (call);
  bool possibly_throws = stmt_could_throw_p (cfun, call);
  bool possibly_throws_externally = (possibly_throws
  				     && stmt_can_throw_external (cfun, call));

  if (possibly_throws)
    {
      unsigned int i;
      for (i = 0; i < gimple_num_ops (call); i++)
        if (gimple_op (call, i)
	    && tree_could_throw_p (gimple_op (call, i)))
	  {
	    if (possibly_throws && cfun->can_throw_non_call_exceptions)
	      {
		if (dump_file)
		  fprintf (dump_file, "    operand can throw; looping\n");
		local->looping = true;
	      }
	    if (possibly_throws_externally)
	      {
		if (dump_file)
		  fprintf (dump_file, "    operand can throw externally\n");
		local->can_throw = true;
	      }
	  }
    }

  /* The const and pure flags are set by a variety of places in the
     compiler (including here).  If someone has already set the flags
     for the callee, (such as for some of the builtins) we will use
     them, otherwise we will compute our own information.

     Const and pure functions have less clobber effects than other
     functions so we process these first.  Otherwise if it is a call
     outside the compilation unit or an indirect call we punt.  This
     leaves local calls which will be processed by following the call
     graph.  */
  if (callee_t)
    {
      enum pure_const_state_e call_state;
      bool call_looping;

      if (gimple_call_builtin_p (call, BUILT_IN_NORMAL)
	  && !nonfreeing_call_p (call))
	local->can_free = true;

      if (special_builtin_state (&call_state, &call_looping, callee_t))
	{
	  worse_state (&local->pure_const_state, &local->looping,
		       call_state, call_looping,
		       NULL, NULL);
	  return;
	}
      /* When bad things happen to bad functions, they cannot be const
	 or pure.  */
      if (setjmp_call_p (callee_t))
	{
	  if (dump_file)
	    fprintf (dump_file, "    setjmp is not const/pure\n");
          local->looping = true;
	  local->pure_const_state = IPA_NEITHER;
	}

      if (DECL_BUILT_IN_CLASS (callee_t) == BUILT_IN_NORMAL)
	switch (DECL_FUNCTION_CODE (callee_t))
	  {
	  case BUILT_IN_LONGJMP:
	  case BUILT_IN_NONLOCAL_GOTO:
	    if (dump_file)
	      fprintf (dump_file,
		       "    longjmp and nonlocal goto is not const/pure\n");
	    local->pure_const_state = IPA_NEITHER;
	    local->looping = true;
	    break;
	  default:
	    break;
	  }
    }
  else if (gimple_call_internal_p (call) && !nonfreeing_call_p (call))
    local->can_free = true;

  /* When not in IPA mode, we can still handle self recursion.  */
  if (!ipa && callee_t
      && recursive_call_p (current_function_decl, callee_t))
    {
      if (dump_file)
        fprintf (dump_file, "    Recursive call can loop.\n");
      local->looping = true;
    }
  /* Either callee is unknown or we are doing local analysis.
     Look to see if there are any bits available for the callee (such as by
     declaration or because it is builtin) and process solely on the basis of
     those bits.  Handle internal calls always, those calls don't have
     corresponding cgraph edges and thus aren't processed during
     the propagation.  */
  else if (!ipa || gimple_call_internal_p (call))
    {
      enum pure_const_state_e call_state;
      bool call_looping;
      if (possibly_throws && cfun->can_throw_non_call_exceptions)
        {
	  if (dump_file)
	    fprintf (dump_file, "    can throw; looping\n");
          local->looping = true;
	}
      if (possibly_throws_externally)
        {
	  if (dump_file)
	    {
	      fprintf (dump_file, "    can throw externally to lp %i\n",
	      	       lookup_stmt_eh_lp (call));
	      if (callee_t)
		fprintf (dump_file, "     callee:%s\n",
			 IDENTIFIER_POINTER (DECL_ASSEMBLER_NAME (callee_t)));
	    }
          local->can_throw = true;
	}
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "    checking flags for call:");
      state_from_flags (&call_state, &call_looping, flags,
			((flags & (ECF_NORETURN | ECF_NOTHROW))
			 == (ECF_NORETURN | ECF_NOTHROW))
			|| (!flag_exceptions && (flags & ECF_NORETURN)));
      worse_state (&local->pure_const_state, &local->looping,
		   call_state, call_looping, NULL, NULL);
    }
  /* Direct functions calls are handled by IPA propagation.  */
}

/* Wrapper around check_decl for loads in local more.  */

static bool
check_load (gimple *, tree op, tree, void *data)
{
  if (DECL_P (op))
    check_decl ((funct_state)data, op, false, false);
  else
    check_op ((funct_state)data, op, false);
  return false;
}

/* Wrapper around check_decl for stores in local more.  */

static bool
check_store (gimple *, tree op, tree, void *data)
{
  if (DECL_P (op))
    check_decl ((funct_state)data, op, true, false);
  else
    check_op ((funct_state)data, op, true);
  return false;
}

/* Wrapper around check_decl for loads in ipa mode.  */

static bool
check_ipa_load (gimple *, tree op, tree, void *data)
{
  if (DECL_P (op))
    check_decl ((funct_state)data, op, false, true);
  else
    check_op ((funct_state)data, op, false);
  return false;
}

/* Wrapper around check_decl for stores in ipa mode.  */

static bool
check_ipa_store (gimple *, tree op, tree, void *data)
{
  if (DECL_P (op))
    check_decl ((funct_state)data, op, true, true);
  else
    check_op ((funct_state)data, op, true);
  return false;
}

/* Look into pointer pointed to by GSIP and figure out what interesting side
   effects it has.  */
static void
check_stmt (gimple_stmt_iterator *gsip, funct_state local, bool ipa)
{
  gimple *stmt = gsi_stmt (*gsip);

  if (is_gimple_debug (stmt))
    return;

  /* Do consider clobber as side effects before IPA, so we rather inline
     C++ destructors and keep clobber semantics than eliminate them.

     TODO: We may get smarter during early optimizations on these and let
     functions containing only clobbers to be optimized more.  This is a common
     case of C++ destructors.  */

  if ((ipa || cfun->after_inlining) && gimple_clobber_p (stmt))
    return;

  if (dump_file)
    {
      fprintf (dump_file, "  scanning: ");
      print_gimple_stmt (dump_file, stmt, 0);
    }

  if (gimple_has_volatile_ops (stmt)
      && !gimple_clobber_p (stmt))
    {
      local->pure_const_state = IPA_NEITHER;
      if (dump_file)
	fprintf (dump_file, "    Volatile stmt is not const/pure\n");
    }

  /* Look for loads and stores.  */
  walk_stmt_load_store_ops (stmt, local,
			    ipa ? check_ipa_load : check_load,
			    ipa ? check_ipa_store :  check_store);

  if (gimple_code (stmt) != GIMPLE_CALL
      && stmt_could_throw_p (cfun, stmt))
    {
      if (cfun->can_throw_non_call_exceptions)
	{
	  if (dump_file)
	    fprintf (dump_file, "    can throw; looping\n");
	  local->looping = true;
	}
      if (stmt_can_throw_external (cfun, stmt))
	{
	  if (dump_file)
	    fprintf (dump_file, "    can throw externally\n");
	  local->can_throw = true;
	}
      else
	if (dump_file)
	  fprintf (dump_file, "    can throw\n");
    }
  switch (gimple_code (stmt))
    {
    case GIMPLE_CALL:
      check_call (local, as_a <gcall *> (stmt), ipa);
      break;
    case GIMPLE_LABEL:
      if (DECL_NONLOCAL (gimple_label_label (as_a <glabel *> (stmt))))
	/* Target of long jump. */
	{
          if (dump_file)
            fprintf (dump_file, "    nonlocal label is not const/pure\n");
	  local->pure_const_state = IPA_NEITHER;
	}
      break;
    case GIMPLE_ASM:
      if (gimple_asm_clobbers_memory_p (as_a <gasm *> (stmt)))
	{
	  if (dump_file)
	    fprintf (dump_file, "    memory asm clobber is not const/pure\n");
	  /* Abandon all hope, ye who enter here. */
	  local->pure_const_state = IPA_NEITHER;
	  local->can_free = true;
	}
      if (gimple_asm_volatile_p (as_a <gasm *> (stmt)))
	{
	  if (dump_file)
	    fprintf (dump_file, "    volatile is not const/pure\n");
	  /* Abandon all hope, ye who enter here. */
	  local->pure_const_state = IPA_NEITHER;
	  local->looping = true;
	  local->can_free = true;
	}
      return;
    default:
      break;
    }
}

/* Check that RETVAL is used only in STMT and in comparisons against 0.
   RETVAL is return value of the function and STMT is return stmt.  */

static bool
check_retval_uses (tree retval, gimple *stmt)
{
  imm_use_iterator use_iter;
  gimple *use_stmt;

  FOR_EACH_IMM_USE_STMT (use_stmt, use_iter, retval)
    if (gcond *cond = dyn_cast<gcond *> (use_stmt))
      {
	tree op2 = gimple_cond_rhs (cond);
	if (!integer_zerop (op2))
	  RETURN_FROM_IMM_USE_STMT (use_iter, false);
      }
    else if (gassign *ga = dyn_cast<gassign *> (use_stmt))
      {
	enum tree_code code = gimple_assign_rhs_code (ga);
	if (TREE_CODE_CLASS (code) != tcc_comparison)
	  RETURN_FROM_IMM_USE_STMT (use_iter, false);
	if (!integer_zerop (gimple_assign_rhs2 (ga)))
	  RETURN_FROM_IMM_USE_STMT (use_iter, false);
      }
    else if (is_gimple_debug (use_stmt))
      ;
    else if (use_stmt != stmt)
      RETURN_FROM_IMM_USE_STMT (use_iter, false);

  return true;
}

/* malloc_candidate_p() checks if FUN can possibly be annotated with malloc
   attribute. Currently this function does a very conservative analysis.
   FUN is considered to be a candidate if
   1) It returns a value of pointer type.
   2) SSA_NAME_DEF_STMT (return_value) is either a function call or
      a phi, and element of phi is either NULL or
      SSA_NAME_DEF_STMT(element) is function call.
   3) The return-value has immediate uses only within comparisons (gcond or gassign)
      and return_stmt (and likewise a phi arg has immediate use only within comparison
      or the phi stmt).  */

#define DUMP_AND_RETURN(reason)  \
{  \
  if (dump_file && (dump_flags & TDF_DETAILS))  \
    fprintf (dump_file, "\n%s is not a malloc candidate, reason: %s\n", \
	     (node->dump_name ()), (reason));  \
  return false;  \
}

static bool
malloc_candidate_p_1 (function *fun, tree retval, gimple *ret_stmt, bool ipa,
		      bitmap visited)
{
  cgraph_node *node = cgraph_node::get_create (fun->decl);
  if (!bitmap_set_bit (visited, SSA_NAME_VERSION (retval)))
    return true;

  if (!check_retval_uses (retval, ret_stmt))
    DUMP_AND_RETURN("Return value has uses outside return stmt"
		    " and comparisons against 0.")

  gimple *def = SSA_NAME_DEF_STMT (retval);

  if (gcall *call_stmt = dyn_cast<gcall *> (def))
    {
      tree callee_decl = gimple_call_fndecl (call_stmt);
      if (!callee_decl)
	return false;

      if (!ipa && !DECL_IS_MALLOC (callee_decl))
	DUMP_AND_RETURN("callee_decl does not have malloc attribute for"
			" non-ipa mode.")

      cgraph_edge *cs = node->get_edge (call_stmt);
      if (cs)
	{
	  ipa_call_summary *es = ipa_call_summaries->get_create (cs);
	  es->is_return_callee_uncaptured = true;
	}
    }

    else if (gphi *phi = dyn_cast<gphi *> (def))
      {
	bool all_args_zero = true;
	for (unsigned i = 0; i < gimple_phi_num_args (phi); ++i)
	  {
	    tree arg = gimple_phi_arg_def (phi, i);
	    if (integer_zerop (arg))
	      continue;

	    all_args_zero = false;
	    if (TREE_CODE (arg) != SSA_NAME)
	      DUMP_AND_RETURN ("phi arg is not SSA_NAME.");
	    if (!check_retval_uses (arg, phi))
	      DUMP_AND_RETURN ("phi arg has uses outside phi"
				 " and comparisons against 0.")

	    gimple *arg_def = SSA_NAME_DEF_STMT (arg);
	    if (is_a<gphi *> (arg_def))
	      {
		if (!malloc_candidate_p_1 (fun, arg, phi, ipa, visited))
		    DUMP_AND_RETURN ("nested phi fail")
		continue;
	      }

	    gcall *call_stmt = dyn_cast<gcall *> (arg_def);
	    if (!call_stmt)
	      DUMP_AND_RETURN ("phi arg is a not a call_stmt.")

	    tree callee_decl = gimple_call_fndecl (call_stmt);
	    if (!callee_decl)
	      return false;
	    if (!ipa && !DECL_IS_MALLOC (callee_decl))
	      DUMP_AND_RETURN("callee_decl does not have malloc attribute"
			      " for non-ipa mode.")

	    cgraph_edge *cs = node->get_edge (call_stmt);
	    if (cs)
	      {
		ipa_call_summary *es = ipa_call_summaries->get_create (cs);
		es->is_return_callee_uncaptured = true;
	      }
	  }

	if (all_args_zero)
	  DUMP_AND_RETURN ("Return value is a phi with all args equal to 0.")
      }

    else
      DUMP_AND_RETURN("def_stmt of return value is not a call or phi-stmt.")

  return true;
}

static bool
malloc_candidate_p (function *fun, bool ipa)
{
  basic_block exit_block = EXIT_BLOCK_PTR_FOR_FN (fun);
  edge e;
  edge_iterator ei;
  cgraph_node *node = cgraph_node::get_create (fun->decl);

  if (EDGE_COUNT (exit_block->preds) == 0
      || !flag_delete_null_pointer_checks)
    return false;

  auto_bitmap visited;
  FOR_EACH_EDGE (e, ei, exit_block->preds)
    {
      gimple_stmt_iterator gsi = gsi_last_bb (e->src);
      greturn *ret_stmt = dyn_cast<greturn *> (gsi_stmt (gsi));

      if (!ret_stmt)
	return false;

      tree retval = gimple_return_retval (ret_stmt);
      if (!retval)
	DUMP_AND_RETURN("No return value.")

      if (TREE_CODE (retval) != SSA_NAME
	  || TREE_CODE (TREE_TYPE (retval)) != POINTER_TYPE)
	DUMP_AND_RETURN("Return value is not SSA_NAME or not a pointer type.")

      if (!malloc_candidate_p_1 (fun, retval, ret_stmt, ipa, visited))
	return false;
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "\nFound %s to be candidate for malloc attribute\n",
	     IDENTIFIER_POINTER (DECL_NAME (fun->decl)));
  return true;
}

#undef DUMP_AND_RETURN

/* This is the main routine for finding the reference patterns for
   global variables within a function FN.  */

static funct_state
analyze_function (struct cgraph_node *fn, bool ipa)
{
  tree decl = fn->decl;
  funct_state l;
  basic_block this_block;

  l = XCNEW (class funct_state_d);
  l->pure_const_state = IPA_CONST;
  l->state_previously_known = IPA_NEITHER;
  l->looping_previously_known = true;
  l->looping = false;
  l->can_throw = false;
  l->can_free = false;
  state_from_flags (&l->state_previously_known, &l->looping_previously_known,
		    flags_from_decl_or_type (fn->decl),
		    fn->cannot_return_p ());

  if (fn->thunk.thunk_p || fn->alias)
    {
      /* Thunk gets propagated through, so nothing interesting happens.  */
      gcc_assert (ipa);
      if (fn->thunk.thunk_p && fn->thunk.virtual_offset_p)
	l->pure_const_state = IPA_NEITHER;
      return l;
    }

  if (dump_file)
    {
      fprintf (dump_file, "\n\n local analysis of %s\n ",
	       fn->dump_name ());
    }

  push_cfun (DECL_STRUCT_FUNCTION (decl));

  FOR_EACH_BB_FN (this_block, cfun)
    {
      gimple_stmt_iterator gsi;
      struct walk_stmt_info wi;

      memset (&wi, 0, sizeof (wi));
      for (gsi = gsi_start_bb (this_block);
	   !gsi_end_p (gsi);
	   gsi_next (&gsi))
	{
	  check_stmt (&gsi, l, ipa);
	  if (l->pure_const_state == IPA_NEITHER
	      && l->looping
	      && l->can_throw
	      && l->can_free)
	    goto end;
	}
    }

end:
  if (l->pure_const_state != IPA_NEITHER)
    {
      /* Const functions cannot have back edges (an
	 indication of possible infinite loop side
	 effect.  */
      if (mark_dfs_back_edges ())
        {
	  /* Preheaders are needed for SCEV to work.
	     Simple latches and recorded exits improve chances that loop will
	     proved to be finite in testcases such as in loop-15.c
	     and loop-24.c  */
	  loop_optimizer_init (LOOPS_HAVE_PREHEADERS
			       | LOOPS_HAVE_SIMPLE_LATCHES
			       | LOOPS_HAVE_RECORDED_EXITS);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    flow_loops_dump (dump_file, NULL, 0);
	  if (mark_irreducible_loops ())
	    {
	      if (dump_file)
	        fprintf (dump_file, "    has irreducible loops\n");
	      l->looping = true;
	    }
	  else
	    {
	      class loop *loop;
	      scev_initialize ();
	      FOR_EACH_LOOP (loop, 0)
		if (!finite_loop_p (loop))
		  {
		    if (dump_file)
		      fprintf (dump_file, "    cannot prove finiteness of "
			       "loop %i\n", loop->num);
		    l->looping =true;
		    break;
		  }
	      scev_finalize ();
	    }
          loop_optimizer_finalize ();
	}
    }

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "    checking previously known:");

  better_state (&l->pure_const_state, &l->looping,
		l->state_previously_known,
		l->looping_previously_known);
  if (TREE_NOTHROW (decl))
    l->can_throw = false;

  l->malloc_state = STATE_MALLOC_BOTTOM;
  if (DECL_IS_MALLOC (decl))
    l->malloc_state = STATE_MALLOC;
  else if (ipa && malloc_candidate_p (DECL_STRUCT_FUNCTION (decl), true))
    l->malloc_state = STATE_MALLOC_TOP;
  else if (malloc_candidate_p (DECL_STRUCT_FUNCTION (decl), false))
    l->malloc_state = STATE_MALLOC;

  pop_cfun ();
  if (dump_file)
    {
      if (l->looping)
        fprintf (dump_file, "Function is locally looping.\n");
      if (l->can_throw)
        fprintf (dump_file, "Function is locally throwing.\n");
      if (l->pure_const_state == IPA_CONST)
        fprintf (dump_file, "Function is locally const.\n");
      if (l->pure_const_state == IPA_PURE)
        fprintf (dump_file, "Function is locally pure.\n");
      if (l->can_free)
	fprintf (dump_file, "Function can locally free.\n");
      if (l->malloc_state == STATE_MALLOC)
	fprintf (dump_file, "Function is locally malloc.\n");
    }
  return l;
}

void
funct_state_summary_t::insert (cgraph_node *node, funct_state_d *state)
{
  /* There are some shared nodes, in particular the initializers on
     static declarations.  We do not need to scan them more than once
     since all we would be interested in are the addressof
     operations.  */
  if (opt_for_fn (node->decl, flag_ipa_pure_const))
    {
      funct_state_d *a = analyze_function (node, true);
      new (state) funct_state_d (*a);
      free (a);
    }
}

/* Called when new clone is inserted to callgraph late.  */

void
funct_state_summary_t::duplicate (cgraph_node *, cgraph_node *dst,
				  funct_state_d *src_data,
				  funct_state_d *dst_data)
{
  new (dst_data) funct_state_d (*src_data);
  if (dst_data->malloc_state == STATE_MALLOC
      && VOID_TYPE_P (TREE_TYPE (TREE_TYPE (dst->decl))))
    dst_data->malloc_state = STATE_MALLOC_BOTTOM;
}


void
pass_ipa_pure_const::
register_hooks (void)
{
  if (init_p)
    return;

  init_p = true;

  funct_state_summaries = new funct_state_summary_t (symtab);
}


/* Analyze each function in the cgraph to see if it is locally PURE or
   CONST.  */

static void
pure_const_generate_summary (void)
{
  struct cgraph_node *node;

  pass_ipa_pure_const *pass = static_cast <pass_ipa_pure_const *> (current_pass);
  pass->register_hooks ();

  /* Process all of the functions.

     We process AVAIL_INTERPOSABLE functions.  We cannot use the results
     by default, but the info can be used at LTO with -fwhole-program or
     when function got cloned and the clone is AVAILABLE.  */

  FOR_EACH_DEFINED_FUNCTION (node)
    if (opt_for_fn (node->decl, flag_ipa_pure_const))
      {
	funct_state_d *a = analyze_function (node, true);
	new (funct_state_summaries->get_create (node)) funct_state_d (*a);
	free (a);
      }
}


/* Serialize the ipa info for lto.  */

static void
pure_const_write_summary (void)
{
  struct cgraph_node *node;
  struct lto_simple_output_block *ob
    = lto_create_simple_output_block (LTO_section_ipa_pure_const);
  unsigned int count = 0;
  lto_symtab_encoder_iterator lsei;
  lto_symtab_encoder_t encoder;

  encoder = lto_get_out_decl_state ()->symtab_node_encoder;

  for (lsei = lsei_start_function_in_partition (encoder); !lsei_end_p (lsei);
       lsei_next_function_in_partition (&lsei))
    {
      node = lsei_cgraph_node (lsei);
      if (node->definition && funct_state_summaries->exists (node))
	count++;
    }

  streamer_write_uhwi_stream (ob->main_stream, count);

  /* Process all of the functions.  */
  for (lsei = lsei_start_function_in_partition (encoder); !lsei_end_p (lsei);
       lsei_next_function_in_partition (&lsei))
    {
      node = lsei_cgraph_node (lsei);
      funct_state_d *fs = funct_state_summaries->get (node);
      if (node->definition && fs != NULL)
	{
	  struct bitpack_d bp;
	  int node_ref;
	  lto_symtab_encoder_t encoder;

	  encoder = ob->decl_state->symtab_node_encoder;
	  node_ref = lto_symtab_encoder_encode (encoder, node);
	  streamer_write_uhwi_stream (ob->main_stream, node_ref);

	  /* Note that flags will need to be read in the opposite
	     order as we are pushing the bitflags into FLAGS.  */
	  bp = bitpack_create (ob->main_stream);
	  bp_pack_value (&bp, fs->pure_const_state, 2);
	  bp_pack_value (&bp, fs->state_previously_known, 2);
	  bp_pack_value (&bp, fs->looping_previously_known, 1);
	  bp_pack_value (&bp, fs->looping, 1);
	  bp_pack_value (&bp, fs->can_throw, 1);
	  bp_pack_value (&bp, fs->can_free, 1);
	  bp_pack_value (&bp, fs->malloc_state, 2);
	  streamer_write_bitpack (&bp);
	}
    }

  lto_destroy_simple_output_block (ob);
}


/* Deserialize the ipa info for lto.  */

static void
pure_const_read_summary (void)
{
  struct lto_file_decl_data **file_data_vec = lto_get_file_decl_data ();
  struct lto_file_decl_data *file_data;
  unsigned int j = 0;

  pass_ipa_pure_const *pass = static_cast <pass_ipa_pure_const *> (current_pass);
  pass->register_hooks ();

  while ((file_data = file_data_vec[j++]))
    {
      const char *data;
      size_t len;
      class lto_input_block *ib
	= lto_create_simple_input_block (file_data,
					 LTO_section_ipa_pure_const,
					 &data, &len);
      if (ib)
	{
	  unsigned int i;
	  unsigned int count = streamer_read_uhwi (ib);

	  for (i = 0; i < count; i++)
	    {
	      unsigned int index;
	      struct cgraph_node *node;
	      struct bitpack_d bp;
	      funct_state fs;
	      lto_symtab_encoder_t encoder;

	      index = streamer_read_uhwi (ib);
	      encoder = file_data->symtab_node_encoder;
	      node = dyn_cast<cgraph_node *> (lto_symtab_encoder_deref (encoder,
									index));

	      fs = funct_state_summaries->get_create (node);
	      /* Note that the flags must be read in the opposite
		 order in which they were written (the bitflags were
		 pushed into FLAGS).  */
	      bp = streamer_read_bitpack (ib);
	      fs->pure_const_state
			= (enum pure_const_state_e) bp_unpack_value (&bp, 2);
	      fs->state_previously_known
			= (enum pure_const_state_e) bp_unpack_value (&bp, 2);
	      fs->looping_previously_known = bp_unpack_value (&bp, 1);
	      fs->looping = bp_unpack_value (&bp, 1);
	      fs->can_throw = bp_unpack_value (&bp, 1);
	      fs->can_free = bp_unpack_value (&bp, 1);
	      fs->malloc_state
			= (enum malloc_state_e) bp_unpack_value (&bp, 2);

	      if (dump_file)
		{
		  int flags = flags_from_decl_or_type (node->decl);
		  fprintf (dump_file, "Read info for %s ", node->dump_name ());
		  if (flags & ECF_CONST)
		    fprintf (dump_file, " const");
		  if (flags & ECF_PURE)
		    fprintf (dump_file, " pure");
		  if (flags & ECF_NOTHROW)
		    fprintf (dump_file, " nothrow");
		  fprintf (dump_file, "\n  pure const state: %s\n",
			   pure_const_names[fs->pure_const_state]);
		  fprintf (dump_file, "  previously known state: %s\n",
			   pure_const_names[fs->state_previously_known]);
		  if (fs->looping)
		    fprintf (dump_file,"  function is locally looping\n");
		  if (fs->looping_previously_known)
		    fprintf (dump_file,"  function is previously known looping\n");
		  if (fs->can_throw)
		    fprintf (dump_file,"  function is locally throwing\n");
		  if (fs->can_free)
		    fprintf (dump_file,"  function can locally free\n");
		  fprintf (dump_file, "\n malloc state: %s\n",
			   malloc_state_names[fs->malloc_state]);
		}
	    }

	  lto_destroy_simple_input_block (file_data,
					  LTO_section_ipa_pure_const,
					  ib, data, len);
	}
    }
}

/* We only propagate across edges that can throw externally and their callee
   is not interposable.  */

static bool
ignore_edge_for_nothrow (struct cgraph_edge *e)
{
  if (!e->can_throw_external || TREE_NOTHROW (e->callee->decl))
    return true;

  enum availability avail;
  cgraph_node *ultimate_target
    = e->callee->function_or_virtual_thunk_symbol (&avail, e->caller);
  if (avail <= AVAIL_INTERPOSABLE || TREE_NOTHROW (ultimate_target->decl))
    return true;
  return ((opt_for_fn (e->callee->decl, flag_non_call_exceptions)
	   && !e->callee->binds_to_current_def_p (e->caller))
	  || !opt_for_fn (e->caller->decl, flag_ipa_pure_const)
	  || !opt_for_fn (ultimate_target->decl, flag_ipa_pure_const));
}

/* Return true if NODE is self recursive function.
   Indirectly recursive functions appears as non-trivial strongly
   connected components, so we need to care about self recursion
   only.  */

static bool
self_recursive_p (struct cgraph_node *node)
{
  struct cgraph_edge *e;
  for (e = node->callees; e; e = e->next_callee)
    if (e->callee->function_symbol () == node)
      return true;
  return false;
}

/* Return true if N is cdtor that is not const or pure.  In this case we may
   need to remove unreachable function if it is marked const/pure.  */

static bool
cdtor_p (cgraph_node *n, void *)
{
  if (DECL_STATIC_CONSTRUCTOR (n->decl) || DECL_STATIC_DESTRUCTOR (n->decl))
    return ((!TREE_READONLY (n->decl) && !DECL_PURE_P (n->decl))
	    || DECL_LOOPING_CONST_OR_PURE_P (n->decl));
  return false;
}

/* Skip edges from and to nodes without ipa_pure_const enabled.
   Ignore not available symbols.  */

static bool
ignore_edge_for_pure_const (struct cgraph_edge *e)
{
  enum availability avail;
  cgraph_node *ultimate_target
    = e->callee->function_or_virtual_thunk_symbol (&avail, e->caller);

  return (avail <= AVAIL_INTERPOSABLE
	  || !opt_for_fn (e->caller->decl, flag_ipa_pure_const)
	  || !opt_for_fn (ultimate_target->decl,
			  flag_ipa_pure_const));
}

/* Produce transitive closure over the callgraph and compute pure/const
   attributes.  */

static bool
propagate_pure_const (void)
{
  struct cgraph_node *node;
  struct cgraph_node *w;
  struct cgraph_node **order =
    XCNEWVEC (struct cgraph_node *, symtab->cgraph_count);
  int order_pos;
  int i;
  struct ipa_dfs_info * w_info;
  bool remove_p = false;
  bool has_cdtor;

  order_pos = ipa_reduced_postorder (order, true,
				     ignore_edge_for_pure_const);
  if (dump_file)
    {
      cgraph_node::dump_cgraph (dump_file);
      ipa_print_order (dump_file, "reduced", order, order_pos);
    }

  /* Propagate the local information through the call graph to produce
     the global information.  All the nodes within a cycle will have
     the same info so we collapse cycles first.  Then we can do the
     propagation in one pass from the leaves to the roots.  */
  for (i = 0; i < order_pos; i++ )
    {
      enum pure_const_state_e pure_const_state = IPA_CONST;
      bool looping = false;
      int count = 0;
      node = order[i];

      if (node->alias)
	continue;

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Starting cycle\n");

      /* Find the worst state for any node in the cycle.  */
      w = node;
      while (w && pure_const_state != IPA_NEITHER)
	{
	  struct cgraph_edge *e;
	  struct cgraph_edge *ie;
	  int i;
	  struct ipa_ref *ref = NULL;

	  funct_state w_l = funct_state_summaries->get_create (w);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  Visiting %s state:%s looping %i\n",
		     w->dump_name (),
		     pure_const_names[w_l->pure_const_state],
		     w_l->looping);

	  /* First merge in function body properties.
	     We are safe to pass NULL as FROM and TO because we will take care
	     of possible interposition when walking callees.  */
	  worse_state (&pure_const_state, &looping,
		       w_l->pure_const_state, w_l->looping,
		       NULL, NULL);
	  if (pure_const_state == IPA_NEITHER)
	    break;

	  count++;

	  /* We consider recursive cycles as possibly infinite.
	     This might be relaxed since infinite recursion leads to stack
	     overflow.  */
	  if (count > 1)
	    looping = true;

	  /* Now walk the edges and merge in callee properties.  */
	  for (e = w->callees; e && pure_const_state != IPA_NEITHER;
	       e = e->next_callee)
	    {
	      enum availability avail;
	      struct cgraph_node *y = e->callee->
				function_or_virtual_thunk_symbol (&avail,
								  e->caller);
	      enum pure_const_state_e edge_state = IPA_CONST;
	      bool edge_looping = false;

	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "    Call to %s",
			   e->callee->dump_name ());
		}
	      if (avail > AVAIL_INTERPOSABLE)
		{
		  funct_state y_l = funct_state_summaries->get_create (y);

		  if (dump_file && (dump_flags & TDF_DETAILS))
		    {
		      fprintf (dump_file,
			       " state:%s looping:%i\n",
			       pure_const_names[y_l->pure_const_state],
			       y_l->looping);
		    }
		  if (y_l->pure_const_state > IPA_PURE
		      && e->cannot_lead_to_return_p ())
		    {
		      if (dump_file && (dump_flags & TDF_DETAILS))
			fprintf (dump_file,
				 "        Ignoring side effects"
				 " -> pure, looping\n");
		      edge_state = IPA_PURE;
		      edge_looping = true;
		    }
		  else
		    {
		      edge_state = y_l->pure_const_state;
		      edge_looping = y_l->looping;
		    }
		}
	      else if (special_builtin_state (&edge_state, &edge_looping,
					      y->decl))
		;
	      else
		state_from_flags (&edge_state, &edge_looping,
				  flags_from_decl_or_type (y->decl),
				  e->cannot_lead_to_return_p ());

	      /* Merge the results with what we already know.  */
	      better_state (&edge_state, &edge_looping,
			    w_l->state_previously_known,
			    w_l->looping_previously_known);
	      worse_state (&pure_const_state, &looping,
			   edge_state, edge_looping, e->caller, e->callee);
	      if (pure_const_state == IPA_NEITHER)
	        break;
	    }

	  /* Now process the indirect call.  */
          for (ie = w->indirect_calls;
	       ie && pure_const_state != IPA_NEITHER; ie = ie->next_callee)
	    {
	      enum pure_const_state_e edge_state = IPA_CONST;
	      bool edge_looping = false;

	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "    Indirect call");
	      state_from_flags (&edge_state, &edge_looping,
			        ie->indirect_info->ecf_flags,
				ie->cannot_lead_to_return_p ());
	      /* Merge the results with what we already know.  */
	      better_state (&edge_state, &edge_looping,
			    w_l->state_previously_known,
			    w_l->looping_previously_known);
	      worse_state (&pure_const_state, &looping,
			   edge_state, edge_looping, NULL, NULL);
	      if (pure_const_state == IPA_NEITHER)
	        break;
	    }

	  /* And finally all loads and stores.  */
	  for (i = 0; w->iterate_reference (i, ref)
	       && pure_const_state != IPA_NEITHER; i++)
	    {
	      enum pure_const_state_e ref_state = IPA_CONST;
	      bool ref_looping = false;
	      switch (ref->use)
		{
		case IPA_REF_LOAD:
		  /* readonly reads are safe.  */
		  if (TREE_READONLY (ref->referred->decl))
		    break;
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "    nonreadonly global var read\n");
		  ref_state = IPA_PURE;
		  break;
		case IPA_REF_STORE:
		  if (ref->cannot_lead_to_return ())
		    break;
		  ref_state = IPA_NEITHER;
		  if (dump_file && (dump_flags & TDF_DETAILS))
		    fprintf (dump_file, "    global var write\n");
		  break;
		case IPA_REF_ADDR:
		  break;
		default:
		  gcc_unreachable ();
		}
	      better_state (&ref_state, &ref_looping,
			    w_l->state_previously_known,
			    w_l->looping_previously_known);
	      worse_state (&pure_const_state, &looping,
			   ref_state, ref_looping, NULL, NULL);
	      if (pure_const_state == IPA_NEITHER)
		break;
	    }
	  w_info = (struct ipa_dfs_info *) w->aux;
	  w = w_info->next_cycle;
	}
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Result %s looping %i\n",
		 pure_const_names [pure_const_state],
		 looping);

      /* Find the worst state of can_free for any node in the cycle.  */
      bool can_free = false;
      w = node;
      while (w && !can_free)
	{
	  struct cgraph_edge *e;
	  funct_state w_l = funct_state_summaries->get (w);

	  if (w_l->can_free
	      || w->get_availability () == AVAIL_INTERPOSABLE
	      || w->indirect_calls)
	    can_free = true;

	  for (e = w->callees; e && !can_free; e = e->next_callee)
	    {
	      enum availability avail;
	      struct cgraph_node *y = e->callee->
				function_or_virtual_thunk_symbol (&avail,
								  e->caller);

	      if (avail > AVAIL_INTERPOSABLE)
		can_free = funct_state_summaries->get (y)->can_free;
	      else
		can_free = true;
	    }
	  w_info = (struct ipa_dfs_info *) w->aux;
	  w = w_info->next_cycle;
	}

      /* Copy back the region's pure_const_state which is shared by
	 all nodes in the region.  */
      w = node;
      while (w)
	{
	  funct_state w_l = funct_state_summaries->get (w);
	  enum pure_const_state_e this_state = pure_const_state;
	  bool this_looping = looping;

	  w_l->can_free = can_free;
	  w->nonfreeing_fn = !can_free;
	  if (!can_free && dump_file)
	    fprintf (dump_file, "Function found not to call free: %s\n",
		     w->dump_name ());

	  if (w_l->state_previously_known != IPA_NEITHER
	      && this_state > w_l->state_previously_known)
	    {
              this_state = w_l->state_previously_known;
	      if (this_state == IPA_NEITHER)
	        this_looping = w_l->looping_previously_known;
	    }
	  if (!this_looping && self_recursive_p (w))
	    this_looping = true;
	  if (!w_l->looping_previously_known)
	    this_looping = false;

	  /* All nodes within a cycle share the same info.  */
	  w_l->pure_const_state = this_state;
	  w_l->looping = this_looping;

	  /* Inline clones share declaration with their offline copies;
	     do not modify their declarations since the offline copy may
	     be different.  */
	  if (!w->inlined_to)
	    switch (this_state)
	      {
	      case IPA_CONST:
		if (!TREE_READONLY (w->decl))
		  {
		    warn_function_const (w->decl, !this_looping);
		    if (dump_file)
		      fprintf (dump_file, "Function found to be %sconst: %s\n",
			       this_looping ? "looping " : "",
			       w->dump_name ());
		  }
		/* Turning constructor or destructor to non-looping const/pure
		   enables us to possibly remove the function completely.  */
		if (this_looping)
		  has_cdtor = false;
		else
		  has_cdtor = w->call_for_symbol_and_aliases (cdtor_p,
							      NULL, true);
		if (w->set_const_flag (true, this_looping))
		  {
		    if (dump_file)
		      fprintf (dump_file,
			       "Declaration updated to be %sconst: %s\n",
			       this_looping ? "looping " : "",
			       w->dump_name ());
		    remove_p |= has_cdtor;
		  }
		break;

	      case IPA_PURE:
		if (!DECL_PURE_P (w->decl))
		  {
		    warn_function_pure (w->decl, !this_looping);
		    if (dump_file)
		      fprintf (dump_file, "Function found to be %spure: %s\n",
			       this_looping ? "looping " : "",
			       w->dump_name ());
		  }
		if (this_looping)
		  has_cdtor = false;
		else
		  has_cdtor = w->call_for_symbol_and_aliases (cdtor_p,
							      NULL, true);
		if (w->set_pure_flag (true, this_looping))
		  {
		    if (dump_file)
		      fprintf (dump_file,
			       "Declaration updated to be %spure: %s\n",
			       this_looping ? "looping " : "",
			       w->dump_name ());
		    remove_p |= has_cdtor;
		  }
		break;

	      default:
		break;
	      }
	  w_info = (struct ipa_dfs_info *) w->aux;
	  w = w_info->next_cycle;
	}
    }

  ipa_free_postorder_info ();
  free (order);
  return remove_p;
}

/* Produce transitive closure over the callgraph and compute nothrow
   attributes.  */

static void
propagate_nothrow (void)
{
  struct cgraph_node *node;
  struct cgraph_node *w;
  struct cgraph_node **order =
    XCNEWVEC (struct cgraph_node *, symtab->cgraph_count);
  int order_pos;
  int i;
  struct ipa_dfs_info * w_info;

  order_pos = ipa_reduced_postorder (order, true,
				     ignore_edge_for_nothrow);
  if (dump_file)
    {
      cgraph_node::dump_cgraph (dump_file);
      ipa_print_order (dump_file, "reduced for nothrow", order, order_pos);
    }

  /* Propagate the local information through the call graph to produce
     the global information.  All the nodes within a cycle will have
     the same info so we collapse cycles first.  Then we can do the
     propagation in one pass from the leaves to the roots.  */
  for (i = 0; i < order_pos; i++ )
    {
      bool can_throw = false;
      node = order[i];

      if (node->alias)
	continue;

      /* Find the worst state for any node in the cycle.  */
      w = node;
      while (w && !can_throw)
	{
	  struct cgraph_edge *e, *ie;

	  if (!TREE_NOTHROW (w->decl))
	    {
	      funct_state w_l = funct_state_summaries->get_create (w);

	      if (w_l->can_throw
		  || w->get_availability () == AVAIL_INTERPOSABLE)
		can_throw = true;

	      for (e = w->callees; e && !can_throw; e = e->next_callee)
		{
		  enum availability avail;

		  if (!e->can_throw_external || TREE_NOTHROW (e->callee->decl))
		    continue;

		  struct cgraph_node *y = e->callee->
				   function_or_virtual_thunk_symbol (&avail,
								     e->caller);

		  /* We can use info about the callee only if we know it
		     cannot be interposed.
		     When callee is compiled with non-call exceptions we also
		     must check that the declaration is bound to current
		     body as other semantically equivalent body may still
		     throw.  */
		  if (avail <= AVAIL_INTERPOSABLE
		      || (!TREE_NOTHROW (y->decl)
			  && (funct_state_summaries->get_create (y)->can_throw
			      || (opt_for_fn (y->decl, flag_non_call_exceptions)
				  && !e->callee->binds_to_current_def_p (w)))))
		    can_throw = true;
		}
	      for (ie = w->indirect_calls; ie && !can_throw;
		   ie = ie->next_callee)
		if (ie->can_throw_external
		    && !(ie->indirect_info->ecf_flags & ECF_NOTHROW))
		  can_throw = true;
	    }
	  w_info = (struct ipa_dfs_info *) w->aux;
	  w = w_info->next_cycle;
	}

      /* Copy back the region's pure_const_state which is shared by
	 all nodes in the region.  */
      w = node;
      while (w)
	{
	  funct_state w_l = funct_state_summaries->get_create (w);
	  if (!can_throw && !TREE_NOTHROW (w->decl))
	    {
	      /* Inline clones share declaration with their offline copies;
		 do not modify their declarations since the offline copy may
		 be different.  */
	      if (!w->inlined_to)
		{
		  w->set_nothrow_flag (true);
		  if (dump_file)
		    fprintf (dump_file, "Function found to be nothrow: %s\n",
			     w->dump_name ());
		}
	    }
	  else if (can_throw && !TREE_NOTHROW (w->decl))
	    w_l->can_throw = true;
	  w_info = (struct ipa_dfs_info *) w->aux;
	  w = w_info->next_cycle;
	}
    }

  ipa_free_postorder_info ();
  free (order);
}

/* Debugging function to dump state of malloc lattice.  */

DEBUG_FUNCTION
static void
dump_malloc_lattice (FILE *dump_file, const char *s)
{
  if (!dump_file)
    return;

  fprintf (dump_file, "\n\nMALLOC LATTICE %s:\n", s);
  cgraph_node *node;
  FOR_EACH_FUNCTION (node)
    {
      funct_state fs = funct_state_summaries->get (node);
      if (fs)
	fprintf (dump_file, "%s: %s\n", node->dump_name (),
		 malloc_state_names[fs->malloc_state]);
    }
}

/* Propagate malloc attribute across the callgraph.  */

static void
propagate_malloc (void)
{
  cgraph_node *node;
  FOR_EACH_FUNCTION (node)
    {
      if (DECL_IS_MALLOC (node->decl))
	if (!funct_state_summaries->exists (node))
	  {
	    funct_state fs = funct_state_summaries->get_create (node);
	    fs->malloc_state = STATE_MALLOC;
	  }
    }

  dump_malloc_lattice (dump_file, "Initial");
  struct cgraph_node **order
    = XNEWVEC (struct cgraph_node *, symtab->cgraph_count);
  int order_pos = ipa_reverse_postorder (order);
  bool changed = true;

  while (changed)
    {
      changed = false;
      /* Walk in postorder.  */
      for (int i = order_pos - 1; i >= 0; --i)
	{
	  cgraph_node *node = order[i];
	  if (node->alias
	      || !node->definition
	      || !funct_state_summaries->exists (node))
	    continue;

	  funct_state l = funct_state_summaries->get (node);

	  /* FIXME: add support for indirect-calls.  */
	  if (node->indirect_calls)
	    {
	      l->malloc_state = STATE_MALLOC_BOTTOM;
	      continue;
	    }

	  if (node->get_availability () <= AVAIL_INTERPOSABLE)
	    {
	      l->malloc_state = STATE_MALLOC_BOTTOM;
	      continue;
	    }

	  if (l->malloc_state == STATE_MALLOC_BOTTOM)
	    continue;

	  vec<cgraph_node *> callees = vNULL;
	  for (cgraph_edge *cs = node->callees; cs; cs = cs->next_callee)
	    {
	      ipa_call_summary *es = ipa_call_summaries->get_create (cs);
	      if (es && es->is_return_callee_uncaptured)
		callees.safe_push (cs->callee);
	    }

	  malloc_state_e new_state = l->malloc_state;
	  for (unsigned j = 0; j < callees.length (); j++)
	    {
	      cgraph_node *callee = callees[j];
	      if (!funct_state_summaries->exists (node))
		{
		  new_state = STATE_MALLOC_BOTTOM;
		  break;
		}
	      malloc_state_e callee_state
		= funct_state_summaries->get_create (callee)->malloc_state;
	      if (new_state < callee_state)
		new_state = callee_state;
	    }
	  if (new_state != l->malloc_state)
	    {
	      changed = true;
	      l->malloc_state = new_state;
	    }
	}
    }

  FOR_EACH_DEFINED_FUNCTION (node)
    if (funct_state_summaries->exists (node))
      {
	funct_state l = funct_state_summaries->get (node);
	if (!node->alias
	    && l->malloc_state == STATE_MALLOC
	    && !node->inlined_to)
	  {
	    if (dump_file && (dump_flags & TDF_DETAILS))
	      fprintf (dump_file, "Function %s found to be malloc\n",
		       node->dump_name ());

	    bool malloc_decl_p = DECL_IS_MALLOC (node->decl);
	    node->set_malloc_flag (true);
	    if (!malloc_decl_p && warn_suggest_attribute_malloc)
		warn_function_malloc (node->decl);
	  }
      }

  dump_malloc_lattice (dump_file, "after propagation");
  ipa_free_postorder_info ();
  free (order);
}

/* Produce the global information by preforming a transitive closure
   on the local information that was produced by generate_summary.  */

unsigned int
pass_ipa_pure_const::
execute (function *)
{
  bool remove_p;

  /* Nothrow makes more function to not lead to return and improve
     later analysis.  */
  propagate_nothrow ();
  propagate_malloc ();
  remove_p = propagate_pure_const ();

  delete funct_state_summaries;
  return remove_p ? TODO_remove_functions : 0;
}

static bool
gate_pure_const (void)
{
  return flag_ipa_pure_const || in_lto_p;
}

pass_ipa_pure_const::pass_ipa_pure_const(gcc::context *ctxt)
    : ipa_opt_pass_d(pass_data_ipa_pure_const, ctxt,
		     pure_const_generate_summary, /* generate_summary */
		     pure_const_write_summary, /* write_summary */
		     pure_const_read_summary, /* read_summary */
		     NULL, /* write_optimization_summary */
		     NULL, /* read_optimization_summary */
		     NULL, /* stmt_fixup */
		     0, /* function_transform_todo_flags_start */
		     NULL, /* function_transform */
		     NULL), /* variable_transform */
  init_p (false) {}

ipa_opt_pass_d *
make_pass_ipa_pure_const (gcc::context *ctxt)
{
  return new pass_ipa_pure_const (ctxt);
}

/* Return true if function should be skipped for local pure const analysis.  */

static bool
skip_function_for_local_pure_const (struct cgraph_node *node)
{
  /* Because we do not schedule pass_fixup_cfg over whole program after early
     optimizations we must not promote functions that are called by already
     processed functions.  */

  if (function_called_by_processed_nodes_p ())
    {
      if (dump_file)
        fprintf (dump_file, "Function called in recursive cycle; ignoring\n");
      return true;
    }
  /* Save some work and do not analyze functions which are interposable and
     do not have any non-interposable aliases.  */
  if (node->get_availability () <= AVAIL_INTERPOSABLE
      && !node->has_aliases_p ())
    {
      if (dump_file)
        fprintf (dump_file,
		 "Function is interposable; not analyzing.\n");
      return true;
    }
  return false;
}

/* Simple local pass for pure const discovery reusing the analysis from
   ipa_pure_const.   This pass is effective when executed together with
   other optimization passes in early optimization pass queue.  */

namespace {

const pass_data pass_data_local_pure_const =
{
  GIMPLE_PASS, /* type */
  "local-pure-const", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_IPA_PURE_CONST, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_local_pure_const : public gimple_opt_pass
{
public:
  pass_local_pure_const (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_local_pure_const, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_local_pure_const (m_ctxt); }
  virtual bool gate (function *) { return gate_pure_const (); }
  virtual unsigned int execute (function *);

}; // class pass_local_pure_const

unsigned int
pass_local_pure_const::execute (function *fun)
{
  bool changed = false;
  funct_state l;
  bool skip;
  struct cgraph_node *node;

  node = cgraph_node::get (current_function_decl);
  skip = skip_function_for_local_pure_const (node);

  if (!warn_suggest_attribute_const
      && !warn_suggest_attribute_pure
      && skip)
    return 0;

  l = analyze_function (node, false);

  /* Do NORETURN discovery.  */
  if (!skip && !TREE_THIS_VOLATILE (current_function_decl)
      && EDGE_COUNT (EXIT_BLOCK_PTR_FOR_FN (fun)->preds) == 0)
    {
      warn_function_noreturn (fun->decl);
      if (dump_file)
	fprintf (dump_file, "Function found to be noreturn: %s\n",
		 current_function_name ());

      /* Update declaration and reduce profile to executed once.  */
      TREE_THIS_VOLATILE (current_function_decl) = 1;
      if (node->frequency > NODE_FREQUENCY_EXECUTED_ONCE)
	node->frequency = NODE_FREQUENCY_EXECUTED_ONCE;

      changed = true;
    }

  switch (l->pure_const_state)
    {
    case IPA_CONST:
      if (!TREE_READONLY (current_function_decl))
	{
	  warn_function_const (current_function_decl, !l->looping);
	  if (dump_file)
	    fprintf (dump_file, "Function found to be %sconst: %s\n",
		     l->looping ? "looping " : "",
		     current_function_name ());
	}
      else if (DECL_LOOPING_CONST_OR_PURE_P (current_function_decl)
	       && !l->looping)
	{
	  if (dump_file)
	    fprintf (dump_file, "Function found to be non-looping: %s\n",
		     current_function_name ());
	}
      if (!skip && node->set_const_flag (true, l->looping))
	{
	  if (dump_file)
	    fprintf (dump_file, "Declaration updated to be %sconst: %s\n",
		     l->looping ? "looping " : "",
		     current_function_name ());
	  changed = true;
	}
      break;

    case IPA_PURE:
      if (!DECL_PURE_P (current_function_decl))
	{
	  warn_function_pure (current_function_decl, !l->looping);
	  if (dump_file)
	    fprintf (dump_file, "Function found to be %spure: %s\n",
		     l->looping ? "looping " : "",
		     current_function_name ());
	}
      else if (DECL_LOOPING_CONST_OR_PURE_P (current_function_decl)
	       && !l->looping)
	{
	  if (dump_file)
	    fprintf (dump_file, "Function found to be non-looping: %s\n",
		     current_function_name ());
	}
      if (!skip && node->set_pure_flag (true, l->looping))
	{
	  if (dump_file)
	    fprintf (dump_file, "Declaration updated to be %spure: %s\n",
		     l->looping ? "looping " : "",
		     current_function_name ());
	  changed = true;
	}
      break;

    default:
      break;
    }
  if (!l->can_throw && !TREE_NOTHROW (current_function_decl))
    {
      node->set_nothrow_flag (true);
      changed = true;
      if (dump_file)
	fprintf (dump_file, "Function found to be nothrow: %s\n",
		 current_function_name ());
    }

  if (l->malloc_state == STATE_MALLOC
      && !DECL_IS_MALLOC (current_function_decl))
    {
      node->set_malloc_flag (true);
      if (warn_suggest_attribute_malloc)
	warn_function_malloc (node->decl);
      changed = true;
      if (dump_file)
	fprintf (dump_file, "Function found to be malloc: %s\n",
		 node->dump_name ());
    }

  free (l);
  if (changed)
    return execute_fixup_cfg ();
  else
    return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_local_pure_const (gcc::context *ctxt)
{
  return new pass_local_pure_const (ctxt);
}

/* Emit noreturn warnings.  */

namespace {

const pass_data pass_data_warn_function_noreturn =
{
  GIMPLE_PASS, /* type */
  "*warn_function_noreturn", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_cfg, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_warn_function_noreturn : public gimple_opt_pass
{
public:
  pass_warn_function_noreturn (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_warn_function_noreturn, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *) { return warn_suggest_attribute_noreturn; }
  virtual unsigned int execute (function *fun)
    {
      if (!TREE_THIS_VOLATILE (current_function_decl)
	  && EDGE_COUNT (EXIT_BLOCK_PTR_FOR_FN (fun)->preds) == 0)
	warn_function_noreturn (current_function_decl);
      return 0;
    }

}; // class pass_warn_function_noreturn

} // anon namespace

gimple_opt_pass *
make_pass_warn_function_noreturn (gcc::context *ctxt)
{
  return new pass_warn_function_noreturn (ctxt);
}

/* Simple local pass for pure const discovery reusing the analysis from
   ipa_pure_const.   This pass is effective when executed together with
   other optimization passes in early optimization pass queue.  */

namespace {

const pass_data pass_data_nothrow =
{
  GIMPLE_PASS, /* type */
  "nothrow", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_IPA_PURE_CONST, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_nothrow : public gimple_opt_pass
{
public:
  pass_nothrow (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_nothrow, ctxt)
  {}

  /* opt_pass methods: */
  opt_pass * clone () { return new pass_nothrow (m_ctxt); }
  virtual bool gate (function *) { return optimize; }
  virtual unsigned int execute (function *);

}; // class pass_nothrow

unsigned int
pass_nothrow::execute (function *)
{
  struct cgraph_node *node;
  basic_block this_block;

  if (TREE_NOTHROW (current_function_decl))
    return 0;

  node = cgraph_node::get (current_function_decl);

  /* We run during lowering, we cannot really use availability yet.  */
  if (cgraph_node::get (current_function_decl)->get_availability ()
      <= AVAIL_INTERPOSABLE)
    {
      if (dump_file)
        fprintf (dump_file, "Function is interposable;"
	         " not analyzing.\n");
      return true;
    }

  FOR_EACH_BB_FN (this_block, cfun)
    {
      for (gimple_stmt_iterator gsi = gsi_start_bb (this_block);
	   !gsi_end_p (gsi);
	   gsi_next (&gsi))
        if (stmt_can_throw_external (cfun, gsi_stmt (gsi)))
	  {
	    if (is_gimple_call (gsi_stmt (gsi)))
	      {
		tree callee_t = gimple_call_fndecl (gsi_stmt (gsi));
		if (callee_t && recursive_call_p (current_function_decl,
						  callee_t))
		  continue;
	      }
	
	    if (dump_file)
	      {
		fprintf (dump_file, "Statement can throw: ");
		print_gimple_stmt (dump_file, gsi_stmt (gsi), 0);
	      }
	    return 0;
	  }
    }

  node->set_nothrow_flag (true);

  bool cfg_changed = false;
  if (self_recursive_p (node))
    FOR_EACH_BB_FN (this_block, cfun)
      if (gimple *g = last_stmt (this_block))
	if (is_gimple_call (g))
	  {
	    tree callee_t = gimple_call_fndecl (g);
	    if (callee_t
		&& recursive_call_p (current_function_decl, callee_t)
		&& maybe_clean_eh_stmt (g)
		&& gimple_purge_dead_eh_edges (this_block))
	      cfg_changed = true;
	  }

  if (dump_file)
    fprintf (dump_file, "Function found to be nothrow: %s\n",
	     current_function_name ());
  return cfg_changed ? TODO_cleanup_cfg : 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_nothrow (gcc::context *ctxt)
{
  return new pass_nothrow (ctxt);
}
