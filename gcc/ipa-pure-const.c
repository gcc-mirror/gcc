/* Callgraph based analysis of static variables.
   Copyright (C) 2004-2015 Free Software Foundation, Inc.
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
#include "tm.h"
#include "alias.h"
#include "symtab.h"
#include "tree.h"
#include "fold-const.h"
#include "print-tree.h"
#include "calls.h"
#include "predict.h"
#include "hard-reg-set.h"
#include "function.h"
#include "dominance.h"
#include "cfg.h"
#include "cfganal.h"
#include "basic-block.h"
#include "tree-ssa-alias.h"
#include "internal-fn.h"
#include "tree-eh.h"
#include "gimple-expr.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "tree-cfg.h"
#include "tree-ssa-loop-niter.h"
#include "tree-inline.h"
#include "tree-pass.h"
#include "langhooks.h"
#include "cgraph.h"
#include "ipa-utils.h"
#include "flags.h"
#include "diagnostic.h"
#include "gimple-pretty-print.h"
#include "langhooks.h"
#include "target.h"
#include "lto-streamer.h"
#include "data-streamer.h"
#include "tree-streamer.h"
#include "cfgloop.h"
#include "tree-scalar-evolution.h"
#include "intl.h"
#include "opts.h"
#include "varasm.h"

/* Lattice values for const and pure functions.  Everything starts out
   being const, then may drop to pure and then neither depending on
   what is found.  */
enum pure_const_state_e
{
  IPA_CONST,
  IPA_PURE,
  IPA_NEITHER
};

const char *pure_const_names[3] = {"const", "pure", "neither"};

/* Holder for the const_state.  There is one of these per function
   decl.  */
struct funct_state_d
{
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
};

/* State used when we know nothing about function.  */
static struct funct_state_d varying_state
   = { IPA_NEITHER, IPA_NEITHER, true, true, true, true };


typedef struct funct_state_d * funct_state;

/* The storage of the funct_state is abstracted because there is the
   possibility that it may be desirable to move this to the cgraph
   local info.  */

/* Array, indexed by cgraph node uid, of function states.  */

static vec<funct_state> funct_state_vec;

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

  /* Holders of ipa cgraph hooks: */
  struct cgraph_node_hook_list *function_insertion_hook_holder;
  struct cgraph_2node_hook_list *node_duplication_hook_holder;
  struct cgraph_node_hook_list *node_removal_hook_holder;

}; // class pass_ipa_pure_const

} // anon namespace

/* Try to guess if function body will always be visible to compiler
   when compiling the call and whether compiler will be able
   to propagate the information by itself.  */

static bool
function_always_visible_to_compiler_p (tree decl)
{
  return (!TREE_PUBLIC (decl) || DECL_DECLARED_INLINE_P (decl));
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
  if (!option_enabled (option, &global_options))
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
	      ? _("function might be candidate for attribute %<%s%>")
	      : _("function might be candidate for attribute %<%s%>"
		  " if it is known to return normally"), attrib_name);
  return warned_about;
}

/* Emit suggestion about __attribute_((pure)) for DECL.  KNOWN_FINITE
   is true if the function is known to be finite.  */

static void
warn_function_pure (tree decl, bool known_finite)
{
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
  static hash_set<tree> *warned_about;
  warned_about 
    = suggest_attribute (OPT_Wsuggest_attribute_const, decl,
			 known_finite, warned_about, "const");
}

static void
warn_function_noreturn (tree decl)
{
  static hash_set<tree> *warned_about;
  if (!lang_hooks.missing_noreturn_ok_p (decl)
      && targetm.warn_func_return (decl))
    warned_about 
      = suggest_attribute (OPT_Wsuggest_attribute_noreturn, decl,
			   true, warned_about, "noreturn");
}

/* Return true if we have a function state for NODE.  */

static inline bool
has_function_state (struct cgraph_node *node)
{
  if (!funct_state_vec.exists ()
      || funct_state_vec.length () <= (unsigned int)node->uid)
    return false;
  return funct_state_vec[node->uid] != NULL;
}

/* Return the function state from NODE.  */

static inline funct_state
get_function_state (struct cgraph_node *node)
{
  if (!funct_state_vec.exists ()
      || funct_state_vec.length () <= (unsigned int)node->uid
      || !funct_state_vec[node->uid])
    /* We might want to put correct previously_known state into varying.  */
    return &varying_state;
 return funct_state_vec[node->uid];
}

/* Set the function state S for NODE.  */

static inline void
set_function_state (struct cgraph_node *node, funct_state s)
{
  if (!funct_state_vec.exists ()
      || funct_state_vec.length () <= (unsigned int)node->uid)
     funct_state_vec.safe_grow_cleared (node->uid + 1);
  funct_state_vec[node->uid] = s;
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
        fprintf (dump_file, "    Volatile operand is not const/pure");
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
      if (TREE_READONLY (t) && !TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (t)))
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
	fprintf (dump_file, " looping");
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
   into STATE and LOOPING worse of the two variants.  */

static inline void
worse_state (enum pure_const_state_e *state, bool *looping,
	     enum pure_const_state_e state2, bool looping2)
{
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
	case BUILT_IN_ALLOCA:
	case BUILT_IN_ALLOCA_WITH_ALIGN:
	case BUILT_IN_STACK_SAVE:
	case BUILT_IN_STACK_RESTORE:
	case BUILT_IN_EH_POINTER:
	case BUILT_IN_EH_FILTER:
	case BUILT_IN_UNWIND_RESUME:
	case BUILT_IN_CXA_END_CLEANUP:
	case BUILT_IN_EH_COPY_VALUES:
	case BUILT_IN_FRAME_ADDRESS:
	case BUILT_IN_APPLY:
	case BUILT_IN_APPLY_ARGS:
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
  bool possibly_throws = stmt_could_throw_p (call);
  bool possibly_throws_externally = (possibly_throws
  				     && stmt_can_throw_external (call));

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
		       call_state, call_looping);
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
	      fprintf (dump_file, "    longjmp and nonlocal goto is not const/pure\n");
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
     those bits. */
  else if (!ipa)
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
		   call_state, call_looping);
    }
  /* Direct functions calls are handled by IPA propagation.  */
}

/* Wrapper around check_decl for loads in local more.  */

static bool
check_load (gimple, tree op, tree, void *data)
{
  if (DECL_P (op))
    check_decl ((funct_state)data, op, false, false);
  else
    check_op ((funct_state)data, op, false);
  return false;
}

/* Wrapper around check_decl for stores in local more.  */

static bool
check_store (gimple, tree op, tree, void *data)
{
  if (DECL_P (op))
    check_decl ((funct_state)data, op, true, false);
  else
    check_op ((funct_state)data, op, true);
  return false;
}

/* Wrapper around check_decl for loads in ipa mode.  */

static bool
check_ipa_load (gimple, tree op, tree, void *data)
{
  if (DECL_P (op))
    check_decl ((funct_state)data, op, false, true);
  else
    check_op ((funct_state)data, op, false);
  return false;
}

/* Wrapper around check_decl for stores in ipa mode.  */

static bool
check_ipa_store (gimple, tree op, tree, void *data)
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
  gimple stmt = gsi_stmt (*gsip);

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
      print_gimple_stmt (dump_file, stmt, 0, 0);
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
      && stmt_could_throw_p (stmt))
    {
      if (cfun->can_throw_non_call_exceptions)
	{
	  if (dump_file)
	    fprintf (dump_file, "    can throw; looping\n");
	  local->looping = true;
	}
      if (stmt_can_throw_external (stmt))
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


/* This is the main routine for finding the reference patterns for
   global variables within a function FN.  */

static funct_state
analyze_function (struct cgraph_node *fn, bool ipa)
{
  tree decl = fn->decl;
  funct_state l;
  basic_block this_block;

  l = XCNEW (struct funct_state_d);
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
	       fn->name ());
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
	      struct loop *loop;
	      scev_initialize ();
	      FOR_EACH_LOOP (loop, 0)
		if (!finite_loop_p (loop))
		  {
		    if (dump_file)
		      fprintf (dump_file, "    can not prove finiteness of "
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
    }
  return l;
}

/* Called when new function is inserted to callgraph late.  */
static void
add_new_function (struct cgraph_node *node, void *data ATTRIBUTE_UNUSED)
{
 if (node->get_availability () < AVAIL_INTERPOSABLE)
   return;
  /* There are some shared nodes, in particular the initializers on
     static declarations.  We do not need to scan them more than once
     since all we would be interested in are the addressof
     operations.  */
  if (node->get_availability () > AVAIL_INTERPOSABLE
      && opt_for_fn (node->decl, flag_ipa_pure_const))
    set_function_state (node, analyze_function (node, true));
}

/* Called when new clone is inserted to callgraph late.  */

static void
duplicate_node_data (struct cgraph_node *src, struct cgraph_node *dst,
	 	     void *data ATTRIBUTE_UNUSED)
{
  if (has_function_state (src))
    {
      funct_state l = XNEW (struct funct_state_d);
      gcc_assert (!has_function_state (dst));
      memcpy (l, get_function_state (src), sizeof (*l));
      set_function_state (dst, l);
    }
}

/* Called when new clone is inserted to callgraph late.  */

static void
remove_node_data (struct cgraph_node *node, void *data ATTRIBUTE_UNUSED)
{
  if (has_function_state (node))
    {
      funct_state l = get_function_state (node);
      if (l != &varying_state)
        free (l);
      set_function_state (node, NULL);
    }
}


void
pass_ipa_pure_const::
register_hooks (void)
{
  if (init_p)
    return;

  init_p = true;

  node_removal_hook_holder =
      symtab->add_cgraph_removal_hook (&remove_node_data, NULL);
  node_duplication_hook_holder =
      symtab->add_cgraph_duplication_hook (&duplicate_node_data, NULL);
  function_insertion_hook_holder =
      symtab->add_cgraph_insertion_hook (&add_new_function, NULL);
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

     We process AVAIL_INTERPOSABLE functions.  We can not use the results
     by default, but the info can be used at LTO with -fwhole-program or
     when function got cloned and the clone is AVAILABLE.  */

  FOR_EACH_DEFINED_FUNCTION (node)
    if (node->get_availability () >= AVAIL_INTERPOSABLE
        && opt_for_fn (node->decl, flag_ipa_pure_const))
      set_function_state (node, analyze_function (node, true));
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
      if (node->definition && has_function_state (node))
	count++;
    }

  streamer_write_uhwi_stream (ob->main_stream, count);

  /* Process all of the functions.  */
  for (lsei = lsei_start_function_in_partition (encoder); !lsei_end_p (lsei);
       lsei_next_function_in_partition (&lsei))
    {
      node = lsei_cgraph_node (lsei);
      if (node->definition && has_function_state (node))
	{
	  struct bitpack_d bp;
	  funct_state fs;
	  int node_ref;
	  lto_symtab_encoder_t encoder;

	  fs = get_function_state (node);

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
      struct lto_input_block *ib
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

	      fs = XCNEW (struct funct_state_d);
	      index = streamer_read_uhwi (ib);
	      encoder = file_data->symtab_node_encoder;
	      node = dyn_cast<cgraph_node *> (lto_symtab_encoder_deref (encoder,
									index));
	      set_function_state (node, fs);

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
	      if (dump_file)
		{
		  int flags = flags_from_decl_or_type (node->decl);
		  fprintf (dump_file, "Read info for %s/%i ",
			   node->name (),
			   node->order);
		  if (flags & ECF_CONST)
		    fprintf (dump_file, " const");
		  if (flags & ECF_PURE)
		    fprintf (dump_file, " pure");
		  if (flags & ECF_NOTHROW)
		    fprintf (dump_file, " nothrow");
		  fprintf (dump_file, "\n  pure const state: %s\n",
			   pure_const_names[fs->pure_const_state]);
		  fprintf (dump_file, "  previously known state: %s\n",
			   pure_const_names[fs->looping_previously_known]);
		  if (fs->looping)
		    fprintf (dump_file,"  function is locally looping\n");
		  if (fs->looping_previously_known)
		    fprintf (dump_file,"  function is previously known looping\n");
		  if (fs->can_throw)
		    fprintf (dump_file,"  function is locally throwing\n");
		  if (fs->can_free)
		    fprintf (dump_file,"  function can locally free\n");
		}
	    }

	  lto_destroy_simple_input_block (file_data,
					  LTO_section_ipa_pure_const,
					  ib, data, len);
	}
    }
}


static bool
ignore_edge (struct cgraph_edge *e)
{
  return (!e->can_throw_external);
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
    return !TREE_READONLY (n->decl) && !DECL_PURE_P (n->decl);
  return false;
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

  order_pos = ipa_reduced_postorder (order, true, false, NULL);
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

	  funct_state w_l = get_function_state (w);
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file, "  Visiting %s/%i state:%s looping %i\n",
		     w->name (),
		     w->order,
		     pure_const_names[w_l->pure_const_state],
		     w_l->looping);

	  /* First merge in function body properties.  */
	  worse_state (&pure_const_state, &looping,
		       w_l->pure_const_state, w_l->looping);
	  if (pure_const_state == IPA_NEITHER)
	    break;

	  /* For overwritable nodes we can not assume anything.  */
	  if (w->get_availability () == AVAIL_INTERPOSABLE)
	    {
	      worse_state (&pure_const_state, &looping,
			   w_l->state_previously_known,
			   w_l->looping_previously_known);
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file,
			   "    Overwritable. state %s looping %i\n",
			   pure_const_names[w_l->state_previously_known],
			   w_l->looping_previously_known);
		}
	      break;
	    }

	  count++;

	  /* We consider recursive cycles as possibly infinite.
	     This might be relaxed since infinite recursion leads to stack
	     overflow.  */
	  if (count > 1)
	    looping = true;

	  /* Now walk the edges and merge in callee properties.  */
	  for (e = w->callees; e; e = e->next_callee)
	    {
	      enum availability avail;
	      struct cgraph_node *y = e->callee->
				function_or_virtual_thunk_symbol (&avail);
	      enum pure_const_state_e edge_state = IPA_CONST;
	      bool edge_looping = false;

	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file,
			   "    Call to %s/%i",
			   e->callee->name (),
			   e->callee->order);
		}
	      if (avail > AVAIL_INTERPOSABLE)
		{
		  funct_state y_l = get_function_state (y);
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
			   edge_state, edge_looping);
	      if (pure_const_state == IPA_NEITHER)
	        break;
	    }
	  if (pure_const_state == IPA_NEITHER)
	    break;

	  /* Now process the indirect call.  */
          for (ie = w->indirect_calls; ie; ie = ie->next_callee)
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
			   edge_state, edge_looping);
	      if (pure_const_state == IPA_NEITHER)
	        break;
	    }
	  if (pure_const_state == IPA_NEITHER)
	    break;

	  /* And finally all loads and stores.  */
	  for (i = 0; w->iterate_reference (i, ref); i++)
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
		case IPA_REF_CHKP:
		  break;
		default:
		  gcc_unreachable ();
		}
	      better_state (&ref_state, &ref_looping,
			    w_l->state_previously_known,
			    w_l->looping_previously_known);
	      worse_state (&pure_const_state, &looping,
			   ref_state, ref_looping);
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
	  funct_state w_l = get_function_state (w);

	  if (w_l->can_free
	      || w->get_availability () == AVAIL_INTERPOSABLE
	      || w->indirect_calls)
	    can_free = true;

	  for (e = w->callees; e && !can_free; e = e->next_callee)
	    {
	      enum availability avail;
	      struct cgraph_node *y = e->callee->
				function_or_virtual_thunk_symbol (&avail);

	      if (avail > AVAIL_INTERPOSABLE)
		can_free = get_function_state (y)->can_free;
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
	  funct_state w_l = get_function_state (w);
	  enum pure_const_state_e this_state = pure_const_state;
	  bool this_looping = looping;

	  w_l->can_free = can_free;
	  w->nonfreeing_fn = !can_free;
	  if (!can_free && dump_file)
	    fprintf (dump_file, "Function found not to call free: %s\n",
		     w->name ());

	  if (w_l->state_previously_known != IPA_NEITHER
	      && this_state > w_l->state_previously_known)
	    {
              this_state = w_l->state_previously_known;
	      this_looping |= w_l->looping_previously_known;
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
	  if (!w->global.inlined_to)
	    switch (this_state)
	      {
	      case IPA_CONST:
		if (!TREE_READONLY (w->decl))
		  {
		    warn_function_const (w->decl, !this_looping);
		    if (dump_file)
		      fprintf (dump_file, "Function found to be %sconst: %s\n",
			       this_looping ? "looping " : "",
			       w->name ());
		  }
		remove_p |= w->call_for_symbol_and_aliases (cdtor_p,
							    NULL, true);
		w->set_const_flag (true, this_looping);
		break;

	      case IPA_PURE:
		if (!DECL_PURE_P (w->decl))
		  {
		    warn_function_pure (w->decl, !this_looping);
		    if (dump_file)
		      fprintf (dump_file, "Function found to be %spure: %s\n",
			       this_looping ? "looping " : "",
			       w->name ());
		  }
		remove_p |= w->call_for_symbol_and_aliases (cdtor_p,
							    NULL, true);
		w->set_pure_flag (true, this_looping);
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

  order_pos = ipa_reduced_postorder (order, true, false, ignore_edge);
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
	  funct_state w_l = get_function_state (w);

	  if (w_l->can_throw
	      || w->get_availability () == AVAIL_INTERPOSABLE)
	    can_throw = true;

	  for (e = w->callees; e && !can_throw; e = e->next_callee)
	    {
	      enum availability avail;
	      struct cgraph_node *y = e->callee->
				function_or_virtual_thunk_symbol (&avail);

	      if (avail > AVAIL_INTERPOSABLE)
		{
		  funct_state y_l = get_function_state (y);

		  if (y_l->can_throw && !TREE_NOTHROW (w->decl)
		      && e->can_throw_external)
		    can_throw = true;
		}
	      else if (e->can_throw_external && !TREE_NOTHROW (y->decl))
	        can_throw = true;
	    }
          for (ie = w->indirect_calls; ie && !can_throw; ie = ie->next_callee)
	    if (ie->can_throw_external)
	      can_throw = true;
	  w_info = (struct ipa_dfs_info *) w->aux;
	  w = w_info->next_cycle;
	}

      /* Copy back the region's pure_const_state which is shared by
	 all nodes in the region.  */
      w = node;
      while (w)
	{
	  funct_state w_l = get_function_state (w);
	  if (!can_throw && !TREE_NOTHROW (w->decl))
	    {
	      /* Inline clones share declaration with their offline copies;
		 do not modify their declarations since the offline copy may
		 be different.  */
	      if (!w->global.inlined_to)
		{
		  w->set_nothrow_flag (true);
		  if (dump_file)
		    fprintf (dump_file, "Function found to be nothrow: %s\n",
			     w->name ());
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


/* Produce the global information by preforming a transitive closure
   on the local information that was produced by generate_summary.  */

unsigned int
pass_ipa_pure_const::
execute (function *)
{
  struct cgraph_node *node;
  bool remove_p;

  symtab->remove_cgraph_insertion_hook (function_insertion_hook_holder);
  symtab->remove_cgraph_duplication_hook (node_duplication_hook_holder);
  symtab->remove_cgraph_removal_hook (node_removal_hook_holder);

  /* Nothrow makes more function to not lead to return and improve
     later analysis.  */
  propagate_nothrow ();
  remove_p = propagate_pure_const ();

  /* Cleanup. */
  FOR_EACH_FUNCTION (node)
    if (has_function_state (node))
      free (get_function_state (node));
  funct_state_vec.release ();
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
  init_p(false),
  function_insertion_hook_holder(NULL),
  node_duplication_hook_holder(NULL),
  node_removal_hook_holder(NULL)
{
}

ipa_opt_pass_d *
make_pass_ipa_pure_const (gcc::context *ctxt)
{
  return new pass_ipa_pure_const (ctxt);
}

/* Return true if function should be skipped for local pure const analysis.  */

static bool
skip_function_for_local_pure_const (struct cgraph_node *node)
{
  /* Because we do not schedule pass_fixup_cfg over whole program after early optimizations
     we must not promote functions that are called by already processed functions.  */

  if (function_called_by_processed_nodes_p ())
    {
      if (dump_file)
        fprintf (dump_file, "Function called in recursive cycle; ignoring\n");
      return true;
    }
  if (node->get_availability () <= AVAIL_INTERPOSABLE)
    {
      if (dump_file)
        fprintf (dump_file, "Function is not available or overwritable; not analyzing.\n");
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
	  if (!skip)
	    {
	      node->set_const_flag (true, l->looping);
	      changed = true;
	    }
	  if (dump_file)
	    fprintf (dump_file, "Function found to be %sconst: %s\n",
		     l->looping ? "looping " : "",
		     current_function_name ());
	}
      else if (DECL_LOOPING_CONST_OR_PURE_P (current_function_decl)
	       && !l->looping)
	{
	  if (!skip)
	    {
	      node->set_const_flag (true, false);
	      changed = true;
	    }
	  if (dump_file)
	    fprintf (dump_file, "Function found to be non-looping: %s\n",
		     current_function_name ());
	}
      break;

    case IPA_PURE:
      if (!DECL_PURE_P (current_function_decl))
	{
	  if (!skip)
	    {
	      node->set_pure_flag (true, l->looping);
	      changed = true;
	    }
	  warn_function_pure (current_function_decl, !l->looping);
	  if (dump_file)
	    fprintf (dump_file, "Function found to be %spure: %s\n",
		     l->looping ? "looping " : "",
		     current_function_name ());
	}
      else if (DECL_LOOPING_CONST_OR_PURE_P (current_function_decl)
	       && !l->looping)
	{
	  if (!skip)
	    {
	      node->set_pure_flag (true, false);
	      changed = true;
	    }
	  if (dump_file)
	    fprintf (dump_file, "Function found to be non-looping: %s\n",
		     current_function_name ());
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

  /* We run during lowering, we can not really use availability yet.  */
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
        if (stmt_can_throw_external (gsi_stmt (gsi)))
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
		print_gimple_stmt (dump_file, gsi_stmt (gsi), 0, 0);
	      }
	    return 0;
	  }
    }

  node->set_nothrow_flag (true);
  if (dump_file)
    fprintf (dump_file, "Function found to be nothrow: %s\n",
	     current_function_name ());
  return 0;
}

} // anon namespace

gimple_opt_pass *
make_pass_nothrow (gcc::context *ctxt)
{
  return new pass_nothrow (ctxt);
}
