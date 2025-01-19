/* strub (stack scrubbing) support.
   Copyright (C) 2021-2025 Free Software Foundation, Inc.
   Contributed by Alexandre Oliva <oliva@adacore.com>.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "gimplify.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-into-ssa.h"
#include "tree-ssa.h"
#include "tree-cfg.h"
#include "cfghooks.h"
#include "cfgloop.h"
#include "cfgcleanup.h"
#include "tree-eh.h"
#include "except.h"
#include "builtins.h"
#include "attribs.h"
#include "tree-inline.h"
#include "cgraph.h"
#include "alloc-pool.h"
#include "symbol-summary.h"
#include "sreal.h"
#include "ipa-cp.h"
#include "ipa-prop.h"
#include "ipa-fnsummary.h"
#include "gimple-fold.h"
#include "fold-const.h"
#include "gimple-walk.h"
#include "tree-dfa.h"
#include "langhooks.h"
#include "calls.h"
#include "vec.h"
#include "stor-layout.h"
#include "varasm.h"
#include "alias.h"
#include "diagnostic.h"
#include "intl.h"
#include "ipa-strub.h"
#include "symtab-thunks.h"
#include "attr-fnspec.h"
#include "target.h"
#include "gcc-urlifier.h"

/* This file introduces two passes that, together, implement
   machine-independent stack scrubbing, strub for short.  It arranges
   for stack frames that have strub enabled to be zeroed-out after
   relinquishing control to a caller, whether by returning or by
   propagating an exception.  This admittedly unusual design decision
   was driven by exception support (one needs a stack frame to be
   active to propagate exceptions out of it), and it enabled an
   implementation that is entirely machine-independent (no custom
   epilogue code is required).

   Strub modes can be selected for stack frames by attaching attribute
   strub to functions or to variables (to their types, actually).
   Different strub modes, with different implementation details, are
   available, and they can be selected by an argument to the strub
   attribute.  When enabled by strub-enabled variables, whether by
   accessing (as in reading from) statically-allocated ones, or by
   introducing (as in declaring) automatically-allocated ones, a
   suitable mode is selected automatically.

   At-calls mode modifies the interface of a function, adding a stack
   watermark argument, that callers use to clean up the stack frame of
   the called function.  Because of the interface change, it can only
   be used when explicitly selected, or when a function is internal to
   a translation unit.  Strub-at-calls function types are distinct
   from their original types (they're not modified in-place), and they
   are not interchangeable with other function types.

   Internal mode, in turn, does not modify the type or the interface
   of a function.  It is currently implemented by turning the function
   into a wrapper, moving the function body to a separate wrapped
   function, and scrubbing the wrapped body's stack in the wrapper.
   Internal-strub function types are mostly interface-compatible with
   other strub modes, namely callable (from strub functions, though
   not strub-enabled) and disabled (not callable from strub
   functions).

   Always_inline functions can be strub functions, but they can only
   be called from other strub functions, because strub functions must
   never be inlined into non-strub functions.  Internal and at-calls
   modes are indistinguishable when it comes to always_inline
   functions: they will necessarily be inlined into another strub
   function, and will thus be integrated into the caller's stack
   frame, whatever the mode.  (Contrast with non-always_inline strub
   functions: an at-calls function can be called from other strub
   functions, ensuring no discontinuity in stack erasing, whereas an
   internal-strub function can only be called from other strub
   functions if it happens to be inlined, or if -fstrub=relaxed mode
   is in effect (that's the default).  In -fstrub=strict mode,
   internal-strub functions are not callable from strub functions,
   because the wrapper itself is not strubbed.

   The implementation involves two simple-IPA passes.  The earliest
   one, strub-mode, assigns strub modes to functions.  It needs to run
   before any inlining, so that we can prevent inlining of strub
   functions into non-strub functions.  It notes explicit strub mode
   requests, enables strub in response to strub variables and testing
   options, and flags unsatisfiable requests.

   Three possibilities of unsatisfiable requests come to mind: (a)
   when a strub mode is explicitly selected, but the function uses
   features that make it ineligible for that mode (e.g. at-calls rules
   out calling __builtin_apply_args, because of the interface changes,
   and internal mode rules out noclone or otherwise non-versionable
   functions, non-default varargs, non-local or forced labels, and
   functions with far too many arguments); (b) when some strub mode
   must be enabled because of a strub variable, but the function is
   not eligible or not viable for any mode; and (c) when
   -fstrub=strict is enabled, and calls are found in strub functions
   to functions that are not callable from strub contexts.
   compute_strub_mode implements (a) and (b), and verify_strub
   implements (c).

   The second IPA pass modifies interfaces of at-calls-strub functions
   and types, introduces strub calls in and around them. and splits
   internal-strub functions.  It is placed after early inlining, so
   that even internal-strub functions get a chance of being inlined
   into other strub functions, but before non-early inlining, so that
   internal-strub wrapper functions still get a chance of inlining
   after splitting.

   Wrappers avoid duplicating the copying of large arguments again by
   passing them by reference to the wrapped bodies.  This involves
   occasional SSA rewriting of address computations, because of the
   additional indirection.  Besides these changes, and the
   introduction of the stack watermark parameter, wrappers and wrapped
   functions cooperate to handle variable argument lists (performing
   va_start in the wrapper, passing the list as an argument, and
   replacing va_start calls in the wrapped body with va_copy), and
   __builtin_apply_args (also called in the wrapper and passed to the
   wrapped body as an argument).

   Strub bodies (both internal-mode wrapped bodies, and at-calls
   functions) always start by adjusting the watermark parameter, by
   calling __builtin___strub_update.  The compiler inserts them in the
   main strub pass.  Allocations of additional stack space for the
   frame (__builtin_alloca) are also followed by watermark updates.
   Stack space temporarily allocated to pass arguments to other
   functions, released right after the call, is not regarded as part
   of the frame.  Around calls to them, i.e., in internal-mode
   wrappers and at-calls callers (even calls through pointers), calls
   to __builtin___strub_enter and __builtin___strub_leave are
   inserted, the latter as a __finally block, so that it runs at
   regular and exceptional exit paths.  strub_enter only initializes
   the stack watermark, and strub_leave is where the scrubbing takes
   place, overwriting with zeros the stack space from the top of the
   stack to the watermark.

   These calls can be optimized in various cases.  In
   pass_ipa_strub::adjust_at_calls_call, for example, we enable
   tail-calling and other optimized calls from one strub body to
   another by passing on the watermark parameter.  The builtins
   themselves may undergo inline substitution during expansion,
   dependign on optimization levels.  This involves dealing with stack
   red zones (when the builtins are called out-of-line, the red zone
   cannot be used) and other ugly details related with inlining strub
   bodies into other strub bodies (see expand_builtin_strub_update).
   expand_builtin_strub_leave may even perform partial inline
   substitution.  */

/* Const and pure functions that gain a watermark parameter for strub purposes
   are still regarded as such, which may cause the inline expansions of the
   __strub builtins to malfunction.  Ideally, attribute "fn spec" would enable
   us to inform the backend about requirements and side effects of the call, but
   call_fusage building in calls.c:expand_call does not even look at
   attr_fnspec, so we resort to asm loads and updates to attain an equivalent
   effect.  Once expand_call gains the ability to issue extra memory uses and
   clobbers based on pure/const function's fnspec, we can define this to 1.  */
#define ATTR_FNSPEC_DECONST_WATERMARK 0

enum strub_mode {
  /* This mode denotes a regular function, that does not require stack
     scrubbing (strubbing).  It may call any other functions, but if
     it calls AT_CALLS (or WRAPPED) ones, strubbing logic is
     automatically introduced around those calls (the latter, by
     inlining INTERNAL wrappers).  */
  STRUB_DISABLED = 0,

  /* This denotes a function whose signature is (to be) modified to
     take an extra parameter, for stack use annotation, and its
     callers must initialize and pass that argument, and perform the
     strubbing.  Functions that are explicitly marked with attribute
     strub must have the mark visible wherever the function is,
     including aliases, and overriders and overriding methods.
     Functions that are implicitly marked for strubbing, for accessing
     variables explicitly marked as such, will only select this
     strubbing method if they are internal to a translation unit.  It
     can only be inlined into other strubbing functions, i.e.,
     STRUB_AT_CALLS or STRUB_WRAPPED.  */
  STRUB_AT_CALLS = 1,

  /* This denotes a function that is to perform strubbing internally,
     without any changes to its interface (the function is turned into
     a strubbing wrapper, and its original body is moved to a separate
     STRUB_WRAPPED function, with a modified interface).  Functions
     may be explicitly marked with attribute strub(2), and the
     attribute must be visible at the point of definition.  Functions
     that are explicitly marked for strubbing, for accessing variables
     explicitly marked as such, may select this strubbing mode if
     their interface cannot change, e.g. because its interface is
     visible to other translation units, directly, by indirection
     (having its address taken), inheritance, etc.  Functions that use
     this method must not have the noclone attribute, nor the noipa
     one.  Functions marked as always_inline may select this mode, but
     they are NOT wrapped, they remain unchanged, and are only inlined
     into strubbed contexts.  Once non-always_inline functions are
     wrapped, the wrapper becomes STRUB_WRAPPER, and the wrapped becomes
     STRUB_WRAPPED.  */
  STRUB_INTERNAL = 2,

  /* This denotes a function whose stack is not strubbed, but that is
     nevertheless explicitly or implicitly marked as callable from strubbing
     functions.  Normally, only STRUB_AT_CALLS (and STRUB_INTERNAL ->
     STRUB_WRAPPED) functions can be called from strubbing contexts (bodies of
     STRUB_AT_CALLS, STRUB_INTERNAL and STRUB_WRAPPED functions), but attribute
     strub(3) enables other functions to be (indirectly) called from these
     contexts.  Some builtins and internal functions may be implicitly marked as
     STRUB_CALLABLE.  */
  STRUB_CALLABLE = 3,

  /* This denotes the function that took over the body of a
     STRUB_INTERNAL function.  At first, it's only called by its
     wrapper, but the wrapper may be inlined.  The wrapped function,
     in turn, can only be inlined into other functions whose stack
     frames are strubbed, i.e., that are STRUB_WRAPPED or
     STRUB_AT_CALLS.  */
  STRUB_WRAPPED = -1,

  /* This denotes the wrapper function that replaced the STRUB_INTERNAL
     function.  This mode overrides the STRUB_INTERNAL mode at the time the
     internal to-be-wrapped function becomes a wrapper, so that inlining logic
     can tell one from the other.  */
  STRUB_WRAPPER = -2,

  /* This denotes an always_inline function that requires strubbing.  It can
     only be called from, and inlined into, other strubbing contexts.  */
  STRUB_INLINABLE = -3,

  /* This denotes a function that accesses strub variables, so it would call for
     internal strubbing (whether or not it's eligible for that), but since
     at-calls strubbing is viable, that's selected as an optimization.  This
     mode addresses the inconvenience that such functions may have different
     modes selected depending on optimization flags, and get a different
     callable status depending on that choice: if we assigned them
     STRUB_AT_CALLS mode, they would be callable when optimizing, whereas
     STRUB_INTERNAL would not be callable.  */
  STRUB_AT_CALLS_OPT = -4,

};

/* Look up a strub attribute in TYPE, and return it.  */

static tree
get_strub_attr_from_type (tree type)
{
  return lookup_attribute ("strub", TYPE_ATTRIBUTES (type));
}

/* Look up a strub attribute in DECL or in its type, and return it.  */

static tree
get_strub_attr_from_decl (tree decl)
{
  tree ret = lookup_attribute ("strub", DECL_ATTRIBUTES (decl));
  if (ret)
    return ret;
  return get_strub_attr_from_type (TREE_TYPE (decl));
}

#define STRUB_ID_COUNT		8
#define STRUB_IDENT_COUNT	3
#define STRUB_TYPE_COUNT	5

#define STRUB_ID_BASE		0
#define STRUB_IDENT_BASE	(STRUB_ID_BASE + STRUB_ID_COUNT)
#define STRUB_TYPE_BASE		(STRUB_IDENT_BASE + STRUB_IDENT_COUNT)
#define STRUB_CACHE_SIZE	(STRUB_TYPE_BASE + STRUB_TYPE_COUNT)

/* Keep the strub mode and temp identifiers and types from being GC'd.  */
static GTY((deletable)) tree strub_cache[STRUB_CACHE_SIZE];

/* Define a function to cache identifier ID, to be used as a strub attribute
   parameter for a strub mode named after NAME.  */
#define DEF_STRUB_IDS(IDX, NAME, ID)				\
static inline tree get_strub_mode_id_ ## NAME () {		\
  int idx = STRUB_ID_BASE + IDX;				\
  tree identifier = strub_cache[idx];				\
  if (!identifier)						\
    strub_cache[idx] = identifier = get_identifier (ID);	\
  return identifier;						\
}
/* Same as DEF_STRUB_IDS, but use the string expansion of NAME as ID.  */
#define DEF_STRUB_ID(IDX, NAME)			\
  DEF_STRUB_IDS (IDX, NAME, #NAME)

/* Define functions for each of the strub mode identifiers.
   Expose dashes rather than underscores.  */
DEF_STRUB_ID (0, disabled)
DEF_STRUB_IDS (1, at_calls, "at-calls")
DEF_STRUB_ID (2, internal)
DEF_STRUB_ID (3, callable)
DEF_STRUB_ID (4, wrapped)
DEF_STRUB_ID (5, wrapper)
DEF_STRUB_ID (6, inlinable)
DEF_STRUB_IDS (7, at_calls_opt, "at-calls-opt")

/* Release the temporary macro names.  */
#undef DEF_STRUB_IDS
#undef DEF_STRUB_ID

/* Return the identifier corresponding to strub MODE.  */

static tree
get_strub_mode_attr_parm (enum strub_mode mode)
{
  switch (mode)
    {
    case STRUB_DISABLED:
      return get_strub_mode_id_disabled ();

    case STRUB_AT_CALLS:
      return get_strub_mode_id_at_calls ();

    case STRUB_INTERNAL:
      return get_strub_mode_id_internal ();

    case STRUB_CALLABLE:
      return get_strub_mode_id_callable ();

    case STRUB_WRAPPED:
      return get_strub_mode_id_wrapped ();

    case STRUB_WRAPPER:
      return get_strub_mode_id_wrapper ();

    case STRUB_INLINABLE:
      return get_strub_mode_id_inlinable ();

    case STRUB_AT_CALLS_OPT:
      return get_strub_mode_id_at_calls_opt ();

    default:
      gcc_unreachable ();
    }
}

/* Return the parmeters (TREE_VALUE) for a strub attribute of MODE.
   We know we use a single parameter, so we bypass the creation of a
   tree list.  */

static tree
get_strub_mode_attr_value (enum strub_mode mode)
{
  return get_strub_mode_attr_parm (mode);
}

/* Determine whether ID is a well-formed strub mode-specifying attribute
   parameter for a function (type).  Only user-visible modes are accepted, and
   ID must be non-NULL.

   For unacceptable parms, return 0, otherwise a nonzero value as below.

   If the parm enables strub, return positive, otherwise negative.

   If the affected type must be a distinct, incompatible type,return an integer
   of absolute value 2, otherwise 1.  */

int
strub_validate_fn_attr_parm (tree id)
{
  int ret;
  const char *s = NULL;
  size_t len = 0;

  /* do NOT test for NULL.  This is only to be called with non-NULL arguments.
     We assume that the strub parameter applies to a function, because only
     functions accept an explicit argument.  If we accepted NULL, and we
     happened to be called to verify the argument for a variable, our return
     values would be wrong.  */
  if (TREE_CODE (id) == STRING_CST)
    {
      s = TREE_STRING_POINTER (id);
      len = TREE_STRING_LENGTH (id) - 1;
    }
  else if (TREE_CODE (id) == IDENTIFIER_NODE)
    {
      s = IDENTIFIER_POINTER (id);
      len = IDENTIFIER_LENGTH (id);
    }
  else
    return 0;

  enum strub_mode mode;

  if (len != 8)
    return 0;

  switch (s[0])
    {
    case 'd':
      mode = STRUB_DISABLED;
      ret = -1;
      break;

    case 'a':
      mode = STRUB_AT_CALLS;
      ret = 2;
      break;

    case 'i':
      mode = STRUB_INTERNAL;
      ret = 1;
      break;

    case 'c':
      mode = STRUB_CALLABLE;
      ret = -2;
      break;

    default:
      /* Other parms are for internal use only.  */
      return 0;
    }

  tree mode_id = get_strub_mode_attr_parm (mode);

  if (TREE_CODE (id) == IDENTIFIER_NODE
      ? id != mode_id
      : strncmp (s, IDENTIFIER_POINTER (mode_id), len) != 0)
    return 0;

  return ret;
}

/* Return the strub mode from STRUB_ATTR.  VAR_P should be TRUE if the attribute
   is taken from a variable, rather than from a function, or a type thereof.  */

static enum strub_mode
get_strub_mode_from_attr (tree strub_attr, bool var_p = false)
{
  enum strub_mode mode = STRUB_DISABLED;

  if (strub_attr)
    {
      if (!TREE_VALUE (strub_attr))
	mode = !var_p ? STRUB_AT_CALLS : STRUB_INTERNAL;
      else
	{
	  gcc_checking_assert (!var_p);
	  tree id = TREE_VALUE (strub_attr);
	  if (TREE_CODE (id) == TREE_LIST)
	    id = TREE_VALUE (id);
	  const char *s = (TREE_CODE (id) == STRING_CST
			   ? TREE_STRING_POINTER (id)
			   : IDENTIFIER_POINTER (id));
	  size_t len = (TREE_CODE (id) == STRING_CST
			? TREE_STRING_LENGTH (id) - 1
			: IDENTIFIER_LENGTH (id));

	  switch (len)
	    {
	    case 7:
	      switch (s[6])
		{
		case 'r':
		  mode = STRUB_WRAPPER;
		  break;

		case 'd':
		  mode = STRUB_WRAPPED;
		  break;

		default:
		  gcc_unreachable ();
		}
	      break;

	    case 8:
	      switch (s[0])
		{
		case 'd':
		  mode = STRUB_DISABLED;
		  break;

		case 'a':
		  mode = STRUB_AT_CALLS;
		  break;

		case 'i':
		  mode = STRUB_INTERNAL;
		  break;

		case 'c':
		  mode = STRUB_CALLABLE;
		  break;

		default:
		  gcc_unreachable ();
		}
	      break;

	    case 9:
	      mode = STRUB_INLINABLE;
	      break;

	    case 12:
	      mode = STRUB_AT_CALLS_OPT;
	      break;

	    default:
	      gcc_unreachable ();
	    }

	  gcc_checking_assert (TREE_CODE (id) == IDENTIFIER_NODE
			       ? id == get_strub_mode_attr_parm (mode)
			       : strncmp (IDENTIFIER_POINTER
					  (get_strub_mode_attr_parm (mode)),
					  s, len) == 0);
	}
    }

  return mode;
}

/* Look up, decode and return the strub mode associated with FNDECL.  */

static enum strub_mode
get_strub_mode_from_fndecl (tree fndecl)
{
  return get_strub_mode_from_attr (get_strub_attr_from_decl (fndecl));
}

/* Look up, decode and return the strub mode associated with NODE.  */

static enum strub_mode
get_strub_mode (cgraph_node *node)
{
  return get_strub_mode_from_fndecl (node->decl);
}

/* Look up, decode and return the strub mode associated with TYPE.  */

static enum strub_mode
get_strub_mode_from_type (tree type)
{
  bool var_p = !FUNC_OR_METHOD_TYPE_P (type);
  tree attr = get_strub_attr_from_type (type);

  if (attr)
    return get_strub_mode_from_attr (attr, var_p);

  if (flag_strub >= -1 && !var_p)
    return STRUB_CALLABLE;

  return STRUB_DISABLED;
}


/* Return TRUE iff NODE calls builtin va_start.  */

static bool
calls_builtin_va_start_p (cgraph_node *node)
{
  bool result = false;

  for (cgraph_edge *e = node->callees; e; e = e->next_callee)
    {
      tree cdecl = e->callee->decl;
      if (fndecl_built_in_p (cdecl, BUILT_IN_VA_START))
	return true;
    }

  return result;
}

/* Return TRUE iff NODE calls builtin apply_args, and optionally REPORT it.  */

static bool
calls_builtin_apply_args_p (cgraph_node *node, bool report = false)
{
  bool result = false;

  for (cgraph_edge *e = node->callees; e; e = e->next_callee)
    {
      tree cdecl = e->callee->decl;
      if (!fndecl_built_in_p (cdecl, BUILT_IN_APPLY_ARGS))
	continue;

      result = true;

      if (!report)
	break;

      sorry_at (e->call_stmt
		? gimple_location (e->call_stmt)
		: DECL_SOURCE_LOCATION (node->decl),
		"at-calls %<strub%> does not support call to %qD",
		cdecl);
    }

  return result;
}

/* Return TRUE iff NODE carries the always_inline attribute.  */

static inline bool
strub_always_inline_p (cgraph_node *node)
{
  return lookup_attribute ("always_inline", DECL_ATTRIBUTES (node->decl));
}

/* Return TRUE iff the target has strub support for T, a function
   decl, or a type used in an indirect call, and optionally REPORT the
   reasons for ineligibility.  If T is a type and error REPORTing is
   enabled, the LOCation (of the indirect call) should be provided.  */
static inline bool
strub_target_support_p (tree t, bool report = false,
			location_t loc = UNKNOWN_LOCATION)
{
  bool result = true;

  if (!targetm.have_strub_support_for (t))
    {
      result = false;

      if (!report)
	return result;

      if (DECL_P (t))
	sorry_at (DECL_SOURCE_LOCATION (t),
		  "%qD is not eligible for %<strub%>"
		  " on the target system", t);
      else
	sorry_at (loc,
		  "unsupported %<strub%> call"
		  " on the target system");
    }

  return result;
}

/* Return TRUE iff NODE is potentially eligible for any strub-enabled mode, and
   optionally REPORT the reasons for ineligibility.  */

static inline bool
can_strub_p (cgraph_node *node, bool report = false)
{
  bool result = strub_target_support_p (node->decl, report);

  if (!report && (!result || strub_always_inline_p (node)))
    return result;

  auto_urlify_attributes sentinel;

  if (flag_split_stack)
    {
      result = false;

      if (!report)
	return result;

      sorry_at (DECL_SOURCE_LOCATION (node->decl),
		"%qD is not eligible for %<strub%>"
		" because %<-fsplit-stack%> is enabled",
		node->decl);
    }

  if (lookup_attribute ("noipa", DECL_ATTRIBUTES (node->decl)))
    {
      result = false;

      if (!report)
	return result;

      sorry_at (DECL_SOURCE_LOCATION (node->decl),
		"%qD is not eligible for %<strub%>"
		" because of attribute %<noipa%>",
		node->decl);
    }

  /* We can't, and don't want to vectorize the watermark and other
     strub-introduced parms.  */
  if (lookup_attribute ("simd", DECL_ATTRIBUTES (node->decl)))
    {
      result = false;

      if (!report)
	return result;

      sorry_at (DECL_SOURCE_LOCATION (node->decl),
		"%qD is not eligible for %<strub%>"
		" because of attribute %<simd%>",
		node->decl);
    }

  return result;
}

/* Return TRUE iff NODE is eligible for at-calls strub, and optionally REPORT
   the reasons for ineligibility.  Besides general non-eligibility for
   strub-enabled modes, at-calls rules out calling builtin apply_args.  */

static bool
can_strub_at_calls_p (cgraph_node *node, bool report = false)
{
  bool result = !report || can_strub_p (node, report);

  if (!result && !report)
    return result;

  return !calls_builtin_apply_args_p (node, report);
}

/* Return TRUE iff the called function (pointer or, if available,
   decl) undergoes a significant type conversion for the call.  Strub
   mode changes between function types, and other non-useless type
   conversions, are regarded as significant.  When the function type
   is overridden, the effective strub mode for the call is that of the
   call fntype, rather than that of the pointer or of the decl.
   Functions called with type overrides cannot undergo type changes;
   it's as if their address was taken, so they're considered
   non-viable for implicit at-calls strub mode.  */

static inline bool
strub_call_fntype_override_p (const gcall *gs)
{
  if (gimple_call_internal_p (gs))
    return false;
  tree fn_type = TREE_TYPE (TREE_TYPE (gimple_call_fn (gs)));
  if (tree decl = gimple_call_fndecl (gs))
    fn_type = TREE_TYPE (decl);

  /* We do NOT want to take the mode from the decl here.  This
     function is used to tell whether we can change the strub mode of
     a function, and whether the effective mode for the call is to be
     taken from the decl or from an overrider type.  When the strub
     mode is explicitly declared, or overridden with a type cast, the
     difference will be noticed in function types.  However, if the
     strub mode is implicit due to e.g. strub variables or -fstrub=*
     command-line flags, we will adjust call types along with function
     types.  In either case, the presence of type or strub mode
     overriders in calls will prevent a function from having its strub
     modes changed in ways that would imply type changes, but taking
     strub modes from decls would defeat this, since we set strub
     modes and then call this function to tell whether the original
     type was overridden to decide whether to adjust the call.  We
     need the answer to be about the type, not the decl.  */
  enum strub_mode mode = get_strub_mode_from_type (fn_type);
  return (get_strub_mode_from_type (gs->u.fntype) != mode
	  || !useless_type_conversion_p (gs->u.fntype, fn_type));
}

/* Return TRUE iff NODE is called directly with a type override.  */

static bool
called_directly_with_type_override_p (cgraph_node *node, void *)
{
  for (cgraph_edge *e = node->callers; e; e = e->next_caller)
    if (e->call_stmt && strub_call_fntype_override_p (e->call_stmt))
      return true;

  return false;
}

/* Return TRUE iff NODE or any other nodes aliased to it are called
   with type overrides.  We can't safely change the type of such
   functions.  */

static bool
called_with_type_override_p (cgraph_node *node)
{
  return (node->call_for_symbol_thunks_and_aliases
	  (called_directly_with_type_override_p, NULL, true, true));
}

/* Symbolic macro for the max number of arguments that internal strub may add to
   a function.  */

#define STRUB_INTERNAL_MAX_EXTRA_ARGS 3

/* We can't perform internal strubbing if the function body involves certain
   features:

   - a non-default __builtin_va_start (e.g. x86's __builtin_ms_va_start) is
   currently unsupported because we can't discover the corresponding va_copy and
   va_end decls in the wrapper, and we don't convey the alternate variable
   arguments ABI to the modified wrapped function.  The default
   __builtin_va_start is supported by calling va_start/va_end at the wrapper,
   that takes variable arguments, passing a pointer to the va_list object to the
   wrapped function, that runs va_copy from it where the original function ran
   va_start.

   __builtin_next_arg is currently unsupported because the wrapped function
   won't be a variable argument function.  We could process it in the wrapper,
   that remains a variable argument function, and replace calls in the wrapped
   body, but we currently don't.

   __builtin_return_address is rejected because it's generally used when the
   actual caller matters, and introducing a wrapper breaks such uses as those in
   the unwinder.  */

static bool
can_strub_internally_p (cgraph_node *node, bool report = false)
{
  bool result = !report || can_strub_p (node, report);

  if (!result && !report)
    return result;

  if (!report && strub_always_inline_p (node))
    return result;

  /* Since we're not changing the function identity proper, just
     moving its full implementation, we *could* disable
     fun->cannot_be_copied_reason and/or temporarily drop a noclone
     attribute, but we'd have to prevent remapping of the labels.  */
  if (lookup_attribute ("noclone", DECL_ATTRIBUTES (node->decl)))
    {
      result = false;

      if (!report)
	return result;

      sorry_at (DECL_SOURCE_LOCATION (node->decl),
		"%qD is not eligible for internal %<strub%>"
		" because of attribute %<noclone%>",
		node->decl);
    }

  if (node->has_gimple_body_p ())
    {
      for (cgraph_edge *e = node->callees; e; e = e->next_callee)
	{
	  tree cdecl = e->callee->decl;
	  if (!((fndecl_built_in_p (cdecl, BUILT_IN_VA_START)
		 && cdecl != builtin_decl_explicit (BUILT_IN_VA_START))
		|| fndecl_built_in_p (cdecl, BUILT_IN_NEXT_ARG)
		|| fndecl_built_in_p (cdecl, BUILT_IN_RETURN_ADDRESS)))
	    continue;

	  result = false;

	  if (!report)
	    return result;

	  sorry_at (e->call_stmt
		    ? gimple_location (e->call_stmt)
		    : DECL_SOURCE_LOCATION (node->decl),
		    "%qD is not eligible for internal %<strub%> "
		    "because it calls %qD",
		    node->decl, cdecl);
	}

      struct function *fun = DECL_STRUCT_FUNCTION (node->decl);
      if (fun->has_nonlocal_label)
	{
	  result = false;

	  if (!report)
	    return result;

	  sorry_at (DECL_SOURCE_LOCATION (node->decl),
		    "%qD is not eligible for internal %<strub%> "
		    "because it contains a non-local goto target",
		    node->decl);
	}

      if (fun->has_forced_label_in_static)
	{
	  result = false;

	  if (!report)
	    return result;

	  sorry_at (DECL_SOURCE_LOCATION (node->decl),
		    "%qD is not eligible for internal %<strub%> "
		    "because the address of a local label escapes",
		    node->decl);
	}

      /* Catch any other case that would prevent versioning/cloning
	 so as to also have it covered above.  */
      gcc_checking_assert (!result /* || !node->has_gimple_body_p () */
			   || tree_versionable_function_p (node->decl));


      /* Label values references are not preserved when copying.  If referenced
	 in nested functions, as in 920415-1.c and 920721-4.c their decls get
	 remapped independently.  The exclusion below might be too broad, in
	 that we might be able to support correctly cases in which the labels
	 are only used internally in a function, but disconnecting forced labels
	 from their original declarations is undesirable in general.  */
      basic_block bb;
      FOR_EACH_BB_FN (bb, DECL_STRUCT_FUNCTION (node->decl))
	for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
	     !gsi_end_p (gsi); gsi_next (&gsi))
	  {
	    glabel *label_stmt = dyn_cast <glabel *> (gsi_stmt (gsi));
	    tree target;

	    if (!label_stmt)
	      break;

	    target = gimple_label_label (label_stmt);

	    if (!FORCED_LABEL (target))
	      continue;

	    result = false;

	    if (!report)
	      return result;

	    sorry_at (gimple_location (label_stmt),
		      "internal %<strub%> does not support forced labels");
	  }
    }

  if (list_length (TYPE_ARG_TYPES (TREE_TYPE (node->decl)))
      >= ((HOST_WIDE_INT_1 << IPA_PARAM_MAX_INDEX_BITS)
	  - STRUB_INTERNAL_MAX_EXTRA_ARGS))
    {
      result = false;

      if (!report)
	return result;

      sorry_at (DECL_SOURCE_LOCATION (node->decl),
		"%qD has too many arguments for internal %<strub%>",
		node->decl);
    }

  return result;
}

/* Return TRUE iff NODE has any strub-requiring local variable, or accesses (as
   in reading) any variable through a strub-requiring type.  */

static bool
strub_from_body_p (cgraph_node *node)
{
  if (!node->has_gimple_body_p ())
    return false;

  /* If any local variable is marked for strub...  */
  unsigned i;
  tree var;
  FOR_EACH_LOCAL_DECL (DECL_STRUCT_FUNCTION (node->decl),
		       i, var)
    if (get_strub_mode_from_type (TREE_TYPE (var))
	!= STRUB_DISABLED)
      return true;

  /* Now scan the body for loads with strub-requiring types.
     ??? Compound types don't propagate the strub requirement to
     component types.  */
  basic_block bb;
  FOR_EACH_BB_FN (bb, DECL_STRUCT_FUNCTION (node->decl))
    for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
	 !gsi_end_p (gsi); gsi_next (&gsi))
      {
	gimple *stmt = gsi_stmt (gsi);

	if (!gimple_assign_load_p (stmt))
	  continue;

	tree rhs = gimple_assign_rhs1 (stmt);
	if (get_strub_mode_from_type (TREE_TYPE (rhs))
	    != STRUB_DISABLED)
	  return true;
      }

  return false;
}

/* Return TRUE iff node is associated with a builtin that should be callable
   from strub contexts.  */

static inline bool
strub_callable_builtin_p (cgraph_node *node)
{
  if (DECL_BUILT_IN_CLASS (node->decl) != BUILT_IN_NORMAL)
    return false;

  enum built_in_function fcode = DECL_FUNCTION_CODE (node->decl);

  switch (fcode)
    {
    case BUILT_IN_NONE:
      gcc_unreachable ();

      /* This temporarily allocates stack for the call, and we can't reasonably
	 update the watermark for that.  Besides, we don't check the actual call
	 target, nor its signature, and it seems to be overkill to as much as
	 try to do so.  */
    case BUILT_IN_APPLY:
      return false;

      /* Conversely, this shouldn't be called from within strub contexts, since
	 the caller may have had its signature modified.  STRUB_INTERNAL is ok,
	 the call will remain in the STRUB_WRAPPER, and removed from the
	 STRUB_WRAPPED clone.  */
    case BUILT_IN_APPLY_ARGS:
      return false;

      /* ??? Make all other builtins callable.  We wish to make any builtin call
	 the compiler might introduce on its own callable.  Anything that is
	 predictable enough as to be known not to allow stack data that should
	 be strubbed to unintentionally escape to non-strub contexts can be
	 allowed, and pretty much every builtin appears to fit this description.
	 The exceptions to this rule seem to be rare, and only available as
	 explicit __builtin calls, so let's keep it simple and allow all of
	 them...  */
    default:
      return true;
    }
}

/* Compute the strub mode to be used for NODE.  STRUB_ATTR should be the strub
   attribute,found for NODE, if any.  */

static enum strub_mode
compute_strub_mode (cgraph_node *node, tree strub_attr)
{
  enum strub_mode req_mode = get_strub_mode_from_attr (strub_attr);

  gcc_checking_assert (flag_strub >= -2 && flag_strub <= 3);

  /* Symbolic encodings of the -fstrub-* flags.  */
  /* Enable strub when explicitly requested through attributes to functions or
     variables, reporting errors if the requests cannot be satisfied.  */
  const bool strub_flag_auto = flag_strub < 0;
  /* strub_flag_auto with strub call verification; without this, functions are
     implicitly callable.  */
  const bool strub_flag_strict = flag_strub < -1;
  /* Disable strub altogether, ignore attributes entirely.  */
  const bool strub_flag_disabled = flag_strub == 0;
  /* On top of _auto, also enable strub implicitly for functions that can
     safely undergo at-calls strubbing.  Internal mode will still be used in
     functions that request it explicitly with attribute strub(2), or when the
     function body requires strubbing and at-calls strubbing is not viable.  */
  const bool strub_flag_at_calls = flag_strub == 1;
  /* On top of default, also enable strub implicitly for functions that can
     safely undergo internal strubbing.  At-calls mode will still be used in
     functions that requiest it explicitly with attribute strub() or strub(1),
     or when the function body requires strubbing and internal strubbing is not
     viable.  */
  const bool strub_flag_internal = flag_strub == 2;
  /* On top of default, also enable strub implicitly for functions that can
     safely undergo strubbing in either mode.  When both modes are viable,
     at-calls is preferred.  */
  const bool strub_flag_either = flag_strub == 3;
  /* Besides the default behavior, enable strub implicitly for all viable
     functions.  */
  const bool strub_flag_viable = flag_strub > 0;

  /* The consider_* variables should be TRUE if selecting the corresponding
     strub modes would be consistent with requests from attributes and command
     line flags.  Attributes associated with functions pretty much mandate a
     selection, and should report an error if not satisfied; strub_flag_auto
     implicitly enables some viable strub mode if that's required by references
     to variables marked for strub; strub_flag_viable enables strub if viable
     (even when favoring one mode, body-requested strub can still be satisfied
     by either mode), and falls back to callable, silently unless variables
     require strubbing.  */

  const bool consider_at_calls
    = (!strub_flag_disabled
       && (strub_attr
	   ? req_mode == STRUB_AT_CALLS
	   : true));
  const bool consider_internal
    = (!strub_flag_disabled
       && (strub_attr
	   ? req_mode == STRUB_INTERNAL
	   : true));

  const bool consider_callable
    = (!strub_flag_disabled
       && (strub_attr
	   ? req_mode == STRUB_CALLABLE
	   : (!strub_flag_strict
	      || strub_callable_builtin_p (node))));

  /* This is a shorthand for either strub-enabled mode.  */
  const bool consider_strub
    = (consider_at_calls || consider_internal);

  /* We can cope with always_inline functions even with noipa and noclone,
     because we just leave them alone.  */
  const bool is_always_inline
    = strub_always_inline_p (node);

  /* Strubbing in general, and each specific strub mode, may have its own set of
     requirements.  We require noipa for strubbing, either because of cloning
     required for internal strub, or because of caller enumeration required for
     at-calls strub.  We don't consider the at-calls mode eligible if it's not
     even considered, it has no further requirements.  Internal mode requires
     cloning and the absence of certain features in the body and, like at-calls,
     it's not eligible if it's not even under consideration.

     ??? Do we need target hooks for further constraints?  E.g., x86's
     "interrupt" attribute breaks internal strubbing because the wrapped clone
     carries the attribute and thus isn't callable; in this case, we could use a
     target hook to adjust the clone instead.  */
  const bool strub_eligible
    = (consider_strub
       && (is_always_inline || can_strub_p (node)));
  const bool at_calls_eligible
    = (consider_at_calls && strub_eligible
       && can_strub_at_calls_p (node));
  const bool internal_eligible
    = (consider_internal && strub_eligible
       && (is_always_inline
	   || can_strub_internally_p (node)));

  /* In addition to the strict eligibility requirements, some additional
     constraints are placed on implicit selection of certain modes.  These do
     not prevent the selection of a mode if explicitly specified as part of a
     function interface (the strub attribute), but they may prevent modes from
     being selected by the command line or by function bodies.  The only actual
     constraint is on at-calls mode: since we change the function's exposed
     signature, we won't do it implicitly if the function can possibly be used
     in ways that do not expect the signature change, e.g., if the function is
     available to or interposable by other units, if its address is taken,
     etc.  */
  const bool at_calls_viable
    = (at_calls_eligible
       && (strub_attr
	   || (node->has_gimple_body_p ()
	       && (!node->externally_visible
		   || (node->binds_to_current_def_p ()
		       && node->can_be_local_p ()))
	       && node->only_called_directly_p ()
	       && !called_with_type_override_p (node))));
  const bool internal_viable
    = (internal_eligible);

  /* Shorthand.  */
  const bool strub_viable
    = (at_calls_viable || internal_viable);

  /* We wish to analyze the body, to look for implicit requests for strub, both
     to implicitly enable it when the body calls for it, and to report errors if
     the body calls for it but neither mode is viable (even if that follows from
     non-eligibility because of the explicit specification of some non-strubbing
     mode).  We can refrain from scanning the body only in rare circumstances:
     when strub is enabled by a function attribute (scanning might be redundant
     in telling us to also enable it), and when we are enabling strub implicitly
     but there are non-viable modes: we want to know whether strubbing is
     required, to fallback to another mode, even if we're only enabling a
     certain mode, or, when either mode would do, to report an error if neither
     happens to be viable.  */
  const bool analyze_body
    = (strub_attr
       ? !consider_strub
       : (strub_flag_auto
	  || (strub_flag_viable && (!at_calls_viable && !internal_viable))
	  || (strub_flag_either && !strub_viable)));

  /* Cases in which strubbing is enabled or disabled by strub_flag_auto.
     Unsatisfiable requests ought to be reported.  */
  const bool strub_required
    = ((strub_attr && consider_strub)
       || (analyze_body && strub_from_body_p (node)));

  /* Besides the required cases, we want to abide by the requests to enabling on
     an if-viable basis.  */
  const bool strub_enable
    = (strub_required
       || (strub_flag_at_calls && at_calls_viable)
       || (strub_flag_internal && internal_viable)
       || (strub_flag_either && strub_viable));

  /* And now we're finally ready to select a mode that abides by the viability
     and eligibility constraints, and that satisfies the strubbing requirements
     and requests, subject to the constraints.  If both modes are viable and
     strub is to be enabled, pick STRUB_AT_CALLS unless STRUB_INTERNAL was named
     as preferred.  */
  const enum strub_mode mode
    = ((strub_enable && is_always_inline)
       ? (strub_required ? STRUB_INLINABLE : STRUB_CALLABLE)
       : (strub_enable && internal_viable
	  && (strub_flag_internal || !at_calls_viable))
       ? STRUB_INTERNAL
       : (strub_enable && at_calls_viable)
       ? (strub_required && !strub_attr
	  ? STRUB_AT_CALLS_OPT
	  : STRUB_AT_CALLS)
       : consider_callable
       ? STRUB_CALLABLE
       : STRUB_DISABLED);

  switch (mode)
    {
    case STRUB_CALLABLE:
      if (is_always_inline)
	break;
      /* Fall through.  */

    case STRUB_DISABLED:
      if (strub_enable && !strub_attr)
	{
	  gcc_checking_assert (analyze_body);
	  error_at (DECL_SOURCE_LOCATION (node->decl),
		    "%qD requires %<strub%>,"
		    " but no viable %<strub%> mode was found",
		    node->decl);
	  break;
	}
      /* Fall through.  */

    case STRUB_AT_CALLS:
    case STRUB_INTERNAL:
    case STRUB_INLINABLE:
      /* Differences from an mode requested through a function attribute are
	 reported in set_strub_mode_to.  */
      break;

    case STRUB_AT_CALLS_OPT:
      /* Functions that select this mode do so because of references to strub
	 variables.  Even if we choose at-calls as an optimization, the
	 requirements for internal strub must still be satisfied.  Optimization
	 options may render implicit at-calls strub not viable (-O0 sets
	 force_output for static non-inline functions), and it would not be good
	 if changing optimization options turned a well-formed into an
	 ill-formed one.  */
      if (!internal_viable)
	can_strub_internally_p (node, true);
      break;

    case STRUB_WRAPPED:
    case STRUB_WRAPPER:
    default:
      gcc_unreachable ();
    }

  return mode;
}

/* Set FNDT's strub mode to MODE; FNDT may be a function decl or
   function type.  If OVERRIDE, do not check whether a mode is already
   set.  */

static void
strub_set_fndt_mode_to (tree fndt, enum strub_mode mode, bool override)
{
  gcc_checking_assert (override
		       || !(DECL_P (fndt)
			    ? get_strub_attr_from_decl (fndt)
			    : get_strub_attr_from_type (fndt)));

  tree attr = tree_cons (get_identifier ("strub"),
			 get_strub_mode_attr_value (mode),
			 NULL_TREE);
  tree *attrp = NULL;
  if (DECL_P (fndt))
    {
      gcc_checking_assert (FUNC_OR_METHOD_TYPE_P (TREE_TYPE (fndt)));
      attrp = &DECL_ATTRIBUTES (fndt);
    }
  else if (FUNC_OR_METHOD_TYPE_P (fndt))
    attrp = &TYPE_ATTRIBUTES (fndt);
  else
    gcc_unreachable ();

  TREE_CHAIN (attr) = *attrp;
  *attrp = attr;
}

/* Set FNDT's strub mode to callable.
   FNDT may be a function decl or a function type.  */

void
strub_make_callable (tree fndt)
{
  strub_set_fndt_mode_to (fndt, STRUB_CALLABLE, false);
}

/* Set NODE to strub MODE.  Report incompatibilities between MODE and the mode
   requested through explicit attributes, and cases of non-eligibility.  */

static void
set_strub_mode_to (cgraph_node *node, enum strub_mode mode)
{
  tree attr = get_strub_attr_from_decl (node->decl);
  enum strub_mode req_mode = get_strub_mode_from_attr (attr);

  if (attr)
    {
      /* Check for and report incompatible mode changes.  */
      if (mode != req_mode
	  && !(req_mode == STRUB_INTERNAL
	       && (mode == STRUB_WRAPPED
		   || mode == STRUB_WRAPPER))
	  && !((req_mode == STRUB_INTERNAL
		|| req_mode == STRUB_AT_CALLS
		|| req_mode == STRUB_CALLABLE)
	       && mode == STRUB_INLINABLE))
	{
	  error_at (DECL_SOURCE_LOCATION (node->decl),
		    "%<strub%> mode %qE selected for %qD, when %qE was requested",
		    get_strub_mode_attr_parm (mode),
		    node->decl,
		    get_strub_mode_attr_parm (req_mode));
	  if (node->alias)
	    {
	      cgraph_node *target = node->ultimate_alias_target ();
	      if (target != node)
		error_at (DECL_SOURCE_LOCATION (target->decl),
			  "the incompatible selection was determined"
			  " by ultimate alias target %qD",
			  target->decl);
	    }

	  /* Report any incompatibilities with explicitly-requested strub.  */
	  switch (req_mode)
	    {
	    case STRUB_AT_CALLS:
	      can_strub_at_calls_p (node, true);
	      break;

	    case STRUB_INTERNAL:
	      can_strub_internally_p (node, true);
	      break;

	    default:
	      break;
	    }
	}

      /* Drop any incompatible strub attributes leading the decl attribute
	 chain.  Return if we find one with the mode we need.  */
      for (;;)
	{
	  if (mode == req_mode)
	    return;

	  if (DECL_ATTRIBUTES (node->decl) != attr)
	    break;

	  DECL_ATTRIBUTES (node->decl) = TREE_CHAIN (attr);
	  attr = get_strub_attr_from_decl (node->decl);
	  if (!attr)
	    break;

	  req_mode = get_strub_mode_from_attr (attr);
	}
    }
  else if (mode == req_mode)
    return;

  strub_set_fndt_mode_to (node->decl, mode, attr);
}

/* Compute and set NODE's strub mode.  */

static void
set_strub_mode (cgraph_node *node)
{
  tree attr = get_strub_attr_from_decl (node->decl);

  if (attr)
    switch (get_strub_mode_from_attr (attr))
      {
	/* These can't have been requested through user attributes, so we must
	   have already gone through them.  */
      case STRUB_WRAPPER:
      case STRUB_WRAPPED:
      case STRUB_INLINABLE:
      case STRUB_AT_CALLS_OPT:
	return;

      case STRUB_DISABLED:
      case STRUB_AT_CALLS:
      case STRUB_INTERNAL:
      case STRUB_CALLABLE:
	break;

      default:
	gcc_unreachable ();
      }

  cgraph_node *xnode = node;
  if (node->alias)
    xnode = node->ultimate_alias_target ();
  /* Weakrefs may remain unresolved (the above will return node) if
     their targets are not defined, so make sure we compute a strub
     mode for them, instead of defaulting to STRUB_DISABLED and
     rendering them uncallable.  */
  enum strub_mode mode = (xnode != node && !xnode->alias
			  ? get_strub_mode (xnode)
			  : compute_strub_mode (node, attr));

  set_strub_mode_to (node, mode);
}


/* Non-strub functions shouldn't be called from within strub contexts,
   except through callable ones.  Always inline strub functions can
   only be called from strub functions.  */

static bool
strub_callable_from_p (strub_mode caller_mode, strub_mode callee_mode)
{
  switch (caller_mode)
    {
    case STRUB_WRAPPED:
    case STRUB_AT_CALLS_OPT:
    case STRUB_AT_CALLS:
    case STRUB_INTERNAL:
    case STRUB_INLINABLE:
      break;

    case STRUB_WRAPPER:
    case STRUB_DISABLED:
    case STRUB_CALLABLE:
      return callee_mode != STRUB_INLINABLE;

    default:
      gcc_unreachable ();
    }

  switch (callee_mode)
    {
    case STRUB_WRAPPED:
    case STRUB_AT_CALLS:
    case STRUB_INLINABLE:
      break;

    case STRUB_AT_CALLS_OPT:
    case STRUB_INTERNAL:
    case STRUB_WRAPPER:
      return (flag_strub >= -1);

    case STRUB_DISABLED:
      return false;

    case STRUB_CALLABLE:
      break;

    default:
      gcc_unreachable ();
    }

  return true;
}

/* Return TRUE iff CALLEE can be inlined into CALLER.  We wish to avoid inlining
   WRAPPED functions back into their WRAPPERs.  More generally, we wish to avoid
   inlining strubbed functions into non-strubbed ones.  CALLER doesn't have to
   be an immediate caller of CALLEE: the immediate caller may have already been
   cloned for inlining, and then CALLER may be further up the original call
   chain.  ???  It would be nice if our own caller would retry inlining callee
   if caller gets inlined.  */

bool
strub_inlinable_to_p (cgraph_node *callee, cgraph_node *caller)
{
  strub_mode callee_mode = get_strub_mode (callee);

  switch (callee_mode)
    {
    case STRUB_WRAPPED:
    case STRUB_AT_CALLS:
    case STRUB_INTERNAL:
    case STRUB_INLINABLE:
    case STRUB_AT_CALLS_OPT:
      break;

    case STRUB_WRAPPER:
    case STRUB_DISABLED:
    case STRUB_CALLABLE:
      /* When we consider inlining, we've already verified callability, so we
	 can even inline callable and then disabled into a strub context.  That
	 will get strubbed along with the context, so it's hopefully not a
	 problem.  */
      return true;

    default:
      gcc_unreachable ();
    }

  strub_mode caller_mode = get_strub_mode (caller);

  switch (caller_mode)
    {
    case STRUB_WRAPPED:
    case STRUB_AT_CALLS:
    case STRUB_INTERNAL:
    case STRUB_INLINABLE:
    case STRUB_AT_CALLS_OPT:
      return true;

    case STRUB_WRAPPER:
    case STRUB_DISABLED:
    case STRUB_CALLABLE:
      break;

    default:
      gcc_unreachable ();
    }

  return false;
}

/* Check that types T1 and T2 are strub-compatible.  Return 1 if the strub modes
   are the same, 2 if they are interchangeable, and 0 otherwise.  */

int
strub_comptypes (tree t1, tree t2)
{
  if (TREE_CODE (t1) != TREE_CODE (t2))
    return 0;

  enum strub_mode m1 = get_strub_mode_from_type (t1);
  enum strub_mode m2 = get_strub_mode_from_type (t2);

  if (m1 == m2)
    return 1;

  /* We're dealing with types, so only strub modes that can be selected by
     attributes in the front end matter.  If either mode is at-calls (for
     functions) or internal (for variables), the conversion is not
     compatible.  */
  bool var_p = !FUNC_OR_METHOD_TYPE_P (t1);
  enum strub_mode mr = var_p ? STRUB_INTERNAL : STRUB_AT_CALLS;
  if (m1 == mr || m2 == mr)
    return 0;

  return 2;
}

/* Return the effective strub mode used for CALL, and set *TYPEP to
   the effective type used for the call.  The effective type and mode
   are those of the callee, unless the call involves a typecast.  */

static enum strub_mode
effective_strub_mode_for_call (gcall *call, tree *typep)
{
  tree type;
  enum strub_mode mode;

  if (strub_call_fntype_override_p (call))
    {
      type = gimple_call_fntype (call);
      mode = get_strub_mode_from_type (type);
    }
  else
    {
      type = TREE_TYPE (TREE_TYPE (gimple_call_fn (call)));
      tree decl = gimple_call_fndecl (call);
      if (decl)
	mode = get_strub_mode_from_fndecl (decl);
      else
	mode = get_strub_mode_from_type (type);
    }

  if (typep)
    *typep = type;

  return mode;
}

/* Create a distinct copy of the type of NODE's function, and change
   the fntype of all calls to it with the same main type to the new
   type.  */

static void
distinctify_node_type (cgraph_node *node)
{
  tree old_type = TREE_TYPE (node->decl);
  tree new_type = build_distinct_type_copy (old_type);
  tree new_ptr_type = NULL_TREE;

  /* Remap any calls to node->decl that use old_type, or a variant
     thereof, to new_type as well.  We don't look for aliases, their
     declarations will have their types changed independently, and
     we'll adjust their fntypes then.  */
  for (cgraph_edge *e = node->callers; e; e = e->next_caller)
    {
      if (!e->call_stmt)
	continue;
      tree fnaddr = gimple_call_fn (e->call_stmt);
      gcc_checking_assert (TREE_CODE (fnaddr) == ADDR_EXPR
			   && TREE_OPERAND (fnaddr, 0) == node->decl);
      if (strub_call_fntype_override_p (e->call_stmt))
	continue;
      if (!new_ptr_type)
	new_ptr_type = build_pointer_type (new_type);
      TREE_TYPE (fnaddr) = new_ptr_type;
      gimple_call_set_fntype (e->call_stmt, new_type);
    }

  TREE_TYPE (node->decl) = new_type;
}

/* Return TRUE iff TYPE and any variants have the same strub mode.  */

static bool
same_strub_mode_in_variants_p (tree type)
{
  enum strub_mode mode = get_strub_mode_from_type (type);

  for (tree other = TYPE_MAIN_VARIANT (type);
       other != NULL_TREE; other = TYPE_NEXT_VARIANT (other))
    if (type != other && mode != get_strub_mode_from_type (other))
      return false;

  /* Check that the canonical type, if set, either is in the same
     variant chain, or has the same strub mode as type.  Also check
     the variants of the canonical type.  */
  if (TYPE_CANONICAL (type)
      && (TYPE_MAIN_VARIANT (TYPE_CANONICAL (type))
	  != TYPE_MAIN_VARIANT (type)))
    {
      if (mode != get_strub_mode_from_type (TYPE_CANONICAL (type)))
	return false;
      else
	return same_strub_mode_in_variants_p (TYPE_CANONICAL (type));
    }

  return true;
}

/* Check that strub functions don't call non-strub functions, and that
   always_inline strub functions are only called by strub
   functions.  */

static void
verify_strub ()
{
  cgraph_node *node;

  /* It's expected that check strub-wise pointer type compatibility of variables
     and of functions is already taken care of by front-ends, on account of the
     attribute's being marked as affecting type identity and of the creation of
     distinct types.  */

  /* Check that call targets in strub contexts have strub-callable types.  */

  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
  {
    enum strub_mode caller_mode = get_strub_mode (node);

    for (cgraph_edge *e = node->indirect_calls; e; e = e->next_callee)
      {
	gcc_checking_assert (e->indirect_unknown_callee);

	if (!e->call_stmt)
	  continue;

	enum strub_mode callee_mode
	  = effective_strub_mode_for_call (e->call_stmt, NULL);

	if (!strub_callable_from_p (caller_mode, callee_mode))
	  error_at (gimple_location (e->call_stmt),
		    "indirect non-%<strub%> call in %<strub%> context %qD",
		    node->decl);
      }

    for (cgraph_edge *e = node->callees; e; e = e->next_callee)
      {
	gcc_checking_assert (!e->indirect_unknown_callee);

	if (!e->call_stmt)
	  continue;

	tree callee_fntype;
	enum strub_mode callee_mode
	  = effective_strub_mode_for_call (e->call_stmt, &callee_fntype);

	if (!strub_callable_from_p (caller_mode, callee_mode))
	  {
	    if (callee_mode == STRUB_INLINABLE)
	      error_at (gimple_location (e->call_stmt),
			"calling %<always_inline%> %<strub%> %qD"
			" in non-%<strub%> context %qD",
			e->callee->decl, node->decl);
	    else if (fndecl_built_in_p (e->callee->decl, BUILT_IN_APPLY_ARGS)
		     && caller_mode == STRUB_INTERNAL)
	      /* This is ok, it will be kept in the STRUB_WRAPPER, and removed
		 from the STRUB_WRAPPED's strub context.  */
	      continue;
	    else if (!strub_call_fntype_override_p (e->call_stmt))
	      error_at (gimple_location (e->call_stmt),
			"calling non-%<strub%> %qD in %<strub%> context %qD",
			e->callee->decl, node->decl);
	    else
	      error_at (gimple_location (e->call_stmt),
			"calling %qD using non-%<strub%> type %qT"
			" in %<strub%> context %qD",
			e->callee->decl, callee_fntype, node->decl);
	  }
      }
  }
}

namespace {

/* Define a pass to compute strub modes.  */
const pass_data pass_data_ipa_strub_mode = {
  SIMPLE_IPA_PASS,
  "strubm",
  OPTGROUP_NONE,
  TV_NONE,
  PROP_cfg, // properties_required
  0,	    // properties_provided
  0,	    // properties_destroyed
  0,	    // properties_start
  0,	    // properties_finish
};

class pass_ipa_strub_mode : public simple_ipa_opt_pass
{
public:
  pass_ipa_strub_mode (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_strub_mode, ctxt)
  {}
  opt_pass *clone () { return new pass_ipa_strub_mode (m_ctxt); }
  virtual bool gate (function *) {
    /* In relaxed (-3) and strict (-4) settings, that only enable strub at a
       function or variable attribute's request, the attribute handler changes
       flag_strub to -1 or -2, respectively, if any strub-enabling occurence of
       the attribute is found.  Therefore, if it remains at -3 or -4, nothing
       that would enable strub was found, so we can disable it and avoid the
       overhead.  */
    if (flag_strub < -2)
      flag_strub = 0;
    return flag_strub;
  }
  virtual unsigned int execute (function *);
};

/* Define a pass to introduce strub transformations.  */
const pass_data pass_data_ipa_strub = {
  SIMPLE_IPA_PASS,
  "strub",
  OPTGROUP_NONE,
  TV_NONE,
  PROP_cfg | PROP_ssa, // properties_required
  0,	    // properties_provided
  0,	    // properties_destroyed
  0,	    // properties_start
  TODO_update_ssa
  | TODO_cleanup_cfg
  | TODO_rebuild_cgraph_edges
  | TODO_verify_il, // properties_finish
};

class pass_ipa_strub : public simple_ipa_opt_pass
{
public:
  pass_ipa_strub (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_strub, ctxt)
  {}
  opt_pass *clone () { return new pass_ipa_strub (m_ctxt); }
  virtual bool gate (function *) { return flag_strub && !seen_error (); }
  virtual unsigned int execute (function *);

  /* Define on demand and cache some types we use often.  */
#define DEF_TYPE(IDX, NAME, INIT)		\
  static inline tree get_ ## NAME () {		\
    int idx = STRUB_TYPE_BASE + IDX;		\
    static tree type = strub_cache[idx];	\
    if (!type)					\
      strub_cache[idx] = type = (INIT);		\
    return type;				\
  }

  /* Use a distinct ptr_type_node to denote the watermark, so that we can
     recognize it in arg lists and avoid modifying types twice.  */
  DEF_TYPE (0, wmt, build_variant_type_copy (ptr_type_node))

  DEF_TYPE (1, pwmt, build_reference_type (get_wmt ()))

  DEF_TYPE (2, qpwmt,
	    build_qualified_type (get_pwmt (),
				  TYPE_QUAL_RESTRICT
				  /* | TYPE_QUAL_CONST */))

  DEF_TYPE (3, qptr,
	    build_qualified_type (ptr_type_node,
				  TYPE_QUAL_RESTRICT
				  | TYPE_QUAL_CONST))

  DEF_TYPE (4, qpvalst,
	    build_qualified_type (build_reference_type
				  (va_list_type_node),
				  TYPE_QUAL_RESTRICT
				  /* | TYPE_QUAL_CONST */))

#undef DEF_TYPE

  /* Define non-strub builtins on demand.  */
#define DEF_NM_BUILTIN(NAME, CODE, FNTYPELIST)			\
  static tree get_ ## NAME () {					\
    tree decl = builtin_decl_explicit (CODE);			\
    if (!decl)							\
      {								\
	tree type = build_function_type_list FNTYPELIST;	\
	decl = add_builtin_function				\
	  ("__builtin_" #NAME,					\
	   type, CODE, BUILT_IN_NORMAL,				\
	   NULL, NULL);						\
	TREE_NOTHROW (decl) = true;				\
	set_builtin_decl ((CODE), decl, true);			\
      }								\
    return decl;						\
  }

  DEF_NM_BUILTIN (stack_address,
		  BUILT_IN_STACK_ADDRESS,
		  (ptr_type_node, NULL))

#undef DEF_NM_BUILTIN

  /* Define strub builtins on demand.  */
#define DEF_SS_BUILTIN(NAME, FNSPEC, CODE, FNTYPELIST)		\
  static tree get_ ## NAME () {					\
    tree decl = builtin_decl_explicit (CODE);			\
    if (!decl)							\
      {								\
	tree type = build_function_type_list FNTYPELIST;	\
	tree attrs = NULL;					\
	if (FNSPEC)						\
	  attrs = tree_cons (get_identifier ("fn spec"),	\
			     build_tree_list			\
			     (NULL_TREE,			\
			      build_string (strlen (FNSPEC),	\
					    (FNSPEC))),		\
			     attrs);				\
	decl = add_builtin_function_ext_scope			\
	  ("__builtin___strub_" #NAME,				\
	   type, CODE, BUILT_IN_NORMAL,				\
	   "__strub_" #NAME, attrs);				\
	TREE_NOTHROW (decl) = true;				\
	set_builtin_decl ((CODE), decl, true);			\
      }								\
    return decl;						\
  }

  DEF_SS_BUILTIN (enter, ". Ot",
		  BUILT_IN___STRUB_ENTER,
		  (void_type_node, get_qpwmt (), NULL))
  DEF_SS_BUILTIN (update, ". Wt",
		  BUILT_IN___STRUB_UPDATE,
		  (void_type_node, get_qpwmt (), NULL))
  DEF_SS_BUILTIN (leave, ". w ",
		  BUILT_IN___STRUB_LEAVE,
		  (void_type_node, get_qpwmt (), NULL))

#undef DEF_SS_BUILTIN

    /* Define strub identifiers on demand.  */
#define DEF_IDENT(IDX, NAME)						\
  static inline tree get_ ## NAME () {					\
    int idx = STRUB_IDENT_BASE + IDX;					\
    tree identifier = strub_cache[idx];					\
    if (!identifier)							\
      strub_cache[idx] = identifier = get_identifier (".strub." #NAME);	\
    return identifier;							\
  }

  DEF_IDENT (0, watermark_ptr)
  DEF_IDENT (1, va_list_ptr)
  DEF_IDENT (2, apply_args)

#undef DEF_IDENT

  static inline int adjust_at_calls_type (tree);
  static inline void adjust_at_calls_call (cgraph_edge *, int, tree);
  static inline void adjust_at_calls_calls (cgraph_node *);

  /* Add to SEQ a call to the strub watermark update builtin, taking NODE's
     location if given.  Optionally add the corresponding edge from NODE, with
     execution frequency COUNT.  Return the modified SEQ.  */

  static inline gimple_seq
  call_update_watermark (tree wmptr, cgraph_node *node, profile_count count,
			 gimple_seq seq = NULL)
    {
      tree uwm = get_update ();
      gcall *update = gimple_build_call (uwm, 1, wmptr);
      if (node)
	gimple_set_location (update, DECL_SOURCE_LOCATION (node->decl));
      gimple_seq_add_stmt (&seq, update);
      if (node)
	node->create_edge (cgraph_node::get_create (uwm), update, count, false);
      return seq;
    }

};

} // anon namespace

/* Gather with this type a collection of parameters that we're turning into
   explicit references.  */

typedef hash_set<tree> indirect_parms_t;

/* Dereference OP's incoming turned-into-reference parm if it's an
   INDIRECT_PARMS or an ADDR_EXPR thereof.  Set *REC and return according to
   gimple-walking expectations.  */

static tree
maybe_make_indirect (indirect_parms_t &indirect_parms, tree op, int *rec)
{
  if (DECL_P (op))
    {
      *rec = 0;
      if (indirect_parms.contains (op))
	{
	  tree ret = gimple_fold_indirect_ref (op);
	  if (!ret)
	    ret = build2 (MEM_REF,
			  TREE_TYPE (TREE_TYPE (op)),
			  op,
			  build_int_cst (TREE_TYPE (op), 0));
	  if (TYPE_VOLATILE (TREE_TYPE (TREE_TYPE (op)))
	      && !TREE_THIS_VOLATILE (ret))
	    TREE_SIDE_EFFECTS (ret) = TREE_THIS_VOLATILE (ret) = 1;
	  return ret;
	}
    }
  else if (TREE_CODE (op) == ADDR_EXPR
	   && DECL_P (TREE_OPERAND (op, 0)))
    {
      *rec = 0;
      if (indirect_parms.contains (TREE_OPERAND (op, 0)))
	{
	  op = TREE_OPERAND (op, 0);
	  return op;
	}
    }

  return NULL_TREE;
}

/* A gimple-walking function that adds dereferencing to indirect parms.  */

static tree
walk_make_indirect (tree *op, int *rec, void *arg)
{
  walk_stmt_info *wi = (walk_stmt_info *)arg;
  indirect_parms_t &indirect_parms = *(indirect_parms_t *)wi->info;

  if (!*op || TYPE_P (*op))
    {
      *rec = 0;
      return NULL_TREE;
    }

  if (tree repl = maybe_make_indirect (indirect_parms, *op, rec))
    {
      *op = repl;
      wi->changed = true;
    }

  return NULL_TREE;
}

/* A gimple-walking function that turns any non-gimple-val ADDR_EXPRs into a
   separate SSA.  Though addresses of e.g. parameters, and of members thereof,
   are gimple vals, turning parameters into references, with an extra layer of
   indirection and thus explicit dereferencing, need to be regimplified.  */

static tree
walk_regimplify_addr_expr (tree *op, int *rec, void *arg)
{
  walk_stmt_info *wi = (walk_stmt_info *)arg;
  gimple_stmt_iterator &gsi = *(gimple_stmt_iterator *)wi->info;

  *rec = 0;

  if (!*op || TREE_CODE (*op) != ADDR_EXPR)
    return NULL_TREE;

  if (!is_gimple_val (*op))
    {
      tree ret = force_gimple_operand_gsi (&gsi, *op, true,
					   NULL_TREE, true, GSI_SAME_STMT);
      gcc_assert (ret != *op);
      *op = ret;
      wi->changed = true;
    }

  return NULL_TREE;
}

/* Turn STMT's PHI arg defs into separate SSA defs if they've become
   non-gimple_val.  Return TRUE if any edge insertions need to be committed.  */

static bool
walk_regimplify_phi (gphi *stmt)
{
  bool needs_commit = false;

  for (unsigned i = 0, n = gimple_phi_num_args (stmt); i < n; i++)
    {
      tree op = gimple_phi_arg_def (stmt, i);
      if ((TREE_CODE (op) == ADDR_EXPR
	   && !is_gimple_val (op))
	  /* ??? A PARM_DECL that was addressable in the original function and
	     had its address in PHI nodes, but that became a reference in the
	     wrapped clone would NOT be updated by update_ssa in PHI nodes.
	     Alas, if we were to create a default def for it now, update_ssa
	     would complain that the symbol that needed rewriting already has
	     SSA names associated with it.  OTOH, leaving the PARM_DECL alone,
	     it eventually causes errors because it remains unchanged in PHI
	     nodes, but it gets rewritten as expected if it appears in other
	     stmts.  So we cheat a little here, and force the PARM_DECL out of
	     the PHI node and into an assignment.  It's a little expensive,
	     because we insert it at the edge, which introduces a basic block
	     that's entirely unnecessary, but it works, and the block will be
	     removed as the default def gets propagated back into the PHI node,
	     so the final optimized code looks just as expected.  */
	  || (TREE_CODE (op) == PARM_DECL
	      && !TREE_ADDRESSABLE (op)))
	{
	  tree temp = make_ssa_name (TREE_TYPE (op), stmt);
	  if (TREE_CODE (op) == PARM_DECL)
	    SET_SSA_NAME_VAR_OR_IDENTIFIER (temp, DECL_NAME (op));
	  SET_PHI_ARG_DEF (stmt, i, temp);

	  gimple *assign = gimple_build_assign (temp, op);
	  if (gimple_phi_arg_has_location (stmt, i))
	    gimple_set_location (assign, gimple_phi_arg_location (stmt, i));
	  gsi_insert_on_edge (gimple_phi_arg_edge (stmt, i), assign);
	  needs_commit = true;
	}
    }

  return needs_commit;
}

/* Create a reference type to use for PARM when turning it into a
   reference.  */

static tree
build_ref_type_for (tree parm)
{
  gcc_checking_assert (TREE_CODE (parm) == PARM_DECL);

  tree ref_type = build_reference_type (TREE_TYPE (parm));

  return ref_type;
}

/* Add cgraph edges from current_function_decl to callees in SEQ with frequency
   COUNT, assuming all calls in SEQ are direct.  */

static void
add_call_edges_for_seq (gimple_seq seq, profile_count count)
{
  cgraph_node *node = cgraph_node::get_create (current_function_decl);

  for (gimple_stmt_iterator gsi = gsi_start (seq);
       !gsi_end_p (gsi); gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);

      gcall *call = dyn_cast <gcall *> (stmt);
      if (!call)
	continue;

      tree callee = gimple_call_fndecl (call);
      gcc_checking_assert (callee);
      node->create_edge (cgraph_node::get_create (callee), call, count, false);
    }
}

/* Insert SEQ after the call at GSI, as if the call was in a try block with SEQ
   as finally, i.e., SEQ will run after the call whether it returns or
   propagates an exception.  This handles block splitting, EH edge and block
   creation, noreturn and nothrow optimizations, and even throwing calls without
   preexisting local handlers.  */

static void
gsi_insert_finally_seq_after_call (gimple_stmt_iterator gsi, gimple_seq seq)
{
  if (!seq)
    return;

  gimple *stmt = gsi_stmt (gsi);

  if (gimple_has_location (stmt))
    annotate_all_with_location (seq, gimple_location (stmt));

  gcall *call = dyn_cast <gcall *> (stmt);
  bool noreturn_p = call && gimple_call_noreturn_p (call);
  int eh_lp = lookup_stmt_eh_lp (stmt);
  bool must_not_throw_p = eh_lp < 0;
  bool nothrow_p = (must_not_throw_p
		    || (call && gimple_call_nothrow_p (call))
		    || (eh_lp <= 0
			&& (TREE_NOTHROW (cfun->decl)
			    || !opt_for_fn (cfun->decl, flag_exceptions))));

  if (noreturn_p && nothrow_p)
    return;

  /* Don't expect an EH edge if we're not to throw, or if we're not in an EH
     region yet.  */
  bool no_eh_edge_p = (nothrow_p || !eh_lp);
  bool must_end_bb = stmt_ends_bb_p (stmt);

  edge eft = NULL, eeh = NULL;
  if (must_end_bb && !(noreturn_p && no_eh_edge_p))
    {
      gcc_checking_assert (gsi_one_before_end_p (gsi));

      edge e;
      edge_iterator ei;
      FOR_EACH_EDGE (e, ei, gsi_bb (gsi)->succs)
	{
	  if ((e->flags & EDGE_EH))
	    {
	      gcc_checking_assert (!eeh);
	      eeh = e;
#if !CHECKING_P
	      if (eft || noreturn_p)
		break;
#endif
	    }
	  if ((e->flags & EDGE_FALLTHRU))
	    {
	      gcc_checking_assert (!eft);
	      eft = e;
#if !CHECKING_P
	      if (eeh || no_eh_edge_p)
		break;
#endif
	    }
	}

      gcc_checking_assert (!(eft && (eft->flags & EDGE_FALLTHRU))
			   == noreturn_p);
      gcc_checking_assert (!(eeh && (eeh->flags & EDGE_EH))
			   == no_eh_edge_p);
      gcc_checking_assert (eft != eeh);
    }

  if (!noreturn_p)
    {
      gimple_seq nseq = nothrow_p ? seq : gimple_seq_copy (seq);

      if (must_end_bb)
	{
	  gcc_checking_assert (gsi_one_before_end_p (gsi));
	  add_call_edges_for_seq (nseq, eft->count ());
	  gsi_insert_seq_on_edge_immediate (eft, nseq);
	}
      else
	{
	  add_call_edges_for_seq (nseq, gsi_bb (gsi)->count);
	  gsi_insert_seq_after (&gsi, nseq, GSI_SAME_STMT);
	}
    }

  if (nothrow_p)
    return;

  if (eh_lp)
    {
      add_call_edges_for_seq (seq, eeh->count ());
      gsi_insert_seq_on_edge_immediate (eeh, seq);
      return;
    }

  /* A throwing call may appear within a basic block in a function that doesn't
     have any EH regions.  We're going to add a cleanup if so, therefore the
     block will have to be split.  */
  basic_block bb = gsi_bb (gsi);
  if (!gsi_one_before_end_p (gsi))
    split_block (bb, stmt);

  /* Create a new block for the EH cleanup.  */
  basic_block bb_eh_cleanup = create_empty_bb (bb);
  if (dom_info_available_p (CDI_DOMINATORS))
    set_immediate_dominator (CDI_DOMINATORS, bb_eh_cleanup, bb);
  if (current_loops)
    add_bb_to_loop (bb_eh_cleanup, current_loops->tree_root);

  /* Make the new block an EH cleanup for the call.  */
  eh_region new_r = gen_eh_region_cleanup (NULL);
  eh_landing_pad lp = gen_eh_landing_pad (new_r);
  tree label = gimple_block_label (bb_eh_cleanup);
  lp->post_landing_pad = label;
  EH_LANDING_PAD_NR (label) = lp->index;
  add_stmt_to_eh_lp (stmt, lp->index);

  /* Add the cleanup code to the EH cleanup block.  */
  gsi = gsi_after_labels (bb_eh_cleanup);
  gsi_insert_seq_before (&gsi, seq, GSI_SAME_STMT);

  /* And then propagate the exception further.  */
  gresx *resx = gimple_build_resx (new_r->index);
  if (gimple_has_location (stmt))
    gimple_set_location (resx, gimple_location (stmt));
  gsi_insert_before (&gsi, resx, GSI_SAME_STMT);

  /* Finally, wire the EH cleanup block into the CFG.  */
  edge neeh = make_eh_edge (stmt);
  neeh->probability = profile_probability::never ();
  gcc_checking_assert (neeh->dest == bb_eh_cleanup);
  gcc_checking_assert (!neeh->dest->count.initialized_p ());
  neeh->dest->count = neeh->count ();
  add_call_edges_for_seq (seq, neeh->dest->count);
}

/* Copy the attribute list at *ATTRS, minus any NAME attributes, leaving
   shareable trailing nodes alone.  */

static inline void
remove_named_attribute_unsharing (const char *name, tree *attrs)
{
  while (tree found = lookup_attribute (name, *attrs))
    {
      /* Copy nodes up to the next NAME attribute.  */
      while (*attrs != found)
	{
	  *attrs = tree_cons (TREE_PURPOSE (*attrs),
			      TREE_VALUE (*attrs),
			      TREE_CHAIN (*attrs));
	  attrs = &TREE_CHAIN (*attrs);
	}
      /* Then drop it.  */
      gcc_checking_assert (*attrs == found);
      *attrs = TREE_CHAIN (*attrs);
    }
}

/* Record the uid of the last cgraph entry whose mode we've already set, so
   that we can perform mode setting incrementally without duplication.  */
static int last_cgraph_uid;

/* Set strub modes for functions introduced since the last call.  */

static void
ipa_strub_set_mode_for_new_functions ()
{
  if (symtab->cgraph_max_uid == last_cgraph_uid)
    return;

  cgraph_node *node;

  /* Go through the functions twice, once over non-aliases, and then over
     aliases, so that aliases can reuse the mode computation of their ultimate
     targets.  */
  for (int aliases = 0; aliases <= 1; aliases++)
    FOR_EACH_FUNCTION (node)
    {
      if (!node->alias != !aliases)
	continue;

      /*  Already done.  */
      if (node->get_uid () < last_cgraph_uid)
	continue;

      set_strub_mode (node);
    }

  last_cgraph_uid = symtab->cgraph_max_uid;
}

/* Return FALSE if NODE is a strub context, and TRUE otherwise.  */

bool
strub_splittable_p (cgraph_node *node)
{
  switch (get_strub_mode (node))
    {
    case STRUB_WRAPPED:
    case STRUB_AT_CALLS:
    case STRUB_AT_CALLS_OPT:
    case STRUB_INLINABLE:
    case STRUB_INTERNAL:
    case STRUB_WRAPPER:
      return false;

    case STRUB_CALLABLE:
    case STRUB_DISABLED:
      break;

    default:
      gcc_unreachable ();
    }

  return true;
}

/* Return the PARM_DECL of the incoming watermark pointer, if there is one.  */

tree
strub_watermark_parm (tree fndecl)
{
  switch (get_strub_mode_from_fndecl (fndecl))
    {
    case STRUB_WRAPPED:
    case STRUB_AT_CALLS:
    case STRUB_AT_CALLS_OPT:
      break;

    case STRUB_INTERNAL:
    case STRUB_WRAPPER:
    case STRUB_CALLABLE:
    case STRUB_DISABLED:
    case STRUB_INLINABLE:
      return NULL_TREE;

    default:
      gcc_unreachable ();
    }

  for (tree parm = DECL_ARGUMENTS (fndecl); parm; parm = DECL_CHAIN (parm))
    /* The type (variant) compare finds the parameter even in a just-created
       clone, before we set its name, but the type-based compare doesn't work
       during builtin expansion within the lto compiler, because we'll have
       created a separate variant in that run.  */
    if (TREE_TYPE (parm) == pass_ipa_strub::get_qpwmt ()
	|| DECL_NAME (parm) == pass_ipa_strub::get_watermark_ptr ())
      return parm;

  gcc_unreachable ();
}

/* Adjust a STRUB_AT_CALLS function TYPE, adding a watermark pointer if it
   hasn't been added yet.  Return the named argument count.  */

int
pass_ipa_strub::adjust_at_calls_type (tree type)
{
  int named_args = 0;

  gcc_checking_assert (same_strub_mode_in_variants_p (type));

  if (!TYPE_ARG_TYPES (type))
    return named_args;

  tree *tlist = &TYPE_ARG_TYPES (type);
  tree qpwmptrt = get_qpwmt ();
  while (*tlist && TREE_VALUE (*tlist) != void_type_node)
    {
      /* The type has already been adjusted.  */
      if (TREE_VALUE (*tlist) == qpwmptrt)
	return named_args;
      named_args++;
      *tlist = tree_cons (TREE_PURPOSE (*tlist),
			  TREE_VALUE (*tlist),
			  TREE_CHAIN (*tlist));
      tlist = &TREE_CHAIN (*tlist);
    }

  /* Add the new argument after all named arguments, so as to not mess with
     attributes that reference parameters.  */
  *tlist = tree_cons (NULL_TREE, get_qpwmt (), *tlist);

#if ATTR_FNSPEC_DECONST_WATERMARK
  if (!type_already_adjusted)
    {
      int flags = flags_from_decl_or_type (type);
      tree fnspec = lookup_attribute ("fn spec", type);

      if ((flags & (ECF_CONST | ECF_PURE | ECF_NOVOPS)) || fnspec)
	{
	  size_t xargs = 1;
	  size_t curlen = 0, tgtlen = 2 + 2 * (named_args + xargs);
	  auto_vec<char> nspecv (tgtlen);
	  char *nspec = &nspecv[0]; /* It will *not* be NUL-terminated!  */
	  if (fnspec)
	    {
	      tree fnspecstr = TREE_VALUE (TREE_VALUE (fnspec));
	      curlen = TREE_STRING_LENGTH (fnspecstr);
	      memcpy (nspec, TREE_STRING_POINTER (fnspecstr), curlen);
	    }
	  if (!curlen)
	    {
	      nspec[curlen++] = '.';
	      nspec[curlen++] = ((flags & ECF_CONST)
				 ? 'c'
				 : (flags & ECF_PURE)
				 ? 'p'
				 : ' ');
	    }
	  while (curlen < tgtlen - 2 * xargs)
	    {
	      nspec[curlen++] = '.';
	      nspec[curlen++] = ' ';
	    }
	  nspec[curlen++] = 'W';
	  nspec[curlen++] = 't';

	  /* The type has already been copied, if needed, before adding
	     parameters.  */
	  TYPE_ATTRIBUTES (type)
	    = tree_cons (get_identifier ("fn spec"),
			 build_tree_list (NULL_TREE,
					  build_string (tgtlen, nspec)),
			 TYPE_ATTRIBUTES (type));
	}
    }
#endif

  return named_args;
}

/* Adjust a call to an at-calls call target.  Create a watermark local variable
   if needed, initialize it before, pass it to the callee according to the
   modified at-calls interface, and release the callee's stack space after the
   call, if not deferred.  If the call is const or pure, arrange for the
   watermark to not be assumed unused or unchanged.  */

void
pass_ipa_strub::adjust_at_calls_call (cgraph_edge *e, int named_args,
				      tree callee_fntype)
{
  gcc_checking_assert (e->call_stmt);
  gcall *ocall = e->call_stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (ocall);

  /* Make sure we haven't modified this call yet.  */
  gcc_checking_assert (!(int (gimple_call_num_args (ocall)) > named_args
			 && (TREE_TYPE (gimple_call_arg (ocall, named_args))
			     == get_pwmt ())));

  tree tsup;
  if (!(tsup = gimple_call_fndecl (ocall)))
    tsup = TREE_TYPE (TREE_TYPE (gimple_call_fn (ocall)));
  if (!strub_target_support_p (tsup, true, gimple_location (ocall)))
    return;

  /* If we're already within a strub context, pass on the incoming watermark
     pointer, and omit the enter and leave calls around the modified call, as an
     optimization, or as a means to satisfy a tail-call requirement.  */
  tree swmp = ((opt_for_fn (e->caller->decl, optimize_size)
		|| opt_for_fn (e->caller->decl, optimize) > 2
		|| gimple_call_must_tail_p (ocall)
		|| (opt_for_fn (e->caller->decl, optimize) == 2
		    && gimple_call_tail_p (ocall)))
	       ? strub_watermark_parm (e->caller->decl)
	       : NULL_TREE);
  bool omit_own_watermark = swmp;
  tree swm = NULL_TREE;
  if (!omit_own_watermark)
    {
      swm = create_tmp_var (get_wmt (), ".strub.watermark");
      TREE_ADDRESSABLE (swm) = true;
      swmp = build1 (ADDR_EXPR, get_pwmt (), swm);

      /* Initialize the watermark before the call.  */
      tree enter = get_enter ();
      gcall *stptr = gimple_build_call (enter, 1,
					unshare_expr (swmp));
      if (gimple_has_location (ocall))
	gimple_set_location (stptr, gimple_location (ocall));
      gsi_insert_before (&gsi, stptr, GSI_SAME_STMT);
      e->caller->create_edge (cgraph_node::get_create (enter),
			      stptr, gsi_bb (gsi)->count, false);
    }


  /* Replace the call with one that passes the swmp argument first.  */
  gcall *wrcall;
  { gcall *stmt = ocall;
    // Mostly copied from gimple_call_copy_skip_args.
    int i = 0;
    int nargs = gimple_call_num_args (stmt);
    auto_vec<tree> vargs (MAX (nargs, named_args) + 1);
    gcall *new_stmt;

    /* pr71109.c calls a prototypeless function, then defines it with
       additional arguments.  It's ill-formed, but after it's inlined,
       it somehow works out.  */
    for (; i < named_args && i < nargs; i++)
      vargs.quick_push (gimple_call_arg (stmt, i));
    for (; i < named_args; i++)
      vargs.quick_push (null_pointer_node);

    vargs.quick_push (unshare_expr (swmp));

    for (; i < nargs; i++)
      vargs.quick_push (gimple_call_arg (stmt, i));

    if (gimple_call_internal_p (stmt))
      gcc_unreachable ();
    else
      new_stmt = gimple_build_call_vec (gimple_call_fn (stmt), vargs);
    gimple_call_set_fntype (new_stmt, callee_fntype);

    if (gimple_call_lhs (stmt))
      gimple_call_set_lhs (new_stmt, gimple_call_lhs (stmt));

    gimple_move_vops (new_stmt, stmt);

    if (gimple_has_location (stmt))
      gimple_set_location (new_stmt, gimple_location (stmt));
    gimple_call_copy_flags (new_stmt, stmt);
    gimple_call_set_chain (new_stmt, gimple_call_chain (stmt));

    gimple_set_modified (new_stmt, true);

    wrcall = new_stmt;
  }

  update_stmt (wrcall);
  gsi_replace (&gsi, wrcall, true);
  cgraph_edge::set_call_stmt (e, wrcall, false);

  /* Insert the strub code after the call.  */
  gimple_seq seq = NULL;

#if !ATTR_FNSPEC_DECONST_WATERMARK
  /* If the call will be assumed to not modify or even read the
     watermark, make it read and modified ourselves.  */
  if ((gimple_call_flags (wrcall)
       & (ECF_CONST | ECF_PURE | ECF_NOVOPS)))
    {
      if (!swm)
	swm = build2 (MEM_REF,
		      TREE_TYPE (TREE_TYPE (swmp)),
		      swmp,
		      build_int_cst (TREE_TYPE (swmp), 0));

      vec<tree, va_gc> *inputs = NULL;
      vec<tree, va_gc> *outputs = NULL;
      vec_safe_push (outputs,
		     build_tree_list
		     (build_tree_list
		      (NULL_TREE, build_string (2, "=m")),
		      unshare_expr (swm)));
      vec_safe_push (inputs,
		     build_tree_list
		     (build_tree_list
		      (NULL_TREE, build_string (1, "m")),
		      unshare_expr (swm)));
      gasm *forcemod = gimple_build_asm_vec ("", inputs, outputs,
					     NULL, NULL);
      gimple_seq_add_stmt (&seq, forcemod);

      /* If the call will be assumed to not even read the watermark,
	 make sure it is already in memory before the call.  */
      if ((gimple_call_flags (wrcall) & ECF_CONST))
	{
	  vec<tree, va_gc> *inputs = NULL;
	  vec_safe_push (inputs,
			 build_tree_list
			 (build_tree_list
			  (NULL_TREE, build_string (1, "m")),
			  unshare_expr (swm)));
	  gasm *force_store = gimple_build_asm_vec ("", inputs, NULL,
						    NULL, NULL);
	  if (gimple_has_location (wrcall))
	    gimple_set_location (force_store, gimple_location (wrcall));
	  gsi_insert_before (&gsi, force_store, GSI_SAME_STMT);
	}
    }
#endif

  if (!omit_own_watermark)
    {
      gcall *sleave = gimple_build_call (get_leave (), 1,
					 unshare_expr (swmp));
      gimple_seq_add_stmt (&seq, sleave);

      gassign *clobber = gimple_build_assign (swm,
					      build_clobber
					      (TREE_TYPE (swm)));
      gimple_seq_add_stmt (&seq, clobber);
    }

  gsi_insert_finally_seq_after_call (gsi, seq);
}

/* Adjust all at-calls calls in NODE. */

void
pass_ipa_strub::adjust_at_calls_calls (cgraph_node *node)
{
  /* Adjust unknown-callee indirect calls with STRUB_AT_CALLS types within
     onode.  */
  if (node->indirect_calls)
    {
      push_cfun (DECL_STRUCT_FUNCTION (node->decl));
      for (cgraph_edge *e = node->indirect_calls; e; e = e->next_callee)
	{
	  gcc_checking_assert (e->indirect_unknown_callee);

	  if (!e->call_stmt)
	    continue;

	  tree callee_fntype;
	  enum strub_mode callee_mode
	    = effective_strub_mode_for_call (e->call_stmt, &callee_fntype);

	  if (callee_mode != STRUB_AT_CALLS
	      && callee_mode != STRUB_AT_CALLS_OPT)
	    continue;

	  int named_args = adjust_at_calls_type (callee_fntype);

	  adjust_at_calls_call (e, named_args, callee_fntype);
	}
      pop_cfun ();
    }

  if (node->callees)
    {
      push_cfun (DECL_STRUCT_FUNCTION (node->decl));
      for (cgraph_edge *e = node->callees; e; e = e->next_callee)
	{
	  gcc_checking_assert (!e->indirect_unknown_callee);

	  if (!e->call_stmt)
	    continue;

	  tree callee_fntype;
	  enum strub_mode callee_mode
	    = effective_strub_mode_for_call (e->call_stmt, &callee_fntype);

	  if (callee_mode != STRUB_AT_CALLS
	      && callee_mode != STRUB_AT_CALLS_OPT)
	    continue;

	  int named_args = adjust_at_calls_type (callee_fntype);

	  adjust_at_calls_call (e, named_args, callee_fntype);
	}
      pop_cfun ();
    }
}

/* The strubm (strub mode) pass computes a strub mode for each function in the
   call graph, and checks, before any inlining, that strub callability
   requirements in effect are satisfied.  */

unsigned int
pass_ipa_strub_mode::execute (function *)
{
  last_cgraph_uid = 0;
  ipa_strub_set_mode_for_new_functions ();

  /* Verify before any inlining or other transformations.  */
  verify_strub ();

  return 0;
}

/* Create a strub mode pass.  */

simple_ipa_opt_pass *
make_pass_ipa_strub_mode (gcc::context *ctxt)
{
  return new pass_ipa_strub_mode (ctxt);
}

/* The strub pass proper adjusts types, signatures, and at-calls calls, and
   splits internal-strub functions.  */

unsigned int
pass_ipa_strub::execute (function *)
{
  cgraph_node *onode;

  ipa_strub_set_mode_for_new_functions ();

  /* First, adjust the signature of at-calls functions.  We adjust types of
     at-calls functions first, so that we don't modify types in place unless
     strub is explicitly requested.  */
  FOR_EACH_FUNCTION (onode)
  {
    enum strub_mode mode = get_strub_mode (onode);

    if (mode == STRUB_AT_CALLS
	|| mode == STRUB_AT_CALLS_OPT)
      {
	/* Create a type variant if strubbing was not explicitly requested in
	   the function type.  */
	if (get_strub_mode_from_type (TREE_TYPE (onode->decl)) != mode)
	  distinctify_node_type (onode);

	int named_args = adjust_at_calls_type (TREE_TYPE (onode->decl));

	/* An external function explicitly declared with strub won't have a
	   body.  Even with implicit at-calls strub, a function may have had its
	   body removed after we selected the mode, and then we have nothing
	   further to do.  */
	if (!onode->has_gimple_body_p ())
	  continue;

	tree *pargs = &DECL_ARGUMENTS (onode->decl);

	/* A noninterposable_alias reuses the same parm decl chain, don't add
	   the parm twice.  */
	bool aliased_parms = (onode->alias && *pargs
			      && DECL_CONTEXT (*pargs) != onode->decl);

	if (aliased_parms)
	  continue;

	for (int i = 0; i < named_args; i++)
	  pargs = &DECL_CHAIN (*pargs);

	tree wmptr = build_decl (DECL_SOURCE_LOCATION (onode->decl),
				 PARM_DECL,
				 get_watermark_ptr (),
				 get_qpwmt ());
	DECL_ARTIFICIAL (wmptr) = 1;
	DECL_ARG_TYPE (wmptr) = get_qpwmt ();
	DECL_CONTEXT (wmptr) = onode->decl;
	TREE_USED (wmptr) = 1;
	DECL_CHAIN (wmptr) = *pargs;
	*pargs = wmptr;

	if (onode->alias)
	  continue;

	cgraph_node *nnode = onode;
	push_cfun (DECL_STRUCT_FUNCTION (nnode->decl));

	{
	  edge e = single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun));
	  gimple_seq seq = call_update_watermark (wmptr, nnode, e->src->count);
	  gsi_insert_seq_on_edge_immediate (e, seq);
	}

	if (DECL_STRUCT_FUNCTION (nnode->decl)->calls_alloca)
	  {
	    basic_block bb;
	    FOR_EACH_BB_FN (bb, cfun)
	      for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
		   !gsi_end_p (gsi); gsi_next (&gsi))
		{
		  gimple *stmt = gsi_stmt (gsi);

		  gcall *call = dyn_cast <gcall *> (stmt);

		  if (!call)
		    continue;

		  if (gimple_alloca_call_p (call))
		    {
		      /* Capture stack growth.  */
		      gimple_seq seq = call_update_watermark (wmptr, NULL,
							      gsi_bb (gsi)
							      ->count);
		      gsi_insert_finally_seq_after_call (gsi, seq);
		    }
		}
	  }

	pop_cfun ();
      }
  }

  FOR_EACH_FUNCTION (onode)
  {
    if (!onode->has_gimple_body_p ())
      continue;

    enum strub_mode mode = get_strub_mode (onode);

    if (mode != STRUB_INTERNAL)
      {
	adjust_at_calls_calls (onode);
	continue;
      }

    bool is_stdarg = calls_builtin_va_start_p (onode);;
    bool apply_args = calls_builtin_apply_args_p (onode);

    vec<ipa_adjusted_param, va_gc> *nparms = NULL;
    unsigned j = 0;
    {
      // The following loop copied from ipa-split.c:split_function.
      for (tree parm = DECL_ARGUMENTS (onode->decl);
	   parm; parm = DECL_CHAIN (parm), j++)
	{
	  ipa_adjusted_param adj = {};
	  adj.op = IPA_PARAM_OP_COPY;
	  adj.base_index = j;
	  adj.prev_clone_index = j;
	  vec_safe_push (nparms, adj);
	}

      if (apply_args)
	{
	  ipa_adjusted_param aaadj = {};
	  aaadj.op = IPA_PARAM_OP_NEW;
	  aaadj.type = get_qptr ();
	  vec_safe_push (nparms, aaadj);
	}

      if (is_stdarg)
	{
	  ipa_adjusted_param vladj = {};
	  vladj.op = IPA_PARAM_OP_NEW;
	  vladj.type = get_qpvalst ();
	  vec_safe_push (nparms, vladj);
	}

      ipa_adjusted_param wmadj = {};
      wmadj.op = IPA_PARAM_OP_NEW;
      wmadj.type = get_qpwmt ();
      vec_safe_push (nparms, wmadj);
    }
    ipa_param_adjustments adj (nparms, -1, false);

    cgraph_node *nnode = onode->create_version_clone_with_body
      (auto_vec<cgraph_edge *> (0),
       NULL, &adj, NULL, NULL, "strub", NULL);

    if (!nnode)
      {
	error_at (DECL_SOURCE_LOCATION (onode->decl),
		  "failed to split %qD for %<strub%>",
		  onode->decl);
	continue;
      }

    onode->split_part = true;
    if (onode->calls_comdat_local)
      nnode->add_to_same_comdat_group (onode);

    set_strub_mode_to (onode, STRUB_WRAPPER);
    set_strub_mode_to (nnode, STRUB_WRAPPED);

    adjust_at_calls_calls (nnode);

    /* Decide which of the wrapped function's parms we want to turn into
       references to the argument passed to the wrapper.  In general, we want to
       copy small arguments, and avoid copying large ones.  Variable-sized array
       lengths given by other arguments, as in 20020210-1.c, would lead to
       problems if passed by value, after resetting the original function and
       dropping the length computation; passing them by reference works.
       DECL_BY_REFERENCE is *not* a substitute for this: it involves copying
       anyway, but performed at the caller.  */
    indirect_parms_t indirect_nparms (3, false);
    unsigned adjust_ftype = 0;
    unsigned named_args = 0;
    for (tree parm = DECL_ARGUMENTS (onode->decl),
	   nparm = DECL_ARGUMENTS (nnode->decl),
	   nparmt = TYPE_ARG_TYPES (TREE_TYPE (nnode->decl));
	 parm;
	 named_args++,
	   parm = DECL_CHAIN (parm),
	   nparm = DECL_CHAIN (nparm),
	   nparmt = nparmt ? TREE_CHAIN (nparmt) : NULL_TREE)
      if (TREE_THIS_VOLATILE (parm)
	  || !(0 /* DECL_BY_REFERENCE (narg) */
	       || is_gimple_reg_type (TREE_TYPE (nparm))
	       || VECTOR_TYPE_P (TREE_TYPE (nparm))
	       || TREE_CODE (TREE_TYPE (nparm)) == COMPLEX_TYPE
	       || (tree_fits_uhwi_p (TYPE_SIZE_UNIT (TREE_TYPE (nparm)))
		   && (tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (nparm)))
		       <= 4 * UNITS_PER_WORD))))
	{
	  /* No point in indirecting pointer types, unless they're
	     volatile.  Presumably they won't ever pass the size-based
	     test above, but check the assumption here, because
	     getting this wrong would mess with attribute access and
	     possibly others.  We deal with fn spec below.  */
	  gcc_checking_assert (!POINTER_TYPE_P (TREE_TYPE (nparm))
			       || TREE_THIS_VOLATILE (parm));

	  indirect_nparms.add (nparm);

	  /* ??? Is there any case in which it is not safe to suggest the parms
	     turned indirect don't alias anything else?  They are distinct,
	     unaliased memory in the wrapper, and the wrapped can't possibly
	     take pointers into them because none of the pointers passed to the
	     wrapper can alias other incoming parameters passed by value, even
	     if with transparent reference, and the wrapper doesn't take any
	     extra parms that could point into wrapper's parms.  So we can
	     probably drop the TREE_ADDRESSABLE and keep the TRUE.  */
	  tree ref_type = build_ref_type_for (nparm);

	  if (TREE_THIS_VOLATILE (nparm)
	      && TYPE_VOLATILE (TREE_TYPE (nparm))
	      && !TYPE_VOLATILE (ref_type))
	    TREE_SIDE_EFFECTS (nparm) = TREE_THIS_VOLATILE (nparm) = 0;
	  DECL_ARG_TYPE (nparm) = TREE_TYPE (nparm) = ref_type;
	  relayout_decl (nparm);
	  TREE_ADDRESSABLE (nparm) = 0;
	  DECL_BY_REFERENCE (nparm) = 0;
	  DECL_NOT_GIMPLE_REG_P (nparm) = 0;
	  /* ??? This avoids mismatches in debug info bind stmts in
	     e.g. a-chahan .  */
	  DECL_ABSTRACT_ORIGIN (nparm) = NULL;

	  if (nparmt)
	    adjust_ftype++;
	}

    /* Also adjust the wrapped function type, if needed.  */
    if (adjust_ftype)
      {
	tree nftype = TREE_TYPE (nnode->decl);

	/* We always add at least one argument at the end of the signature, when
	   cloning the function, so we don't expect to need to duplicate the
	   type here.  */
	gcc_checking_assert (TYPE_ARG_TYPES (nftype)
			     != TYPE_ARG_TYPES (TREE_TYPE (onode->decl)));

	/* Check that fnspec still works for the modified function signature,
	   and drop it otherwise.  */
	bool drop_fnspec = false;
	tree fnspec = lookup_attribute ("fn spec", TYPE_ATTRIBUTES (nftype));
	attr_fnspec spec = fnspec ? attr_fnspec (fnspec) : attr_fnspec ("");

	unsigned retcopy;
	if (!(fnspec && spec.returns_arg (&retcopy)))
	  retcopy = (unsigned) -1;

	unsigned i = 0;
	for (tree nparm = DECL_ARGUMENTS (nnode->decl),
	       nparmt = TYPE_ARG_TYPES (nftype);
	     adjust_ftype > 0;
	     i++, nparm = DECL_CHAIN (nparm), nparmt = TREE_CHAIN (nparmt))
	  if (indirect_nparms.contains (nparm))
	    {
	      TREE_VALUE (nparmt) = TREE_TYPE (nparm);
	      adjust_ftype--;

	      if (fnspec && !drop_fnspec)
		{
		  if (i == retcopy)
		    drop_fnspec = true;
		  else if (spec.arg_specified_p (i))
		    {
		      /* Properties that apply to pointers only must not be
			 present, because we don't make pointers further
			 indirect.  */
		      gcc_checking_assert
			(!spec.arg_max_access_size_given_by_arg_p (i, NULL));
		      gcc_checking_assert (!spec.arg_copied_to_arg_p (i, NULL));

		      /* Any claim of direct access only is invalidated by
			 adding an indirection level.  */
		      if (spec.arg_direct_p (i))
			drop_fnspec = true;

		      /* If there's a claim the argument is not read from, the
			 added indirection invalidates it: if the argument is
			 used at all, then the pointer will necessarily be
			 read.  */
		      if (!spec.arg_maybe_read_p (i)
			  && spec.arg_used_p (i))
			drop_fnspec = true;
		    }
		}
	    }

	/* ??? Maybe we could adjust it instead.  Note we don't need
	   to mess with attribute access: pointer-typed parameters are
	   not modified, so they can remain unchanged.  */
	if (drop_fnspec)
	  remove_named_attribute_unsharing ("fn spec",
					    &TYPE_ATTRIBUTES (nftype));

	TREE_TYPE (nnode->decl) = nftype;
      }

#if ATTR_FNSPEC_DECONST_WATERMARK
    {
      int flags = flags_from_decl_or_type (nnode->decl);
      tree fnspec = lookup_attribute ("fn spec", TREE_TYPE (nnode->decl));

      if ((flags & (ECF_CONST | ECF_PURE | ECF_NOVOPS)) || fnspec)
	{
	  size_t xargs = 1 + int (is_stdarg) + int (apply_args);
	  size_t curlen = 0, tgtlen = 2 + 2 * (named_args + xargs);
	  auto_vec<char> nspecv (tgtlen);
	  char *nspec = &nspecv[0]; /* It will *not* be NUL-terminated!  */
	  bool no_writes_p = true;
	  if (fnspec)
	    {
	      tree fnspecstr = TREE_VALUE (TREE_VALUE (fnspec));
	      curlen = TREE_STRING_LENGTH (fnspecstr);
	      memcpy (nspec, TREE_STRING_POINTER (fnspecstr), curlen);
	      if (!(flags & (ECF_CONST | ECF_PURE | ECF_NOVOPS))
		  && curlen >= 2
		  && nspec[1] != 'c' && nspec[1] != 'C'
		  && nspec[1] != 'p' && nspec[1] != 'P')
		no_writes_p = false;
	    }
	  if (!curlen)
	    {
	      nspec[curlen++] = '.';
	      nspec[curlen++] = ((flags & ECF_CONST)
				 ? 'c'
				 : (flags & ECF_PURE)
				 ? 'p'
				 : ' ');
	    }
	  while (curlen < tgtlen - 2 * xargs)
	    {
	      nspec[curlen++] = '.';
	      nspec[curlen++] = ' ';
	    }

	  /* These extra args are unlikely to be present in const or pure
	     functions.  It's conceivable that a function that takes variable
	     arguments, or that passes its arguments on to another function,
	     could be const or pure, but it would not modify the arguments, and,
	     being pure or const, it couldn't possibly modify or even access
	     memory referenced by them.  But it can read from these internal
	     data structures created by the wrapper, and from any
	     argument-passing memory referenced by them, so we denote the
	     possibility of reading from multiple levels of indirection, but
	     only of reading because const/pure.  */
	  if (apply_args)
	    {
	      nspec[curlen++] = 'r';
	      nspec[curlen++] = ' ';
	    }
	  if (is_stdarg)
	    {
	      nspec[curlen++] = (no_writes_p ? 'r' : '.');
	      nspec[curlen++] = (no_writes_p ? 't' : ' ');
	    }

	  nspec[curlen++] = 'W';
	  nspec[curlen++] = 't';

	  /* The type has already been copied before adding parameters.  */
	  gcc_checking_assert (TYPE_ARG_TYPES (TREE_TYPE (nnode->decl))
			       != TYPE_ARG_TYPES (TREE_TYPE (onode->decl)));
	  TYPE_ATTRIBUTES (TREE_TYPE (nnode->decl))
	    = tree_cons (get_identifier ("fn spec"),
			 build_tree_list (NULL_TREE,
					  build_string (tgtlen, nspec)),
			 TYPE_ATTRIBUTES (TREE_TYPE (nnode->decl)));
	}
    }
#endif

    {
      tree decl = onode->decl;
      cgraph_node *target = nnode;

      { // copied from create_wrapper

	/* Preserve DECL_RESULT so we get right by reference flag.  */
	tree decl_result = DECL_RESULT (decl);

	/* Remove the function's body but keep arguments to be reused
	   for thunk.  */
	onode->release_body (true);
	onode->reset (/* unlike create_wrapper: preserve_comdat_group = */true);

	DECL_UNINLINABLE (decl) = false;
	DECL_RESULT (decl) = decl_result;
	DECL_INITIAL (decl) = NULL;
	allocate_struct_function (decl, false);
	set_cfun (NULL);

	/* Turn alias into thunk and expand it into GIMPLE representation.  */
	onode->definition = true;

	thunk_info::get_create (onode);
	onode->thunk = true;
	onode->create_edge (target, NULL, onode->count);
	onode->callees->can_throw_external = !TREE_NOTHROW (target->decl);

	tree arguments = DECL_ARGUMENTS (decl);

	while (arguments)
	  {
	    TREE_ADDRESSABLE (arguments) = false;
	    arguments = TREE_CHAIN (arguments);
	  }

	{
	  tree alias = onode->callees->callee->decl;
	  tree thunk_fndecl = decl;
	  tree a;

	  int nxargs = 1 + is_stdarg + apply_args;

	  { // Simplified from expand_thunk.
	    tree restype;
	    basic_block bb, then_bb, else_bb, return_bb;
	    gimple_stmt_iterator bsi;
	    int nargs = 0;
	    tree arg;
	    int i;
	    tree resdecl;
	    tree restmp = NULL;

	    gcall *call;
	    greturn *ret;
	    bool alias_is_noreturn = TREE_THIS_VOLATILE (alias);

	    a = DECL_ARGUMENTS (thunk_fndecl);

	    current_function_decl = thunk_fndecl;

	    /* Ensure thunks are emitted in their correct sections.  */
	    resolve_unique_section (thunk_fndecl, 0,
				    flag_function_sections);

	    bitmap_obstack_initialize (NULL);

	    /* Build the return declaration for the function.  */
	    restype = TREE_TYPE (TREE_TYPE (thunk_fndecl));
	    if (DECL_RESULT (thunk_fndecl) == NULL_TREE)
	      {
		resdecl = build_decl (input_location, RESULT_DECL, 0, restype);
		DECL_ARTIFICIAL (resdecl) = 1;
		DECL_IGNORED_P (resdecl) = 1;
		DECL_CONTEXT (resdecl) = thunk_fndecl;
		DECL_RESULT (thunk_fndecl) = resdecl;
	      }
	    else
	      resdecl = DECL_RESULT (thunk_fndecl);

	    profile_count cfg_count = onode->count;
	    if (!cfg_count.initialized_p ())
	      cfg_count = profile_count::from_gcov_type (BB_FREQ_MAX).guessed_local ();

	    bb = then_bb = else_bb = return_bb
	      = init_lowered_empty_function (thunk_fndecl, true, cfg_count);

	    bsi = gsi_start_bb (bb);

	    /* Build call to the function being thunked.  */
	    if (!VOID_TYPE_P (restype)
		&& (!alias_is_noreturn
		    || TREE_ADDRESSABLE (restype)
		    || TREE_CODE (TYPE_SIZE_UNIT (restype)) != INTEGER_CST))
	      {
		if (DECL_BY_REFERENCE (resdecl))
		  {
		    restmp = gimple_fold_indirect_ref (resdecl);
		    if (!restmp)
		      restmp = build2 (MEM_REF,
				       TREE_TYPE (TREE_TYPE (resdecl)),
				       resdecl,
				       build_int_cst (TREE_TYPE (resdecl), 0));
		  }
		else if (aggregate_value_p (resdecl, TREE_TYPE (thunk_fndecl)))
		  {
		    restmp = resdecl;

		    if (VAR_P (restmp))
		      {
			add_local_decl (cfun, restmp);
			BLOCK_VARS (DECL_INITIAL (current_function_decl))
			  = restmp;
		      }
		  }
		else
		  restmp = create_tmp_reg (restype, "retval");
	      }

	    for (arg = a; arg; arg = DECL_CHAIN (arg))
	      nargs++;
	    auto_vec<tree> vargs (nargs + nxargs);
	    i = 0;
	    arg = a;

	    if (nargs)
	      for (tree nparm = DECL_ARGUMENTS (nnode->decl);
		   i < nargs;
		   i++, arg = DECL_CHAIN (arg), nparm = DECL_CHAIN (nparm))
		{
		  tree save_arg = arg;

		  /* Arrange to pass indirectly the parms, if we decided to do
		     so, and revert its type in the wrapper.  */
		  if (indirect_nparms.contains (nparm))
		    {
		      tree ref_type = TREE_TYPE (nparm);
		      TREE_ADDRESSABLE (arg) = true;
		      arg = build1 (ADDR_EXPR, ref_type, arg);
		    }
		  else if (!TREE_THIS_VOLATILE (arg))
		    DECL_NOT_GIMPLE_REG_P (arg) = 0;

		  /* Convert the argument back to the type used by the calling
		     conventions, e.g. a non-prototyped float type is passed as
		     double, as in 930603-1.c, and needs to be converted back to
		     double to be passed on unchanged to the wrapped
		     function.  */
		  if (TREE_TYPE (nparm) != DECL_ARG_TYPE (nparm))
		    {
		      tree tmp = arg;
		      /* If ARG is e.g. volatile, we must copy and
			 convert in separate statements.  */
		      if (!is_gimple_val (arg))
			{
			  tmp = create_tmp_reg (TYPE_MAIN_VARIANT
						(TREE_TYPE (arg)), "arg");
			  gimple *stmt = gimple_build_assign (tmp, arg);
			  gsi_insert_after (&bsi, stmt, GSI_NEW_STMT);
			}
		      arg = fold_convert (DECL_ARG_TYPE (nparm), tmp);
		    }

		  if (!is_gimple_val (arg))
		    {
		      tree tmp = create_tmp_reg (TYPE_MAIN_VARIANT
						 (TREE_TYPE (arg)), "arg");
		      gimple *stmt = gimple_build_assign (tmp, arg);
		      gsi_insert_after (&bsi, stmt, GSI_NEW_STMT);
		      arg = tmp;
		    }
		  vargs.quick_push (arg);
		  arg = save_arg;
		}
	    /* These strub arguments are adjusted later.  */
	    if (apply_args)
	      vargs.quick_push (null_pointer_node);
	    if (is_stdarg)
	      vargs.quick_push (null_pointer_node);
	    vargs.quick_push (null_pointer_node);
	    call = gimple_build_call_vec (build_fold_addr_expr_loc (0, alias),
					  vargs);
	    onode->callees->call_stmt = call;
	    // gimple_call_set_from_thunk (call, true);
	    if (DECL_STATIC_CHAIN (alias))
	      {
		tree p = DECL_STRUCT_FUNCTION (alias)->static_chain_decl;
		tree type = TREE_TYPE (p);
		tree decl = build_decl (DECL_SOURCE_LOCATION (thunk_fndecl),
					PARM_DECL, create_tmp_var_name ("CHAIN"),
					type);
		DECL_ARTIFICIAL (decl) = 1;
		DECL_IGNORED_P (decl) = 1;
		TREE_USED (decl) = 1;
		DECL_CONTEXT (decl) = thunk_fndecl;
		DECL_ARG_TYPE (decl) = type;
		TREE_READONLY (decl) = 1;

		struct function *sf = DECL_STRUCT_FUNCTION (thunk_fndecl);
		sf->static_chain_decl = decl;

		gimple_call_set_chain (call, decl);
	      }

	    /* Return slot optimization is always possible and in fact required to
	       return values with DECL_BY_REFERENCE.  */
	    if (aggregate_value_p (resdecl, TREE_TYPE (thunk_fndecl))
		&& (!is_gimple_reg_type (TREE_TYPE (resdecl))
		    || DECL_BY_REFERENCE (resdecl)))
	      gimple_call_set_return_slot_opt (call, true);

	    if (restmp)
	      {
		gimple_call_set_lhs (call, restmp);
		gcc_assert (useless_type_conversion_p (TREE_TYPE (restmp),
						       TREE_TYPE (TREE_TYPE (alias))));
	      }
	    gsi_insert_after (&bsi, call, GSI_NEW_STMT);
	    if (!alias_is_noreturn)
	      {
		/* Build return value.  */
		if (!DECL_BY_REFERENCE (resdecl))
		  ret = gimple_build_return (restmp);
		else
		  ret = gimple_build_return (resdecl);

		gsi_insert_after (&bsi, ret, GSI_NEW_STMT);
	      }
	    else
	      {
		remove_edge (single_succ_edge (bb));
	      }

	    cfun->gimple_df->in_ssa_p = true;
	    update_max_bb_count ();
	    profile_status_for_fn (cfun)
	      = cfg_count.initialized_p () && cfg_count.ipa_p ()
	      ? PROFILE_READ : PROFILE_GUESSED;
	    /* FIXME: C++ FE should stop setting TREE_ASM_WRITTEN on thunks.  */
	    // TREE_ASM_WRITTEN (thunk_fndecl) = false;
	    delete_unreachable_blocks ();
	    update_ssa (TODO_update_ssa);
	    checking_verify_flow_info ();
	    free_dominance_info (CDI_DOMINATORS);

	    /* Since we want to emit the thunk, we explicitly mark its name as
	       referenced.  */
	    onode->thunk = false;
	    onode->lowered = true;
	    bitmap_obstack_release (NULL);
	  }
	  current_function_decl = NULL;
	  set_cfun (NULL);
	}

	thunk_info::remove (onode);

	// some more of create_wrapper at the end of the next block.
      }
    }

    {
      tree aaval = NULL_TREE;
      tree vaptr = NULL_TREE;
      tree wmptr = NULL_TREE;
      for (tree arg = DECL_ARGUMENTS (nnode->decl); arg; arg = DECL_CHAIN (arg))
	{
	  aaval = vaptr;
	  vaptr = wmptr;
	  wmptr = arg;
	}

      if (!apply_args)
	aaval = NULL_TREE;
      /* The trailing args are [apply_args], [va_list_ptr], and
	 watermark.  If we don't have a va_list_ptr, the penultimate
	 argument is apply_args.
       */
      else if (!is_stdarg)
	aaval = vaptr;

      if (!is_stdarg)
	vaptr = NULL_TREE;

      DECL_NAME (wmptr) = get_watermark_ptr ();
      DECL_ARTIFICIAL (wmptr) = 1;
      DECL_IGNORED_P (wmptr) = 1;
      TREE_USED (wmptr) = 1;

      if (is_stdarg)
	{
	  DECL_NAME (vaptr) = get_va_list_ptr ();
	  DECL_ARTIFICIAL (vaptr) = 1;
	  DECL_IGNORED_P (vaptr) = 1;
	  TREE_USED (vaptr) = 1;
	}

      if (apply_args)
	{
	  DECL_NAME (aaval) = get_apply_args ();
	  DECL_ARTIFICIAL (aaval) = 1;
	  DECL_IGNORED_P (aaval) = 1;
	  TREE_USED (aaval) = 1;
	}

      push_cfun (DECL_STRUCT_FUNCTION (nnode->decl));

      {
	edge e = single_succ_edge (ENTRY_BLOCK_PTR_FOR_FN (cfun));
	gimple_seq seq = call_update_watermark (wmptr, nnode, e->src->count);
	gsi_insert_seq_on_edge_immediate (e, seq);
      }

      bool any_indirect = !indirect_nparms.is_empty ();

      if (any_indirect)
	{
	  basic_block bb;
	  bool needs_commit = false;
	  FOR_EACH_BB_FN (bb, cfun)
	    {
	      for (gphi_iterator gsi = gsi_start_nonvirtual_phis (bb);
		   !gsi_end_p (gsi);
		   gsi_next_nonvirtual_phi (&gsi))
		{
		  gphi *stmt = gsi.phi ();

		  walk_stmt_info wi = {};
		  wi.info = &indirect_nparms;
		  walk_gimple_op (stmt, walk_make_indirect, &wi);
		  if (wi.changed && !is_gimple_debug (gsi_stmt (gsi)))
		    if (walk_regimplify_phi (stmt))
		      needs_commit = true;
		}

	      for (gimple_stmt_iterator gsi = gsi_start_bb (bb);
		   !gsi_end_p (gsi); gsi_next (&gsi))
		{
		  gimple *stmt = gsi_stmt (gsi);

		  walk_stmt_info wi = {};
		  wi.info = &indirect_nparms;
		  walk_gimple_op (stmt, walk_make_indirect, &wi);
		  if (wi.changed)
		    {
		      if (!is_gimple_debug (stmt))
			{
			  wi.info = &gsi;
			  walk_gimple_op (stmt, walk_regimplify_addr_expr,
					  &wi);
			}
		      update_stmt (stmt);
		    }
		}
	    }
	  if (needs_commit)
	    gsi_commit_edge_inserts ();
	}

      if (DECL_STRUCT_FUNCTION (nnode->decl)->calls_alloca
	  || is_stdarg || apply_args)
	for (cgraph_edge *e = nnode->callees, *enext; e; e = enext)
	  {
	    if (!e->call_stmt)
	      continue;

	    gcall *call = e->call_stmt;
	    gimple_stmt_iterator gsi = gsi_for_stmt (call);
	    tree fndecl = e->callee->decl;

	    enext = e->next_callee;

	    if (gimple_alloca_call_p (call))
	      {
		gimple_seq seq = call_update_watermark (wmptr, NULL,
							gsi_bb (gsi)->count);
		gsi_insert_finally_seq_after_call (gsi, seq);
	      }
	    else if (fndecl && is_stdarg
		     && fndecl_built_in_p (fndecl, BUILT_IN_VA_START))
	      {
		/* Using a non-default stdarg ABI makes the function ineligible
		   for internal strub.  */
		gcc_checking_assert (builtin_decl_explicit (BUILT_IN_VA_START)
				     == fndecl);
		tree bvacopy = builtin_decl_explicit (BUILT_IN_VA_COPY);
		gimple_call_set_fndecl (call, bvacopy);
		tree arg = vaptr;
		/* The va_copy source must be dereferenced, unless it's an array
		   type, that would have decayed to a pointer.  */
		if (TREE_CODE (TREE_TYPE (TREE_TYPE (vaptr))) != ARRAY_TYPE)
		  {
		    arg = gimple_fold_indirect_ref (vaptr);
		    if (!arg)
		      arg = build2 (MEM_REF,
				    TREE_TYPE (TREE_TYPE (vaptr)),
				    vaptr,
				    build_int_cst (TREE_TYPE (vaptr), 0));
		    if (!is_gimple_val (arg))
		      arg = force_gimple_operand_gsi (&gsi, arg, true,
						      NULL_TREE, true, GSI_SAME_STMT);
		  }
		gimple_call_set_arg (call, 1, arg);
		update_stmt (call);
		e->redirect_callee (cgraph_node::get_create (bvacopy));
	      }
	    else if (fndecl && apply_args
		     && fndecl_built_in_p (fndecl, BUILT_IN_APPLY_ARGS))
	      {
		tree lhs = gimple_call_lhs (call);
		gimple *assign = (lhs
				  ? gimple_build_assign (lhs, aaval)
				  : gimple_build_nop ());
		gsi_replace (&gsi, assign, true);
		cgraph_edge::remove (e);
	      }
	  }

      { // a little more copied from create_wrapper

	/* Inline summary set-up.  */
	nnode->analyze ();
	// inline_analyze_function (nnode);
      }

      pop_cfun ();
    }

    {
      push_cfun (DECL_STRUCT_FUNCTION (onode->decl));
      gimple_stmt_iterator gsi
	= gsi_after_labels (single_succ (ENTRY_BLOCK_PTR_FOR_FN (cfun)));

      gcall *wrcall;
      while (!(wrcall = dyn_cast <gcall *> (gsi_stmt (gsi))))
	gsi_next (&gsi);

      tree swm = create_tmp_var (get_wmt (), ".strub.watermark");
      TREE_ADDRESSABLE (swm) = true;
      tree swmp = build1 (ADDR_EXPR, get_pwmt (), swm);

      tree enter = get_enter ();
      gcall *stptr = gimple_build_call (enter, 1, unshare_expr (swmp));
      gimple_set_location (stptr, gimple_location (wrcall));
      gsi_insert_before (&gsi, stptr, GSI_SAME_STMT);
      onode->create_edge (cgraph_node::get_create (enter),
			  stptr, gsi_bb (gsi)->count, false);

      int nargs = gimple_call_num_args (wrcall);

      gimple_seq seq = NULL;

      if (apply_args)
	{
	  tree aalst = create_tmp_var (ptr_type_node, ".strub.apply_args");
	  tree bappargs = builtin_decl_explicit (BUILT_IN_APPLY_ARGS);
	  gcall *appargs = gimple_build_call (bappargs, 0);
	  gimple_call_set_lhs (appargs, aalst);
	  gimple_set_location (appargs, gimple_location (wrcall));
	  gsi_insert_before (&gsi, appargs, GSI_SAME_STMT);
	  gimple_call_set_arg (wrcall, nargs - 2 - is_stdarg, aalst);
	  onode->create_edge (cgraph_node::get_create (bappargs),
			      appargs, gsi_bb (gsi)->count, false);
	}

      if (is_stdarg)
	{
	  tree valst = create_tmp_var (va_list_type_node, ".strub.va_list");
	  TREE_ADDRESSABLE (valst) = true;
	  tree vaptr = build1 (ADDR_EXPR,
			       build_pointer_type (va_list_type_node),
			       valst);
	  gimple_call_set_arg (wrcall, nargs - 2, unshare_expr (vaptr));

	  tree bvastart = builtin_decl_explicit (BUILT_IN_VA_START);
	  gcall *vastart = gimple_build_call (bvastart, 2,
					      unshare_expr (vaptr),
					      integer_zero_node);
	  gimple_set_location (vastart, gimple_location (wrcall));
	  gsi_insert_before (&gsi, vastart, GSI_SAME_STMT);
	  onode->create_edge (cgraph_node::get_create (bvastart),
			      vastart, gsi_bb (gsi)->count, false);

	  tree bvaend = builtin_decl_explicit (BUILT_IN_VA_END);
	  gcall *vaend = gimple_build_call (bvaend, 1, unshare_expr (vaptr));
	  gimple_set_location (vaend, gimple_location (wrcall));
	  gimple_seq_add_stmt (&seq, vaend);
	}

      gimple_call_set_arg (wrcall, nargs - 1, unshare_expr (swmp));
      // gimple_call_set_tail (wrcall, false);
      update_stmt (wrcall);

      {
#if !ATTR_FNSPEC_DECONST_WATERMARK
	/* If the call will be assumed to not modify or even read the
	   watermark, make it read and modified ourselves.  */
	if ((gimple_call_flags (wrcall)
	     & (ECF_CONST | ECF_PURE | ECF_NOVOPS)))
	  {
	    vec<tree, va_gc> *inputs = NULL;
	    vec<tree, va_gc> *outputs = NULL;
	    vec_safe_push (outputs,
			   build_tree_list
			   (build_tree_list
			    (NULL_TREE, build_string (2, "=m")),
			    swm));
	    vec_safe_push (inputs,
			   build_tree_list
			   (build_tree_list
			    (NULL_TREE, build_string (1, "m")),
			    swm));
	    gasm *forcemod = gimple_build_asm_vec ("", inputs, outputs,
						   NULL, NULL);
	    gimple_seq_add_stmt (&seq, forcemod);

	    /* If the call will be assumed to not even read the watermark,
	       make sure it is already in memory before the call.  */
	    if ((gimple_call_flags (wrcall) & ECF_CONST))
	      {
		vec<tree, va_gc> *inputs = NULL;
		vec_safe_push (inputs,
			       build_tree_list
			       (build_tree_list
				(NULL_TREE, build_string (1, "m")),
				swm));
		gasm *force_store = gimple_build_asm_vec ("", inputs, NULL,
							  NULL, NULL);
		gimple_set_location (force_store, gimple_location (wrcall));
		gsi_insert_before (&gsi, force_store, GSI_SAME_STMT);
	      }
	  }
#endif

	gcall *sleave = gimple_build_call (get_leave (), 1,
					   unshare_expr (swmp));
	gimple_seq_add_stmt (&seq, sleave);

	gassign *clobber = gimple_build_assign (swm,
						build_clobber
						(TREE_TYPE (swm)));
	gimple_seq_add_stmt (&seq, clobber);
      }

      gsi_insert_finally_seq_after_call (gsi, seq);

      /* For nnode, we don't rebuild edges because we wish to retain
	 any redirections copied to it from earlier passes, so we add
	 call graph edges explicitly there, but for onode, we create a
	 fresh function, so we may as well just issue the calls and
	 then rebuild all cgraph edges.  */
      // cgraph_edge::rebuild_edges ();
      onode->analyze ();
      // inline_analyze_function (onode);

      pop_cfun ();
    }
  }

  return 0;
}

simple_ipa_opt_pass *
make_pass_ipa_strub (gcc::context *ctxt)
{
  return new pass_ipa_strub (ctxt);
}

#include "gt-ipa-strub.h"
