/* Report error messages, build initializers, and perform
   some front-end optimizations for C++ compiler.
   Copyright (C) 1987-2016 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@cygnus.com)

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


/* This file is part of the C++ front end.
   It contains routines to build C++ expressions given their operands,
   including computing the types of the result, C and C++ specific error
   checks, and some optimization.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "cp-tree.h"
#include "stor-layout.h"
#include "varasm.h"
#include "intl.h"

static tree
process_init_constructor (tree type, tree init, tsubst_flags_t complain);


/* Print an error message stemming from an attempt to use
   BASETYPE as a base class for TYPE.  */

tree
error_not_base_type (tree basetype, tree type)
{
  if (TREE_CODE (basetype) == FUNCTION_DECL)
    basetype = DECL_CONTEXT (basetype);
  error ("type %qT is not a base type for type %qT", basetype, type);
  return error_mark_node;
}

tree
binfo_or_else (tree base, tree type)
{
  tree binfo = lookup_base (type, base, ba_unique,
			    NULL, tf_warning_or_error);

  if (binfo == error_mark_node)
    return NULL_TREE;
  else if (!binfo)
    error_not_base_type (base, type);
  return binfo;
}

/* According to ARM $7.1.6, "A `const' object may be initialized, but its
   value may not be changed thereafter.  */

void
cxx_readonly_error (tree arg, enum lvalue_use errstring)
{
 
/* This macro is used to emit diagnostics to ensure that all format
   strings are complete sentences, visible to gettext and checked at
   compile time.  */
 
#define ERROR_FOR_ASSIGNMENT(AS, ASM, IN, DE, ARG)                      \
  do {                                                                  \
    switch (errstring)                                                  \
      {                                                                 \
      case lv_assign:							\
        error(AS, ARG);                                                 \
        break;                                                          \
      case lv_asm:							\
        error(ASM, ARG);                                                \
        break;                                                          \
      case lv_increment:						\
        error (IN, ARG);                                                \
        break;                                                          \
      case lv_decrement:                                               \
        error (DE, ARG);                                                \
        break;                                                          \
      default:                                                          \
        gcc_unreachable ();                                             \
      }                                                                 \
  } while (0)

  /* Handle C++-specific things first.  */

  if (VAR_P (arg)
      && DECL_LANG_SPECIFIC (arg)
      && DECL_IN_AGGR_P (arg)
      && !TREE_STATIC (arg))
    ERROR_FOR_ASSIGNMENT (G_("assignment of "
			     "constant field %qD"),
			  G_("constant field %qD "
			     "used as %<asm%> output"),
			  G_("increment of "
			     "constant field %qD"),
			  G_("decrement of "
			     "constant field %qD"),
			  arg);
  else if (INDIRECT_REF_P (arg)
	   && TREE_CODE (TREE_TYPE (TREE_OPERAND (arg, 0))) == REFERENCE_TYPE
	   && (VAR_P (TREE_OPERAND (arg, 0))
	       || TREE_CODE (TREE_OPERAND (arg, 0)) == PARM_DECL))
    ERROR_FOR_ASSIGNMENT (G_("assignment of "
                             "read-only reference %qD"),
                          G_("read-only reference %qD "
			     "used as %<asm%> output"), 
                          G_("increment of "
                             "read-only reference %qD"),
                          G_("decrement of "
                             "read-only reference %qD"),
                          TREE_OPERAND (arg, 0));
  else
    readonly_error (input_location, arg, errstring);
}


/* Structure that holds information about declarations whose type was
   incomplete and we could not check whether it was abstract or not.  */

struct GTY((chain_next ("%h.next"), for_user)) pending_abstract_type {
  /* Declaration which we are checking for abstractness. It is either
     a DECL node, or an IDENTIFIER_NODE if we do not have a full
     declaration available.  */
  tree decl;

  /* Type which will be checked for abstractness.  */
  tree type;

  /* Kind of use in an unnamed declarator.  */
  enum abstract_class_use use;

  /* Position of the declaration. This is only needed for IDENTIFIER_NODEs,
     because DECLs already carry locus information.  */
  location_t locus;

  /* Link to the next element in list.  */
  struct pending_abstract_type* next;
};

struct abstract_type_hasher : ggc_ptr_hash<pending_abstract_type>
{
  typedef tree compare_type;
  static hashval_t hash (pending_abstract_type *);
  static bool equal (pending_abstract_type *, tree);
};

/* Compute the hash value of the node VAL. This function is used by the
   hash table abstract_pending_vars.  */

hashval_t
abstract_type_hasher::hash (pending_abstract_type *pat)
{
  return (hashval_t) TYPE_UID (pat->type);
}


/* Compare node VAL1 with the type VAL2. This function is used by the
   hash table abstract_pending_vars.  */

bool
abstract_type_hasher::equal (pending_abstract_type *pat1, tree type2)
{
  return (pat1->type == type2);
}

/* Hash table that maintains pending_abstract_type nodes, for which we still
   need to check for type abstractness.  The key of the table is the type
   of the declaration.  */
static GTY (()) hash_table<abstract_type_hasher> *abstract_pending_vars = NULL;

static int abstract_virtuals_error_sfinae (tree, tree, abstract_class_use, tsubst_flags_t);

/* This function is called after TYPE is completed, and will check if there
   are pending declarations for which we still need to verify the abstractness
   of TYPE, and emit a diagnostic (through abstract_virtuals_error) if TYPE
   turned out to be incomplete.  */

void
complete_type_check_abstract (tree type)
{
  struct pending_abstract_type *pat;
  location_t cur_loc = input_location;

  gcc_assert (COMPLETE_TYPE_P (type));

  if (!abstract_pending_vars)
    return;

  /* Retrieve the list of pending declarations for this type.  */
  pending_abstract_type **slot
    = abstract_pending_vars->find_slot_with_hash (type, TYPE_UID (type),
						  NO_INSERT);
  if (!slot)
    return;
  pat = *slot;
  gcc_assert (pat);

  /* If the type is not abstract, do not do anything.  */
  if (CLASSTYPE_PURE_VIRTUALS (type))
    {
      struct pending_abstract_type *prev = 0, *next;

      /* Reverse the list to emit the errors in top-down order.  */
      for (; pat; pat = next)
	{
	  next = pat->next;
	  pat->next = prev;
	  prev = pat;
	}
      pat = prev;

      /* Go through the list, and call abstract_virtuals_error for each
	element: it will issue a diagnostic if the type is abstract.  */
      while (pat)
	{
	  gcc_assert (type == pat->type);

	  /* Tweak input_location so that the diagnostic appears at the correct
	    location. Notice that this is only needed if the decl is an
	    IDENTIFIER_NODE.  */
	  input_location = pat->locus;
	  abstract_virtuals_error_sfinae (pat->decl, pat->type, pat->use,
					  tf_warning_or_error);
	  pat = pat->next;
	}
    }

  abstract_pending_vars->clear_slot (slot);

  input_location = cur_loc;
}


/* If TYPE has abstract virtual functions, issue an error about trying
   to create an object of that type.  DECL is the object declared, or
   NULL_TREE if the declaration is unavailable, in which case USE specifies
   the kind of invalid use.  Returns 1 if an error occurred; zero if
   all was well.  */

static int
abstract_virtuals_error_sfinae (tree decl, tree type, abstract_class_use use,
				tsubst_flags_t complain)
{
  vec<tree, va_gc> *pure;

  /* This function applies only to classes. Any other entity can never
     be abstract.  */
  if (!CLASS_TYPE_P (type))
    return 0;
  type = TYPE_MAIN_VARIANT (type);

#if 0
  /* Instantiation here seems to be required by the standard,
     but breaks e.g. boost::bind.  FIXME!  */
  /* In SFINAE, non-N3276 context, force instantiation.  */
  if (!(complain & (tf_error|tf_decltype)))
    complete_type (type);
#endif

  /* If the type is incomplete, we register it within a hash table,
     so that we can check again once it is completed. This makes sense
     only for objects for which we have a declaration or at least a
     name.  */
  if (!COMPLETE_TYPE_P (type) && (complain & tf_error))
    {
      struct pending_abstract_type *pat;

      gcc_assert (!decl || DECL_P (decl) || identifier_p (decl));

      if (!abstract_pending_vars)
	abstract_pending_vars
	  = hash_table<abstract_type_hasher>::create_ggc (31);

      pending_abstract_type **slot
       	= abstract_pending_vars->find_slot_with_hash (type, TYPE_UID (type),
						      INSERT);

      pat = ggc_alloc<pending_abstract_type> ();
      pat->type = type;
      pat->decl = decl;
      pat->use = use;
      pat->locus = ((decl && DECL_P (decl))
		    ? DECL_SOURCE_LOCATION (decl)
		    : input_location);

      pat->next = *slot;
      *slot = pat;

      return 0;
    }

  if (!TYPE_SIZE (type))
    /* TYPE is being defined, and during that time
       CLASSTYPE_PURE_VIRTUALS holds the inline friends.  */
    return 0;

  pure = CLASSTYPE_PURE_VIRTUALS (type);
  if (!pure)
    return 0;

  if (!(complain & tf_error))
    return 1;

  if (decl)
    {
      if (VAR_P (decl))
	error ("cannot declare variable %q+D to be of abstract "
	       "type %qT", decl, type);
      else if (TREE_CODE (decl) == PARM_DECL)
	{
	  if (DECL_NAME (decl))
	    error ("cannot declare parameter %q+D to be of abstract type %qT",
		   decl, type);
	  else
	    error ("cannot declare parameter to be of abstract type %qT",
		   type);
	}
      else if (TREE_CODE (decl) == FIELD_DECL)
	error ("cannot declare field %q+D to be of abstract type %qT",
	       decl, type);
      else if (TREE_CODE (decl) == FUNCTION_DECL
	       && TREE_CODE (TREE_TYPE (decl)) == METHOD_TYPE)
	error ("invalid abstract return type for member function %q+#D", decl);
      else if (TREE_CODE (decl) == FUNCTION_DECL)
	error ("invalid abstract return type for function %q+#D", decl);
      else if (identifier_p (decl))
	/* Here we do not have location information.  */
	error ("invalid abstract type %qT for %qE", type, decl);
      else
	error ("invalid abstract type for %q+D", decl);
    }
  else switch (use)
    {
    case ACU_ARRAY:
      error ("creating array of %qT, which is an abstract class type", type);
      break;
    case ACU_CAST:
      error ("invalid cast to abstract class type %qT", type);
      break;
    case ACU_NEW:
      error ("invalid new-expression of abstract class type %qT", type);
      break;
    case ACU_RETURN:
      error ("invalid abstract return type %qT", type);
      break;
    case ACU_PARM:
      error ("invalid abstract parameter type %qT", type);
      break;
    case ACU_THROW:
      error ("expression of abstract class type %qT cannot "
	     "be used in throw-expression", type);
      break;
    case ACU_CATCH:
      error ("cannot declare catch parameter to be of abstract "
	     "class type %qT", type);
      break;
    default:
      error ("cannot allocate an object of abstract type %qT", type);
    }

  /* Only go through this once.  */
  if (pure->length ())
    {
      unsigned ix;
      tree fn;

      inform (DECL_SOURCE_LOCATION (TYPE_MAIN_DECL (type)),
	      "  because the following virtual functions are pure within %qT:",
	      type);

      FOR_EACH_VEC_ELT (*pure, ix, fn)
	if (! DECL_CLONED_FUNCTION_P (fn)
	    || DECL_COMPLETE_DESTRUCTOR_P (fn))
	  inform (DECL_SOURCE_LOCATION (fn), "\t%#D", fn);

      /* Now truncate the vector.  This leaves it non-null, so we know
	 there are pure virtuals, but empty so we don't list them out
	 again.  */
      pure->truncate (0);
    }

  return 1;
}

int
abstract_virtuals_error_sfinae (tree decl, tree type, tsubst_flags_t complain)
{
  return abstract_virtuals_error_sfinae (decl, type, ACU_UNKNOWN, complain);
}

int
abstract_virtuals_error_sfinae (abstract_class_use use, tree type,
				tsubst_flags_t complain)
{
  return abstract_virtuals_error_sfinae (NULL_TREE, type, use, complain);
}


/* Wrapper for the above function in the common case of wanting errors.  */

int
abstract_virtuals_error (tree decl, tree type)
{
  return abstract_virtuals_error_sfinae (decl, type, tf_warning_or_error);
}

int
abstract_virtuals_error (abstract_class_use use, tree type)
{
  return abstract_virtuals_error_sfinae (use, type, tf_warning_or_error);
}

/* Print an inform about the declaration of the incomplete type TYPE.  */

void
cxx_incomplete_type_inform (const_tree type)
{
  if (!TYPE_MAIN_DECL (type))
    return;

  location_t loc = DECL_SOURCE_LOCATION (TYPE_MAIN_DECL (type));
  tree ptype = strip_top_quals (CONST_CAST_TREE (type));

  if (current_class_type
      && TYPE_BEING_DEFINED (current_class_type)
      && same_type_p (ptype, current_class_type))
    inform (loc, "definition of %q#T is not complete until "
	    "the closing brace", ptype);
  else if (!TYPE_TEMPLATE_INFO (ptype))
    inform (loc, "forward declaration of %q#T", ptype);
  else
    inform (loc, "declaration of %q#T", ptype);
}

/* Print an error message for invalid use of an incomplete type.
   VALUE is the expression that was used (or 0 if that isn't known)
   and TYPE is the type that was invalid.  DIAG_KIND indicates the
   type of diagnostic (see diagnostic.def).  */

void
cxx_incomplete_type_diagnostic (location_t loc, const_tree value,
				const_tree type, diagnostic_t diag_kind)
{
  bool is_decl = false, complained = false;

  gcc_assert (diag_kind == DK_WARNING 
	      || diag_kind == DK_PEDWARN 
	      || diag_kind == DK_ERROR);

  /* Avoid duplicate error message.  */
  if (TREE_CODE (type) == ERROR_MARK)
    return;

  if (value != 0 && (VAR_P (value)
		     || TREE_CODE (value) == PARM_DECL
		     || TREE_CODE (value) == FIELD_DECL))
    {
      complained = emit_diagnostic (diag_kind, DECL_SOURCE_LOCATION (value), 0,
				    "%qD has incomplete type", value);
      is_decl = true;
    } 
 retry:
  /* We must print an error message.  Be clever about what it says.  */

  switch (TREE_CODE (type))
    {
    case RECORD_TYPE:
    case UNION_TYPE:
    case ENUMERAL_TYPE:
      if (!is_decl)
	complained = emit_diagnostic (diag_kind, loc, 0,
				      "invalid use of incomplete type %q#T",
				      type);
      if (complained)
	cxx_incomplete_type_inform (type);
      break;

    case VOID_TYPE:
      emit_diagnostic (diag_kind, loc, 0,
		       "invalid use of %qT", type);
      break;

    case ARRAY_TYPE:
      if (TYPE_DOMAIN (type))
	{
	  type = TREE_TYPE (type);
	  goto retry;
	}
      emit_diagnostic (diag_kind, loc, 0,
		       "invalid use of array with unspecified bounds");
      break;

    case OFFSET_TYPE:
    bad_member:
      {
	tree member = TREE_OPERAND (value, 1);
	if (is_overloaded_fn (member))
	  member = get_first_fn (member);
	if (DECL_FUNCTION_MEMBER_P (member)
	    && ! flag_ms_extensions)
	  emit_diagnostic (diag_kind, loc, 0,
			   "invalid use of member function %qD "
			   "(did you forget the %<()%> ?)", member);
	else
	  emit_diagnostic (diag_kind, loc, 0,
			   "invalid use of member %qD "
			   "(did you forget the %<&%> ?)", member);
      }
      break;

    case TEMPLATE_TYPE_PARM:
      if (is_auto (type))
	emit_diagnostic (diag_kind, loc, 0,
			 "invalid use of %<auto%>");
      else
	emit_diagnostic (diag_kind, loc, 0,
			 "invalid use of template type parameter %qT", type);
      break;

    case BOUND_TEMPLATE_TEMPLATE_PARM:
      emit_diagnostic (diag_kind, loc, 0,
		       "invalid use of template template parameter %qT",
		       TYPE_NAME (type));
      break;

    case TYPENAME_TYPE:
    case DECLTYPE_TYPE:
      emit_diagnostic (diag_kind, loc, 0,
		       "invalid use of dependent type %qT", type);
      break;

    case LANG_TYPE:
      if (type == init_list_type_node)
	{
	  emit_diagnostic (diag_kind, loc, 0,
			   "invalid use of brace-enclosed initializer list");
	  break;
	}
      gcc_assert (type == unknown_type_node);
      if (value && TREE_CODE (value) == COMPONENT_REF)
	goto bad_member;
      else if (value && TREE_CODE (value) == ADDR_EXPR)
	emit_diagnostic (diag_kind, loc, 0,
			 "address of overloaded function with no contextual "
			 "type information");
      else if (value && TREE_CODE (value) == OVERLOAD)
	emit_diagnostic (diag_kind, loc, 0,
			 "overloaded function with no contextual type information");
      else
	emit_diagnostic (diag_kind, loc, 0,
			 "insufficient contextual information to determine type");
      break;

    default:
      gcc_unreachable ();
    }
}

/* Print an error message for invalid use of an incomplete type.
   VALUE is the expression that was used (or 0 if that isn't known)
   and TYPE is the type that was invalid.  */

void
cxx_incomplete_type_error (location_t loc, const_tree value, const_tree type)
{
  cxx_incomplete_type_diagnostic (loc, value, type, DK_ERROR);
}


/* The recursive part of split_nonconstant_init.  DEST is an lvalue
   expression to which INIT should be assigned.  INIT is a CONSTRUCTOR.
   Return true if the whole of the value was initialized by the
   generated statements.  */

static bool
split_nonconstant_init_1 (tree dest, tree init)
{
  unsigned HOST_WIDE_INT idx;
  tree field_index, value;
  tree type = TREE_TYPE (dest);
  tree inner_type = NULL;
  bool array_type_p = false;
  bool complete_p = true;
  HOST_WIDE_INT num_split_elts = 0;

  switch (TREE_CODE (type))
    {
    case ARRAY_TYPE:
      inner_type = TREE_TYPE (type);
      array_type_p = true;
      if ((TREE_SIDE_EFFECTS (init)
	   && TYPE_HAS_NONTRIVIAL_DESTRUCTOR (type))
	  || array_of_runtime_bound_p (type))
	{
	  /* For an array, we only need/want a single cleanup region rather
	     than one per element.  */
	  tree code = build_vec_init (dest, NULL_TREE, init, false, 1,
				      tf_warning_or_error);
	  add_stmt (code);
	  return true;
	}
      /* FALLTHRU */

    case RECORD_TYPE:
    case UNION_TYPE:
    case QUAL_UNION_TYPE:
      FOR_EACH_CONSTRUCTOR_ELT (CONSTRUCTOR_ELTS (init), idx,
				field_index, value)
	{
	  /* The current implementation of this algorithm assumes that
	     the field was set for all the elements. This is usually done
	     by process_init_constructor.  */
	  gcc_assert (field_index);

	  if (!array_type_p)
	    inner_type = TREE_TYPE (field_index);

	  if (TREE_CODE (value) == CONSTRUCTOR)
	    {
	      tree sub;

	      if (array_type_p)
		sub = build4 (ARRAY_REF, inner_type, dest, field_index,
			      NULL_TREE, NULL_TREE);
	      else
		sub = build3 (COMPONENT_REF, inner_type, dest, field_index,
			      NULL_TREE);

	      if (!split_nonconstant_init_1 (sub, value))
		complete_p = false;
	      else
		CONSTRUCTOR_ELTS (init)->ordered_remove (idx--);
	      num_split_elts++;
	    }
	  else if (!initializer_constant_valid_p (value, inner_type))
	    {
	      tree code;
	      tree sub;

	      /* FIXME: Ordered removal is O(1) so the whole function is
		 worst-case quadratic. This could be fixed using an aside
		 bitmap to record which elements must be removed and remove
		 them all at the same time. Or by merging
		 split_non_constant_init into process_init_constructor_array,
		 that is separating constants from non-constants while building
		 the vector.  */
	      CONSTRUCTOR_ELTS (init)->ordered_remove (idx);
	      --idx;

	      if (TREE_CODE (field_index) == RANGE_EXPR)
		{
		  /* Use build_vec_init to initialize a range.  */
		  tree low = TREE_OPERAND (field_index, 0);
		  tree hi = TREE_OPERAND (field_index, 1);
		  sub = build4 (ARRAY_REF, inner_type, dest, low,
				NULL_TREE, NULL_TREE);
		  sub = cp_build_addr_expr (sub, tf_warning_or_error);
		  tree max = size_binop (MINUS_EXPR, hi, low);
		  code = build_vec_init (sub, max, value, false, 0,
					 tf_warning_or_error);
		  add_stmt (code);
		  if (tree_fits_shwi_p (max))
		    num_split_elts += tree_to_shwi (max);
		}
	      else
		{
		  if (array_type_p)
		    sub = build4 (ARRAY_REF, inner_type, dest, field_index,
				  NULL_TREE, NULL_TREE);
		  else
		    sub = build3 (COMPONENT_REF, inner_type, dest, field_index,
				  NULL_TREE);

		  code = build2 (INIT_EXPR, inner_type, sub, value);
		  code = build_stmt (input_location, EXPR_STMT, code);
		  code = maybe_cleanup_point_expr_void (code);
		  add_stmt (code);
		  if (tree cleanup
		      = cxx_maybe_build_cleanup (sub, tf_warning_or_error))
		    finish_eh_cleanup (cleanup);
		}

	      num_split_elts++;
	    }
	}
      break;

    case VECTOR_TYPE:
      if (!initializer_constant_valid_p (init, type))
	{
	  tree code;
	  tree cons = copy_node (init);
	  CONSTRUCTOR_ELTS (init) = NULL;
	  code = build2 (MODIFY_EXPR, type, dest, cons);
	  code = build_stmt (input_location, EXPR_STMT, code);
	  add_stmt (code);
	  num_split_elts += CONSTRUCTOR_NELTS (init);
	}
      break;

    default:
      gcc_unreachable ();
    }

  /* The rest of the initializer is now a constant. */
  TREE_CONSTANT (init) = 1;
  return complete_p && complete_ctor_at_level_p (TREE_TYPE (init),
						 num_split_elts, inner_type);
}

/* A subroutine of store_init_value.  Splits non-constant static
   initializer INIT into a constant part and generates code to
   perform the non-constant part of the initialization to DEST.
   Returns the code for the runtime init.  */

tree
split_nonconstant_init (tree dest, tree init)
{
  tree code;

  if (TREE_CODE (init) == TARGET_EXPR)
    init = TARGET_EXPR_INITIAL (init);
  if (TREE_CODE (init) == CONSTRUCTOR)
    {
      init = cp_fully_fold (init);
      code = push_stmt_list ();
      if (split_nonconstant_init_1 (dest, init))
	init = NULL_TREE;
      code = pop_stmt_list (code);
      DECL_INITIAL (dest) = init;
      TREE_READONLY (dest) = 0;
    }
  else
    code = build2 (INIT_EXPR, TREE_TYPE (dest), dest, init);

  return code;
}

/* Perform appropriate conversions on the initial value of a variable,
   store it in the declaration DECL,
   and print any error messages that are appropriate.
   If the init is invalid, store an ERROR_MARK.

   C++: Note that INIT might be a TREE_LIST, which would mean that it is
   a base class initializer for some aggregate type, hopefully compatible
   with DECL.  If INIT is a single element, and DECL is an aggregate
   type, we silently convert INIT into a TREE_LIST, allowing a constructor
   to be called.

   If INIT is a TREE_LIST and there is no constructor, turn INIT
   into a CONSTRUCTOR and use standard initialization techniques.
   Perhaps a warning should be generated?

   Returns code to be executed if initialization could not be performed
   for static variable.  In that case, caller must emit the code.  */

tree
store_init_value (tree decl, tree init, vec<tree, va_gc>** cleanups, int flags)
{
  tree value, type;

  /* If variable's type was invalidly declared, just ignore it.  */

  type = TREE_TYPE (decl);
  if (TREE_CODE (type) == ERROR_MARK)
    return NULL_TREE;

  if (MAYBE_CLASS_TYPE_P (type))
    {
      if (TREE_CODE (init) == TREE_LIST)
	{
	  error ("constructor syntax used, but no constructor declared "
		 "for type %qT", type);
	  init = build_constructor_from_list (init_list_type_node, nreverse (init));
	}
    }

  /* End of special C++ code.  */

  if (flags & LOOKUP_ALREADY_DIGESTED)
    value = init;
  else
    /* Digest the specified initializer into an expression.  */
    value = digest_init_flags (type, init, flags);

  value = extend_ref_init_temps (decl, value, cleanups);

  /* In C++11 constant expression is a semantic, not syntactic, property.
     In C++98, make sure that what we thought was a constant expression at
     template definition time is still constant and otherwise perform this
     as optimization, e.g. to fold SIZEOF_EXPRs in the initializer.  */
  if (decl_maybe_constant_var_p (decl) || TREE_STATIC (decl))
    {
      bool const_init;
      value = instantiate_non_dependent_expr (value);
      if (DECL_DECLARED_CONSTEXPR_P (decl)
	  || (DECL_IN_AGGR_P (decl) && !DECL_VAR_DECLARED_INLINE_P (decl)))
	{
	  /* Diagnose a non-constant initializer for constexpr.  */
	  if (processing_template_decl
	      && !require_potential_constant_expression (value))
	    value = error_mark_node;
	  else
	    value = cxx_constant_value (value, decl);
	}
      value = maybe_constant_init (value, decl);
      if (TREE_CODE (value) == CONSTRUCTOR && cp_has_mutable_p (type))
	/* Poison this CONSTRUCTOR so it can't be copied to another
	   constexpr variable.  */
	CONSTRUCTOR_MUTABLE_POISON (value) = true;
      const_init = (reduced_constant_expression_p (value)
		    || error_operand_p (value));
      DECL_INITIALIZED_BY_CONSTANT_EXPRESSION_P (decl) = const_init;
      TREE_CONSTANT (decl) = const_init && decl_maybe_constant_var_p (decl);
    }
  value = cp_fully_fold (value);

  if (cxx_dialect >= cxx14 && CLASS_TYPE_P (strip_array_types (type)))
    /* Handle aggregate NSDMI in non-constant initializers, too.  */
    value = replace_placeholders (value, decl);

  /* DECL may change value; purge caches.  */
  clear_cv_and_fold_caches ();

  /* If the initializer is not a constant, fill in DECL_INITIAL with
     the bits that are constant, and then return an expression that
     will perform the dynamic initialization.  */
  if (value != error_mark_node
      && (TREE_SIDE_EFFECTS (value)
	  || array_of_runtime_bound_p (type)
	  || ! reduced_constant_expression_p (value)))
    return split_nonconstant_init (decl, value);
  /* If the value is a constant, just put it in DECL_INITIAL.  If DECL
     is an automatic variable, the middle end will turn this into a
     dynamic initialization later.  */
  DECL_INITIAL (decl) = value;
  return NULL_TREE;
}


/* Give diagnostic about narrowing conversions within { }.  */

bool
check_narrowing (tree type, tree init, tsubst_flags_t complain)
{
  tree ftype = unlowered_expr_type (init);
  bool ok = true;
  REAL_VALUE_TYPE d;

  if (((!warn_narrowing || !(complain & tf_warning))
       && cxx_dialect == cxx98)
      || !ARITHMETIC_TYPE_P (type))
    return ok;

  if (BRACE_ENCLOSED_INITIALIZER_P (init)
      && TREE_CODE (type) == COMPLEX_TYPE)
    {
      tree elttype = TREE_TYPE (type);
      if (CONSTRUCTOR_NELTS (init) > 0)
        ok &= check_narrowing (elttype, CONSTRUCTOR_ELT (init, 0)->value,
			       complain);
      if (CONSTRUCTOR_NELTS (init) > 1)
	ok &= check_narrowing (elttype, CONSTRUCTOR_ELT (init, 1)->value,
			       complain);
      return ok;
    }

  init = fold_non_dependent_expr (init);

  if (TREE_CODE (type) == INTEGER_TYPE
      && TREE_CODE (ftype) == REAL_TYPE)
    ok = false;
  else if (INTEGRAL_OR_ENUMERATION_TYPE_P (ftype)
	   && CP_INTEGRAL_TYPE_P (type))
    {
      if (TREE_CODE (ftype) == ENUMERAL_TYPE)
	/* Check for narrowing based on the values of the enumeration. */
	ftype = ENUM_UNDERLYING_TYPE (ftype);
      if ((tree_int_cst_lt (TYPE_MAX_VALUE (type),
			    TYPE_MAX_VALUE (ftype))
	   || tree_int_cst_lt (TYPE_MIN_VALUE (ftype),
			       TYPE_MIN_VALUE (type)))
	  && (TREE_CODE (init) != INTEGER_CST
	      || !int_fits_type_p (init, type)))
	ok = false;
    }
  else if (TREE_CODE (ftype) == REAL_TYPE
	   && TREE_CODE (type) == REAL_TYPE)
    {
      if (TYPE_PRECISION (type) < TYPE_PRECISION (ftype))
	{
	  if (TREE_CODE (init) == REAL_CST)
	    {
	      /* Issue 703: Loss of precision is OK as long as the value is
		 within the representable range of the new type.  */
	      REAL_VALUE_TYPE r;
	      d = TREE_REAL_CST (init);
	      real_convert (&r, TYPE_MODE (type), &d);
	      if (real_isinf (&r))
		ok = false;
	    }
	  else
	    ok = false;
	}
    }
  else if (INTEGRAL_OR_ENUMERATION_TYPE_P (ftype)
	   && TREE_CODE (type) == REAL_TYPE)
    {
      ok = false;
      if (TREE_CODE (init) == INTEGER_CST)
	{
	  d = real_value_from_int_cst (0, init);
	  if (exact_real_truncate (TYPE_MODE (type), &d))
	    ok = true;
	}
    }

  bool almost_ok = ok;
  if (!ok && !CONSTANT_CLASS_P (init) && (complain & tf_warning_or_error))
    {
      tree folded = cp_fully_fold (init);
      if (TREE_CONSTANT (folded) && check_narrowing (type, folded, tf_none))
	almost_ok = true;
    }

  if (!ok)
    {
      location_t loc = EXPR_LOC_OR_LOC (init, input_location);
      if (cxx_dialect == cxx98)
	{
	  if (complain & tf_warning)
	    warning_at (loc, OPT_Wnarrowing, "narrowing conversion of %qE "
			"from %qT to %qT inside { } is ill-formed in C++11",
			init, ftype, type);
	  ok = true;
	}
      else if (!CONSTANT_CLASS_P (init))
	{
	  if (complain & tf_warning_or_error)
	    {
	      if ((!almost_ok || pedantic)
		  && pedwarn (loc, OPT_Wnarrowing,
			      "narrowing conversion of %qE "
			      "from %qT to %qT inside { }",
			      init, ftype, type)
		  && almost_ok)
		inform (loc, " the expression has a constant value but is not "
			"a C++ constant-expression");
	      ok = true;
	    }
	}
      else if (complain & tf_error)
	{
	  int savederrorcount = errorcount;
	  global_dc->pedantic_errors = 1;
	  pedwarn (loc, OPT_Wnarrowing,
		   "narrowing conversion of %qE from %qT to %qT "
		   "inside { }", init, ftype, type);
	  if (errorcount == savederrorcount)
	    ok = true;
	  global_dc->pedantic_errors = flag_pedantic_errors;
	}
    }

  return ok;
}

/* Process the initializer INIT for a variable of type TYPE, emitting
   diagnostics for invalid initializers and converting the initializer as
   appropriate.

   For aggregate types, it assumes that reshape_init has already run, thus the
   initializer will have the right shape (brace elision has been undone).

   NESTED is true iff we are being called for an element of a CONSTRUCTOR.  */

static tree
digest_init_r (tree type, tree init, bool nested, int flags,
	       tsubst_flags_t complain)
{
  enum tree_code code = TREE_CODE (type);

  if (error_operand_p (init))
    return error_mark_node;

  gcc_assert (init);

  /* We must strip the outermost array type when completing the type,
     because the its bounds might be incomplete at the moment.  */
  if (!complete_type_or_maybe_complain (TREE_CODE (type) == ARRAY_TYPE
					? TREE_TYPE (type) : type, NULL_TREE,
					complain))
    return error_mark_node;

  /* Strip NON_LVALUE_EXPRs since we aren't using as an lvalue
     (g++.old-deja/g++.law/casts2.C).  */
  if (TREE_CODE (init) == NON_LVALUE_EXPR)
    init = TREE_OPERAND (init, 0);

  location_t loc = EXPR_LOC_OR_LOC (init, input_location);

  /* Initialization of an array of chars from a string constant. The initializer
     can be optionally enclosed in braces, but reshape_init has already removed
     them if they were present.  */
  if (code == ARRAY_TYPE)
    {
      if (nested && !TYPE_DOMAIN (type))
	{
	  /* C++ flexible array members have a null domain.  */
	  pedwarn (loc, OPT_Wpedantic,
		   "initialization of a flexible array member");
	}

      tree typ1 = TYPE_MAIN_VARIANT (TREE_TYPE (type));
      if (char_type_p (typ1)
	  /*&& init */
	  && TREE_CODE (init) == STRING_CST)
	{
	  tree char_type = TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (init)));

	  if (TYPE_PRECISION (typ1) == BITS_PER_UNIT)
	    {
	      if (char_type != char_type_node)
		{
		  if (complain & tf_error)
		    error_at (loc, "char-array initialized from wide string");
		  return error_mark_node;
		}
	    }
	  else
	    {
	      if (char_type == char_type_node)
		{
		  if (complain & tf_error)
		    error_at (loc,
			      "int-array initialized from non-wide string");
		  return error_mark_node;
		}
	      else if (char_type != typ1)
		{
		  if (complain & tf_error)
		    error_at (loc, "int-array initialized from incompatible "
			      "wide string");
		  return error_mark_node;
		}
	    }

	  if (type != TREE_TYPE (init))
	    {
	      init = copy_node (init);
	      TREE_TYPE (init) = type;
	    }
	  if (TYPE_DOMAIN (type) && TREE_CONSTANT (TYPE_SIZE (type)))
	    {
	      /* Not a flexible array member.  */
	      int size = TREE_INT_CST_LOW (TYPE_SIZE (type));
	      size = (size + BITS_PER_UNIT - 1) / BITS_PER_UNIT;
	      /* In C it is ok to subtract 1 from the length of the string
		 because it's ok to ignore the terminating null char that is
		 counted in the length of the constant, but in C++ this would
		 be invalid.  */
	      if (size < TREE_STRING_LENGTH (init))
		permerror (loc, "initializer-string for array "
			   "of chars is too long");
	    }
	  return init;
	}
    }

  /* Handle scalar types (including conversions) and references.  */
  if ((TREE_CODE (type) != COMPLEX_TYPE
       || BRACE_ENCLOSED_INITIALIZER_P (init))
      && (SCALAR_TYPE_P (type) || code == REFERENCE_TYPE))
    {
      if (nested)
	flags |= LOOKUP_NO_NARROWING;
      init = convert_for_initialization (0, type, init, flags,
					 ICR_INIT, NULL_TREE, 0,
					 complain);

      return init;
    }

  /* Come here only for aggregates: records, arrays, unions, complex numbers
     and vectors.  */
  gcc_assert (TREE_CODE (type) == ARRAY_TYPE
	      || VECTOR_TYPE_P (type)
	      || TREE_CODE (type) == RECORD_TYPE
	      || TREE_CODE (type) == UNION_TYPE
	      || TREE_CODE (type) == COMPLEX_TYPE);

  /* "If T is a class type and the initializer list has a single
     element of type cv U, where U is T or a class derived from T,
     the object is initialized from that element."  */
  if (flag_checking
      && cxx_dialect >= cxx11
      && BRACE_ENCLOSED_INITIALIZER_P (init)
      && CONSTRUCTOR_NELTS (init) == 1
      && ((CLASS_TYPE_P (type) && !CLASSTYPE_NON_AGGREGATE (type))
	  || VECTOR_TYPE_P (type)))
    {
      tree elt = CONSTRUCTOR_ELT (init, 0)->value;
      if (reference_related_p (type, TREE_TYPE (elt)))
	/* We should have fixed this in reshape_init.  */
	gcc_unreachable ();
    }

  if (BRACE_ENCLOSED_INITIALIZER_P (init)
      && !TYPE_NON_AGGREGATE_CLASS (type))
    return process_init_constructor (type, init, complain);
  else
    {
      if (COMPOUND_LITERAL_P (init) && TREE_CODE (type) == ARRAY_TYPE)
	{
	  if (complain & tf_error)
	    error_at (loc, "cannot initialize aggregate of type %qT with "
		      "a compound literal", type);

	  return error_mark_node;
	}

      if (TREE_CODE (type) == ARRAY_TYPE
	  && !BRACE_ENCLOSED_INITIALIZER_P (init))
	{
	  /* Allow the result of build_array_copy and of
	     build_value_init_noctor.  */
	  if ((TREE_CODE (init) == VEC_INIT_EXPR
	       || TREE_CODE (init) == CONSTRUCTOR)
	      && (same_type_ignoring_top_level_qualifiers_p
		  (type, TREE_TYPE (init))))
	    return init;

	  if (complain & tf_error)
	    error_at (loc, "array must be initialized with a brace-enclosed"
		      " initializer");
	  return error_mark_node;
	}

      return convert_for_initialization (NULL_TREE, type, init,
					 flags,
					 ICR_INIT, NULL_TREE, 0,
                                         complain);
    }
}

tree
digest_init (tree type, tree init, tsubst_flags_t complain)
{
  return digest_init_r (type, init, false, LOOKUP_IMPLICIT, complain);
}

tree
digest_init_flags (tree type, tree init, int flags)
{
  return digest_init_r (type, init, false, flags, tf_warning_or_error);
}

/* Process the initializer INIT for an NSDMI DECL (a FIELD_DECL).  */
tree
digest_nsdmi_init (tree decl, tree init)
{
  gcc_assert (TREE_CODE (decl) == FIELD_DECL);

  tree type = TREE_TYPE (decl);
  int flags = LOOKUP_IMPLICIT;
  if (DIRECT_LIST_INIT_P (init))
    flags = LOOKUP_NORMAL;
  if (BRACE_ENCLOSED_INITIALIZER_P (init)
      && CP_AGGREGATE_TYPE_P (type))
    init = reshape_init (type, init, tf_warning_or_error);
  init = digest_init_flags (type, init, flags);
  if (TREE_CODE (init) == TARGET_EXPR)
    /* This represents the whole initialization.  */
    TARGET_EXPR_DIRECT_INIT_P (init) = true;
  return init;
}

/* Set of flags used within process_init_constructor to describe the
   initializers.  */
#define PICFLAG_ERRONEOUS 1
#define PICFLAG_NOT_ALL_CONSTANT 2
#define PICFLAG_NOT_ALL_SIMPLE 4
#define PICFLAG_SIDE_EFFECTS 8

/* Given an initializer INIT, return the flag (PICFLAG_*) which better
   describe it.  */

static int
picflag_from_initializer (tree init)
{
  if (init == error_mark_node)
    return PICFLAG_ERRONEOUS;
  else if (!TREE_CONSTANT (init))
    {
      if (TREE_SIDE_EFFECTS (init))
	return PICFLAG_SIDE_EFFECTS;
      else
	return PICFLAG_NOT_ALL_CONSTANT;
    }
  else if (!initializer_constant_valid_p (init, TREE_TYPE (init)))
    return PICFLAG_NOT_ALL_SIMPLE;
  return 0;
}

/* Adjust INIT for going into a CONSTRUCTOR.  */

static tree
massage_init_elt (tree type, tree init, tsubst_flags_t complain)
{
  init = digest_init_r (type, init, true, LOOKUP_IMPLICIT, complain);
  /* Strip a simple TARGET_EXPR when we know this is an initializer.  */
  if (SIMPLE_TARGET_EXPR_P (init))
    init = TARGET_EXPR_INITIAL (init);
  /* When we defer constant folding within a statement, we may want to
     defer this folding as well.  */
  tree t = fold_non_dependent_expr (init);
  t = maybe_constant_init (t);
  if (TREE_CONSTANT (t))
    init = t;
  return init;
}

/* Subroutine of process_init_constructor, which will process an initializer
   INIT for an array or vector of type TYPE. Returns the flags (PICFLAG_*)
   which describe the initializers.  */

static int
process_init_constructor_array (tree type, tree init,
				tsubst_flags_t complain)
{
  unsigned HOST_WIDE_INT i, len = 0;
  int flags = 0;
  bool unbounded = false;
  constructor_elt *ce;
  vec<constructor_elt, va_gc> *v = CONSTRUCTOR_ELTS (init);

  gcc_assert (TREE_CODE (type) == ARRAY_TYPE
	      || VECTOR_TYPE_P (type));

  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      /* C++ flexible array members have a null domain.  */
      tree domain = TYPE_DOMAIN (type);
      if (domain && TREE_CONSTANT (TYPE_MAX_VALUE (domain)))
	len = wi::ext (wi::to_offset (TYPE_MAX_VALUE (domain))
                       - wi::to_offset (TYPE_MIN_VALUE (domain)) + 1,
		       TYPE_PRECISION (TREE_TYPE (domain)),
		       TYPE_SIGN (TREE_TYPE (domain))).to_uhwi ();
      else
	unbounded = true;  /* Take as many as there are.  */
    }
  else
    /* Vectors are like simple fixed-size arrays.  */
    len = TYPE_VECTOR_SUBPARTS (type);

  /* There must not be more initializers than needed.  */
  if (!unbounded && vec_safe_length (v) > len)
    {
      if (complain & tf_error)
	error ("too many initializers for %qT", type);
      else
	return PICFLAG_ERRONEOUS;
    }

  FOR_EACH_VEC_SAFE_ELT (v, i, ce)
    {
      if (ce->index)
	{
	  gcc_assert (TREE_CODE (ce->index) == INTEGER_CST);
	  if (compare_tree_int (ce->index, i) != 0)
	    {
	      ce->value = error_mark_node;
	      sorry ("non-trivial designated initializers not supported");
	    }
	}
      else
	ce->index = size_int (i);
      gcc_assert (ce->value);
      ce->value = massage_init_elt (TREE_TYPE (type), ce->value, complain);

      if (ce->value != error_mark_node)
	gcc_assert (same_type_ignoring_top_level_qualifiers_p
		      (TREE_TYPE (type), TREE_TYPE (ce->value)));

      flags |= picflag_from_initializer (ce->value);
    }

  /* No more initializers. If the array is unbounded, we are done. Otherwise,
     we must add initializers ourselves.  */
  if (!unbounded)
    for (; i < len; ++i)
      {
	tree next;

	if (type_build_ctor_call (TREE_TYPE (type)))
	  {
	    /* If this type needs constructors run for default-initialization,
	       we can't rely on the back end to do it for us, so make the
	       initialization explicit by list-initializing from T{}.  */
	    next = build_constructor (init_list_type_node, NULL);
	    next = massage_init_elt (TREE_TYPE (type), next, complain);
	    if (initializer_zerop (next))
	      /* The default zero-initialization is fine for us; don't
		 add anything to the CONSTRUCTOR.  */
	      next = NULL_TREE;
	  }
	else if (!zero_init_p (TREE_TYPE (type)))
	  next = build_zero_init (TREE_TYPE (type),
				  /*nelts=*/NULL_TREE,
				  /*static_storage_p=*/false);
	else
	  /* The default zero-initialization is fine for us; don't
	     add anything to the CONSTRUCTOR.  */
	  next = NULL_TREE;

	if (next)
	  {
	    flags |= picflag_from_initializer (next);
	    CONSTRUCTOR_APPEND_ELT (v, size_int (i), next);
	  }
      }

  CONSTRUCTOR_ELTS (init) = v;
  return flags;
}

/* Subroutine of process_init_constructor, which will process an initializer
   INIT for a class of type TYPE. Returns the flags (PICFLAG_*) which describe
   the initializers.  */

static int
process_init_constructor_record (tree type, tree init,
				 tsubst_flags_t complain)
{
  vec<constructor_elt, va_gc> *v = NULL;
  tree field;
  int skipped = 0;

  gcc_assert (TREE_CODE (type) == RECORD_TYPE);
  gcc_assert (!CLASSTYPE_VBASECLASSES (type));
  gcc_assert (!TYPE_BINFO (type)
	      || cxx_dialect >= cxx1z
	      || !BINFO_N_BASE_BINFOS (TYPE_BINFO (type)));
  gcc_assert (!TYPE_POLYMORPHIC_P (type));

 restart:
  int flags = 0;
  unsigned HOST_WIDE_INT idx = 0;
  /* Generally, we will always have an index for each initializer (which is
     a FIELD_DECL, put by reshape_init), but compound literals don't go trough
     reshape_init. So we need to handle both cases.  */
  for (field = TYPE_FIELDS (type); field; field = DECL_CHAIN (field))
    {
      tree next;
      tree type;

      if (!DECL_NAME (field) && DECL_C_BIT_FIELD (field))
	continue;

      if (TREE_CODE (field) != FIELD_DECL
	  || (DECL_ARTIFICIAL (field)
	      && !(cxx_dialect >= cxx1z && DECL_FIELD_IS_BASE (field))))
	continue;

      /* If this is a bitfield, first convert to the declared type.  */
      type = TREE_TYPE (field);
      if (DECL_BIT_FIELD_TYPE (field))
	type = DECL_BIT_FIELD_TYPE (field);
      if (type == error_mark_node)
	return PICFLAG_ERRONEOUS;

      if (idx < CONSTRUCTOR_NELTS (init))
	{
	  constructor_elt *ce = &(*CONSTRUCTOR_ELTS (init))[idx];
	  if (ce->index)
	    {
	      /* We can have either a FIELD_DECL or an IDENTIFIER_NODE. The
		 latter case can happen in templates where lookup has to be
		 deferred.  */
	      gcc_assert (TREE_CODE (ce->index) == FIELD_DECL
			  || identifier_p (ce->index));
	      if (ce->index != field
		  && ce->index != DECL_NAME (field))
		{
		  ce->value = error_mark_node;
		  sorry ("non-trivial designated initializers not supported");
		}
	    }

	  gcc_assert (ce->value);
	  next = massage_init_elt (type, ce->value, complain);
	  ++idx;
	}
      else if (DECL_INITIAL (field))
	{
	  if (skipped > 0)
	    {
	      /* We're using an NSDMI past a field with implicit
	         zero-init.  Go back and make it explicit.  */
	      skipped = -1;
	      vec_safe_truncate (v, 0);
	      goto restart;
	    }
	  /* C++14 aggregate NSDMI.  */
	  next = get_nsdmi (field, /*ctor*/false);
	}
      else if (type_build_ctor_call (TREE_TYPE (field)))
	{
	  /* If this type needs constructors run for
	     default-initialization, we can't rely on the back end to do it
	     for us, so build up TARGET_EXPRs.  If the type in question is
	     a class, just build one up; if it's an array, recurse.  */
	  next = build_constructor (init_list_type_node, NULL);
	  next = massage_init_elt (TREE_TYPE (field), next, complain);

	  /* Warn when some struct elements are implicitly initialized.  */
	  if ((complain & tf_warning)
	      && !EMPTY_CONSTRUCTOR_P (init))
	    warning (OPT_Wmissing_field_initializers,
		     "missing initializer for member %qD", field);
	}
      else
	{
	  const_tree fldtype = TREE_TYPE (field);
	  if (TREE_CODE (fldtype) == REFERENCE_TYPE)
	    {
	      if (complain & tf_error)
		error ("member %qD is uninitialized reference", field);
	      else
		return PICFLAG_ERRONEOUS;
	    }
	  else if (CLASSTYPE_REF_FIELDS_NEED_INIT (fldtype))
	    {
	      if (complain & tf_error)
		error ("member %qD with uninitialized reference fields", field);
	      else
		return PICFLAG_ERRONEOUS;
	    }

	  /* Warn when some struct elements are implicitly initialized
	     to zero.  However, avoid issuing the warning for flexible
	     array members since they need not have any elements.  */
	  if ((TREE_CODE (fldtype) != ARRAY_TYPE || TYPE_DOMAIN (fldtype))
	      && (complain & tf_warning)
	      && !EMPTY_CONSTRUCTOR_P (init))
	    warning (OPT_Wmissing_field_initializers,
		     "missing initializer for member %qD", field);

	  if (!zero_init_p (fldtype)
	      || skipped < 0)
	    next = build_zero_init (TREE_TYPE (field), /*nelts=*/NULL_TREE,
				    /*static_storage_p=*/false);
	  else
	    {
	      /* The default zero-initialization is fine for us; don't
		 add anything to the CONSTRUCTOR.  */
	      skipped = 1;
	      continue;
	    }
	}

      /* If this is a bitfield, now convert to the lowered type.  */
      if (type != TREE_TYPE (field))
	next = cp_convert_and_check (TREE_TYPE (field), next, complain);
      flags |= picflag_from_initializer (next);
      CONSTRUCTOR_APPEND_ELT (v, field, next);
    }

  if (idx < CONSTRUCTOR_NELTS (init))
    {
      if (complain & tf_error)
	error ("too many initializers for %qT", type);
      else
	return PICFLAG_ERRONEOUS;
    }

  CONSTRUCTOR_ELTS (init) = v;
  return flags;
}

/* Subroutine of process_init_constructor, which will process a single
   initializer INIT for a union of type TYPE. Returns the flags (PICFLAG_*)
   which describe the initializer.  */

static int
process_init_constructor_union (tree type, tree init,
				tsubst_flags_t complain)
{
  constructor_elt *ce;
  int len;

  /* If the initializer was empty, use the union's NSDMI if it has one.
     Otherwise use default zero initialization.  */
  if (vec_safe_is_empty (CONSTRUCTOR_ELTS (init)))
    {
      for (tree field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	{
	  if (DECL_INITIAL (field))
	    {
	      CONSTRUCTOR_APPEND_ELT (CONSTRUCTOR_ELTS (init),
				      field,
				      get_nsdmi (field, /*in_ctor=*/false));
	      break;
	    }
	}

      if (vec_safe_is_empty (CONSTRUCTOR_ELTS (init)))
	return 0;
    }

  len = CONSTRUCTOR_ELTS (init)->length ();
  if (len > 1)
    {
      if (!(complain & tf_error))
	return PICFLAG_ERRONEOUS;
      error ("too many initializers for %qT", type);
      CONSTRUCTOR_ELTS (init)->block_remove (1, len-1);
    }

  ce = &(*CONSTRUCTOR_ELTS (init))[0];

  /* If this element specifies a field, initialize via that field.  */
  if (ce->index)
    {
      if (TREE_CODE (ce->index) == FIELD_DECL)
	;
      else if (identifier_p (ce->index))
	{
	  /* This can happen within a cast, see g++.dg/opt/cse2.C.  */
	  tree name = ce->index;
	  tree field;
	  for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	    if (DECL_NAME (field) == name)
	      break;
	  if (!field)
	    {
	      if (complain & tf_error)
		error ("no field %qD found in union being initialized",
		       field);
	      ce->value = error_mark_node;
	    }
	  ce->index = field;
	}
      else
	{
	  gcc_assert (TREE_CODE (ce->index) == INTEGER_CST
		      || TREE_CODE (ce->index) == RANGE_EXPR);
	  if (complain & tf_error)
	    error ("index value instead of field name in union initializer");
	  ce->value = error_mark_node;
	}
    }
  else
    {
      /* Find the first named field.  ANSI decided in September 1990
	 that only named fields count here.  */
      tree field = TYPE_FIELDS (type);
      while (field && (!DECL_NAME (field) || TREE_CODE (field) != FIELD_DECL))
	field = TREE_CHAIN (field);
      if (field == NULL_TREE)
	{
	  if (complain & tf_error)
	    error ("too many initializers for %qT", type);
	  ce->value = error_mark_node;
	}
      ce->index = field;
    }

  if (ce->value && ce->value != error_mark_node)
    ce->value = massage_init_elt (TREE_TYPE (ce->index), ce->value, complain);

  return picflag_from_initializer (ce->value);
}

/* Process INIT, a constructor for a variable of aggregate type TYPE. The
   constructor is a brace-enclosed initializer, and will be modified in-place.

   Each element is converted to the right type through digest_init, and
   missing initializers are added following the language rules (zero-padding,
   etc.).

   After the execution, the initializer will have TREE_CONSTANT if all elts are
   constant, and TREE_STATIC set if, in addition, all elts are simple enough
   constants that the assembler and linker can compute them.

   The function returns the initializer itself, or error_mark_node in case
   of error.  */

static tree
process_init_constructor (tree type, tree init, tsubst_flags_t complain)
{
  int flags;

  gcc_assert (BRACE_ENCLOSED_INITIALIZER_P (init));

  if (TREE_CODE (type) == ARRAY_TYPE || VECTOR_TYPE_P (type))
    flags = process_init_constructor_array (type, init, complain);
  else if (TREE_CODE (type) == RECORD_TYPE)
    flags = process_init_constructor_record (type, init, complain);
  else if (TREE_CODE (type) == UNION_TYPE)
    flags = process_init_constructor_union (type, init, complain);
  else
    gcc_unreachable ();

  if (flags & PICFLAG_ERRONEOUS)
    return error_mark_node;

  TREE_TYPE (init) = type;
  if (TREE_CODE (type) == ARRAY_TYPE && TYPE_DOMAIN (type) == NULL_TREE)
    cp_complete_array_type (&TREE_TYPE (init), init, /*do_default=*/0);
  if (flags & PICFLAG_SIDE_EFFECTS)
    {
      TREE_CONSTANT (init) = false;
      TREE_SIDE_EFFECTS (init) = true;
    }
  else if (flags & PICFLAG_NOT_ALL_CONSTANT)
    /* Make sure TREE_CONSTANT isn't set from build_constructor.  */
    TREE_CONSTANT (init) = false;
  else
    {
      TREE_CONSTANT (init) = 1;
      if (!(flags & PICFLAG_NOT_ALL_SIMPLE))
	TREE_STATIC (init) = 1;
    }
  return init;
}

/* Given a structure or union value DATUM, construct and return
   the structure or union component which results from narrowing
   that value to the base specified in BASETYPE.  For example, given the
   hierarchy

   class L { int ii; };
   class A : L { ... };
   class B : L { ... };
   class C : A, B { ... };

   and the declaration

   C x;

   then the expression

   x.A::ii refers to the ii member of the L part of
   the A part of the C object named by X.  In this case,
   DATUM would be x, and BASETYPE would be A.

   I used to think that this was nonconformant, that the standard specified
   that first we look up ii in A, then convert x to an L& and pull out the
   ii part.  But in fact, it does say that we convert x to an A&; A here
   is known as the "naming class".  (jason 2000-12-19)

   BINFO_P points to a variable initialized either to NULL_TREE or to the
   binfo for the specific base subobject we want to convert to.  */

tree
build_scoped_ref (tree datum, tree basetype, tree* binfo_p)
{
  tree binfo;

  if (datum == error_mark_node)
    return error_mark_node;
  if (*binfo_p)
    binfo = *binfo_p;
  else
    binfo = lookup_base (TREE_TYPE (datum), basetype, ba_check,
			 NULL, tf_warning_or_error);

  if (!binfo || binfo == error_mark_node)
    {
      *binfo_p = NULL_TREE;
      if (!binfo)
	error_not_base_type (basetype, TREE_TYPE (datum));
      return error_mark_node;
    }

  *binfo_p = binfo;
  return build_base_path (PLUS_EXPR, datum, binfo, 1,
			  tf_warning_or_error);
}

/* Build a reference to an object specified by the C++ `->' operator.
   Usually this just involves dereferencing the object, but if the
   `->' operator is overloaded, then such overloads must be
   performed until an object which does not have the `->' operator
   overloaded is found.  An error is reported when circular pointer
   delegation is detected.  */

tree
build_x_arrow (location_t loc, tree expr, tsubst_flags_t complain)
{
  tree orig_expr = expr;
  tree type = TREE_TYPE (expr);
  tree last_rval = NULL_TREE;
  vec<tree, va_gc> *types_memoized = NULL;

  if (type == error_mark_node)
    return error_mark_node;

  if (processing_template_decl)
    {
      if (type && TREE_CODE (type) == POINTER_TYPE
	  && !dependent_scope_p (TREE_TYPE (type)))
	/* Pointer to current instantiation, don't treat as dependent.  */;
      else if (type_dependent_expression_p (expr))
	return build_min_nt_loc (loc, ARROW_EXPR, expr);
      expr = build_non_dependent_expr (expr);
    }

  if (MAYBE_CLASS_TYPE_P (type))
    {
      struct tinst_level *actual_inst = current_instantiation ();
      tree fn = NULL;

      while ((expr = build_new_op (loc, COMPONENT_REF,
				   LOOKUP_NORMAL, expr, NULL_TREE, NULL_TREE,
				   &fn, complain)))
	{
	  if (expr == error_mark_node)
	    return error_mark_node;

	  /* This provides a better instantiation backtrace in case of
	     error.  */
	  if (fn && DECL_USE_TEMPLATE (fn))
	    push_tinst_level_loc (fn, 
				  (current_instantiation () != actual_inst)
				  ? DECL_SOURCE_LOCATION (fn)
				  : input_location);
	  fn = NULL;

	  if (vec_member (TREE_TYPE (expr), types_memoized))
	    {
	      if (complain & tf_error)
		error ("circular pointer delegation detected");
	      return error_mark_node;
	    }

	  vec_safe_push (types_memoized, TREE_TYPE (expr));
	  last_rval = expr;
	}

      while (current_instantiation () != actual_inst)
	pop_tinst_level ();

      if (last_rval == NULL_TREE)
	{
	  if (complain & tf_error)
	    error ("base operand of %<->%> has non-pointer type %qT", type);
	  return error_mark_node;
	}

      if (TREE_CODE (TREE_TYPE (last_rval)) == REFERENCE_TYPE)
	last_rval = convert_from_reference (last_rval);
    }
  else
    last_rval = decay_conversion (expr, complain);

  if (TYPE_PTR_P (TREE_TYPE (last_rval)))
    {
      if (processing_template_decl)
	{
	  expr = build_min (ARROW_EXPR, TREE_TYPE (TREE_TYPE (last_rval)),
			    orig_expr);
	  TREE_SIDE_EFFECTS (expr) = TREE_SIDE_EFFECTS (last_rval);
	  return expr;
	}

      return cp_build_indirect_ref (last_rval, RO_NULL, complain);
    }

  if (complain & tf_error)
    {
      if (types_memoized)
	error ("result of %<operator->()%> yields non-pointer result");
      else
	error ("base operand of %<->%> is not a pointer");
    }
  return error_mark_node;
}

/* Return an expression for "DATUM .* COMPONENT".  DATUM has not
   already been checked out to be of aggregate type.  */

tree
build_m_component_ref (tree datum, tree component, tsubst_flags_t complain)
{
  tree ptrmem_type;
  tree objtype;
  tree type;
  tree binfo;
  tree ctype;

  if (error_operand_p (datum) || error_operand_p (component))
    return error_mark_node;

  datum = mark_lvalue_use (datum);
  component = mark_rvalue_use (component);

  ptrmem_type = TREE_TYPE (component);
  if (!TYPE_PTRMEM_P (ptrmem_type))
    {
      if (complain & tf_error)
	error ("%qE cannot be used as a member pointer, since it is of "
	       "type %qT", component, ptrmem_type);
      return error_mark_node;
    }

  objtype = TYPE_MAIN_VARIANT (TREE_TYPE (datum));
  if (! MAYBE_CLASS_TYPE_P (objtype))
    {
      if (complain & tf_error)
	error ("cannot apply member pointer %qE to %qE, which is of "
	       "non-class type %qT", component, datum, objtype);
      return error_mark_node;
    }

  type = TYPE_PTRMEM_POINTED_TO_TYPE (ptrmem_type);
  ctype = complete_type (TYPE_PTRMEM_CLASS_TYPE (ptrmem_type));

  if (!COMPLETE_TYPE_P (ctype))
    {
      if (!same_type_p (ctype, objtype))
	goto mismatch;
      binfo = NULL;
    }
  else
    {
      binfo = lookup_base (objtype, ctype, ba_check, NULL, complain);

      if (!binfo)
	{
	mismatch:
	  if (complain & tf_error)
	    error ("pointer to member type %qT incompatible with object "
		   "type %qT", type, objtype);
	  return error_mark_node;
	}
      else if (binfo == error_mark_node)
	return error_mark_node;
    }

  if (TYPE_PTRDATAMEM_P (ptrmem_type))
    {
      cp_lvalue_kind kind = lvalue_kind (datum);
      tree ptype;

      /* Compute the type of the field, as described in [expr.ref].
	 There's no such thing as a mutable pointer-to-member, so
	 things are not as complex as they are for references to
	 non-static data members.  */
      type = cp_build_qualified_type (type,
				      (cp_type_quals (type)
				       | cp_type_quals (TREE_TYPE (datum))));

      datum = build_address (datum);

      /* Convert object to the correct base.  */
      if (binfo)
	{
	  datum = build_base_path (PLUS_EXPR, datum, binfo, 1, complain);
	  if (datum == error_mark_node)
	    return error_mark_node;
	}

      /* Build an expression for "object + offset" where offset is the
	 value stored in the pointer-to-data-member.  */
      ptype = build_pointer_type (type);
      datum = fold_build_pointer_plus (fold_convert (ptype, datum), component);
      datum = cp_build_indirect_ref (datum, RO_NULL, complain);
      if (datum == error_mark_node)
	return error_mark_node;

      /* If the object expression was an rvalue, return an rvalue.  */
      if (kind & clk_class)
	datum = rvalue (datum);
      else if (kind & clk_rvalueref)
	datum = move (datum);
      return datum;
    }
  else
    {
      /* 5.5/6: In a .* expression whose object expression is an rvalue, the
	 program is ill-formed if the second operand is a pointer to member
	 function with ref-qualifier &. In a .* expression whose object
	 expression is an lvalue, the program is ill-formed if the second
	 operand is a pointer to member function with ref-qualifier &&.  */
      if (FUNCTION_REF_QUALIFIED (type))
	{
	  bool lval = lvalue_p (datum);
	  if (lval && FUNCTION_RVALUE_QUALIFIED (type))
	    {
	      if (complain & tf_error)
		error ("pointer-to-member-function type %qT requires an rvalue",
		       ptrmem_type);
	      return error_mark_node;
	    }
	  else if (!lval && !FUNCTION_RVALUE_QUALIFIED (type))
	    {
	      if (complain & tf_error)
		error ("pointer-to-member-function type %qT requires an lvalue",
		       ptrmem_type);
	      return error_mark_node;
	    }
	}
      return build2 (OFFSET_REF, type, datum, component);
    }
}

/* Return a tree node for the expression TYPENAME '(' PARMS ')'.  */

tree
build_functional_cast (tree exp, tree parms, tsubst_flags_t complain)
{
  /* This is either a call to a constructor,
     or a C cast in C++'s `functional' notation.  */

  /* The type to which we are casting.  */
  tree type;
  vec<tree, va_gc> *parmvec;

  if (error_operand_p (exp) || parms == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (exp) == TYPE_DECL)
    {
      type = TREE_TYPE (exp);

      if (complain & tf_warning
	  && TREE_DEPRECATED (type)
	  && DECL_ARTIFICIAL (exp))
	warn_deprecated_use (type, NULL_TREE);
    }
  else
    type = exp;

  /* We need to check this explicitly, since value-initialization of
     arrays is allowed in other situations.  */
  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      if (complain & tf_error)
	error ("functional cast to array type %qT", type);
      return error_mark_node;
    }

  if (tree anode = type_uses_auto (type))
    {
      if (!CLASS_PLACEHOLDER_TEMPLATE (anode))
	{
	  if (complain & tf_error)
	    error ("invalid use of %qT", anode);
	  return error_mark_node;
	}
      else if (!parms)
	{
	  if (complain & tf_error)
	    error ("cannot deduce template arguments for %qT from ()", anode);
	  return error_mark_node;
	}
      else
	type = do_auto_deduction (type, parms, anode, complain,
				  adc_variable_type);
    }

  if (processing_template_decl)
    {
      tree t;

      /* Diagnose this even in a template.  We could also try harder
	 to give all the usual errors when the type and args are
	 non-dependent...  */
      if (TREE_CODE (type) == REFERENCE_TYPE && !parms)
	{
	  if (complain & tf_error)
	    error ("invalid value-initialization of reference type");
	  return error_mark_node;
	}

      t = build_min (CAST_EXPR, type, parms);
      /* We don't know if it will or will not have side effects.  */
      TREE_SIDE_EFFECTS (t) = 1;
      return t;
    }

  if (! MAYBE_CLASS_TYPE_P (type))
    {
      if (parms == NULL_TREE)
	{
	  if (VOID_TYPE_P (type))
	    return void_node;
	  return build_value_init (cv_unqualified (type), complain);
	}

      /* This must build a C cast.  */
      parms = build_x_compound_expr_from_list (parms, ELK_FUNC_CAST, complain);
      return cp_build_c_cast (type, parms, complain);
    }

  /* Prepare to evaluate as a call to a constructor.  If this expression
     is actually used, for example,

     return X (arg1, arg2, ...);

     then the slot being initialized will be filled in.  */

  if (!complete_type_or_maybe_complain (type, NULL_TREE, complain))
    return error_mark_node;
  if (abstract_virtuals_error_sfinae (ACU_CAST, type, complain))
    return error_mark_node;

  /* [expr.type.conv]

     If the expression list is a single-expression, the type
     conversion is equivalent (in definedness, and if defined in
     meaning) to the corresponding cast expression.  */
  if (parms && TREE_CHAIN (parms) == NULL_TREE)
    return cp_build_c_cast (type, TREE_VALUE (parms), complain);

  /* [expr.type.conv]

     The expression T(), where T is a simple-type-specifier for a
     non-array complete object type or the (possibly cv-qualified)
     void type, creates an rvalue of the specified type, which is
     value-initialized.  */

  if (parms == NULL_TREE)
    {
      exp = build_value_init (type, complain);
      exp = get_target_expr_sfinae (exp, complain);
      return exp;
    }

  /* Call the constructor.  */
  parmvec = make_tree_vector ();
  for (; parms != NULL_TREE; parms = TREE_CHAIN (parms))
    vec_safe_push (parmvec, TREE_VALUE (parms));
  exp = build_special_member_call (NULL_TREE, complete_ctor_identifier,
				   &parmvec, type, LOOKUP_NORMAL, complain);
  release_tree_vector (parmvec);

  if (exp == error_mark_node)
    return error_mark_node;

  return build_cplus_new (type, exp, complain);
}


/* Add new exception specifier SPEC, to the LIST we currently have.
   If it's already in LIST then do nothing.
   Moan if it's bad and we're allowed to. COMPLAIN < 0 means we
   know what we're doing.  */

tree
add_exception_specifier (tree list, tree spec, int complain)
{
  bool ok;
  tree core = spec;
  bool is_ptr;
  diagnostic_t diag_type = DK_UNSPECIFIED; /* none */

  if (spec == error_mark_node)
    return list;

  gcc_assert (spec && (!list || TREE_VALUE (list)));

  /* [except.spec] 1, type in an exception specifier shall not be
     incomplete, or pointer or ref to incomplete other than pointer
     to cv void.  */
  is_ptr = TYPE_PTR_P (core);
  if (is_ptr || TREE_CODE (core) == REFERENCE_TYPE)
    core = TREE_TYPE (core);
  if (complain < 0)
    ok = true;
  else if (VOID_TYPE_P (core))
    ok = is_ptr;
  else if (TREE_CODE (core) == TEMPLATE_TYPE_PARM)
    ok = true;
  else if (processing_template_decl)
    ok = true;
  else
    {
      ok = true;
      /* 15.4/1 says that types in an exception specifier must be complete,
	 but it seems more reasonable to only require this on definitions
	 and calls.  So just give a pedwarn at this point; we will give an
	 error later if we hit one of those two cases.  */
      if (!COMPLETE_TYPE_P (complete_type (core)))
	diag_type = DK_PEDWARN; /* pedwarn */
    }

  if (ok)
    {
      tree probe;

      for (probe = list; probe; probe = TREE_CHAIN (probe))
	if (same_type_p (TREE_VALUE (probe), spec))
	  break;
      if (!probe)
	list = tree_cons (NULL_TREE, spec, list);
    }
  else
    diag_type = DK_ERROR; /* error */

  if (diag_type != DK_UNSPECIFIED
      && (complain & tf_warning_or_error))
    cxx_incomplete_type_diagnostic (NULL_TREE, core, diag_type);

  return list;
}

/* Like nothrow_spec_p, but don't abort on deferred noexcept.  */

static bool
nothrow_spec_p_uninst (const_tree spec)
{
  if (DEFERRED_NOEXCEPT_SPEC_P (spec))
    return false;
  return nothrow_spec_p (spec);
}

/* Combine the two exceptions specifier lists LIST and ADD, and return
   their union.  */

tree
merge_exception_specifiers (tree list, tree add)
{
  tree noex, orig_list;

  /* No exception-specifier or noexcept(false) are less strict than
     anything else.  Prefer the newer variant (LIST).  */
  if (!list || list == noexcept_false_spec)
    return list;
  else if (!add || add == noexcept_false_spec)
    return add;

  /* noexcept(true) and throw() are stricter than anything else.
     As above, prefer the more recent one (LIST).  */
  if (nothrow_spec_p_uninst (add))
    return list;

  /* Two implicit noexcept specs (e.g. on a destructor) are equivalent.  */
  if (UNEVALUATED_NOEXCEPT_SPEC_P (add)
      && UNEVALUATED_NOEXCEPT_SPEC_P (list))
    return list;
  /* We should have instantiated other deferred noexcept specs by now.  */
  gcc_assert (!DEFERRED_NOEXCEPT_SPEC_P (add));

  if (nothrow_spec_p_uninst (list))
    return add;
  noex = TREE_PURPOSE (list);
  gcc_checking_assert (!TREE_PURPOSE (add)
		       || errorcount || !flag_exceptions
		       || cp_tree_equal (noex, TREE_PURPOSE (add)));

  /* Combine the dynamic-exception-specifiers, if any.  */
  orig_list = list;
  for (; add && TREE_VALUE (add); add = TREE_CHAIN (add))
    {
      tree spec = TREE_VALUE (add);
      tree probe;

      for (probe = orig_list; probe && TREE_VALUE (probe);
	   probe = TREE_CHAIN (probe))
	if (same_type_p (TREE_VALUE (probe), spec))
	  break;
      if (!probe)
	{
	  spec = build_tree_list (NULL_TREE, spec);
	  TREE_CHAIN (spec) = list;
	  list = spec;
	}
    }

  /* Keep the noexcept-specifier at the beginning of the list.  */
  if (noex != TREE_PURPOSE (list))
    list = tree_cons (noex, TREE_VALUE (list), TREE_CHAIN (list));

  return list;
}

/* Subroutine of build_call.  Ensure that each of the types in the
   exception specification is complete.  Technically, 15.4/1 says that
   they need to be complete when we see a declaration of the function,
   but we should be able to get away with only requiring this when the
   function is defined or called.  See also add_exception_specifier.  */

void
require_complete_eh_spec_types (tree fntype, tree decl)
{
  tree raises;
  /* Don't complain about calls to op new.  */
  if (decl && DECL_ARTIFICIAL (decl))
    return;
  for (raises = TYPE_RAISES_EXCEPTIONS (fntype); raises;
       raises = TREE_CHAIN (raises))
    {
      tree type = TREE_VALUE (raises);
      if (type && !COMPLETE_TYPE_P (type))
	{
	  if (decl)
	    error
	      ("call to function %qD which throws incomplete type %q#T",
	       decl, type);
	  else
	    error ("call to function which throws incomplete type %q#T",
		   decl);
	}
    }
}


#include "gt-cp-typeck2.h"
