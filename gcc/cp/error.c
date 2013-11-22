/* Call-backs for C++ error reporting.
   This code is non-reentrant.
   Copyright (C) 1993-2013 Free Software Foundation, Inc.
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
#include "tm.h"
#include "tree.h"
#include "stringpool.h"
#include "cp-tree.h"
#include "flags.h"
#include "diagnostic.h"
#include "tree-diagnostic.h"
#include "langhooks-def.h"
#include "intl.h"
#include "cxx-pretty-print.h"
#include "tree-pretty-print.h"
#include "pointer-set.h"
#include "c-family/c-objc.h"
#include "ubsan.h"

#include <new>                    // For placement-new.

#define pp_separate_with_comma(PP) pp_cxx_separate_with (PP, ',')
#define pp_separate_with_semicolon(PP) pp_cxx_separate_with (PP, ';')

/* The global buffer where we dump everything.  It is there only for
   transitional purpose.  It is expected, in the near future, to be
   completely removed.  */
static cxx_pretty_printer scratch_pretty_printer;
#define cxx_pp (&scratch_pretty_printer)

/* Translate if being used for diagnostics, but not for dump files or
   __PRETTY_FUNCTION.  */
#define M_(msgid) (pp_translate_identifiers (cxx_pp) ? _(msgid) : (msgid))

# define NEXT_CODE(T) (TREE_CODE (TREE_TYPE (T)))

static const char *args_to_string (tree, int);
static const char *assop_to_string (enum tree_code);
static const char *code_to_string (enum tree_code);
static const char *cv_to_string (tree, int);
static const char *decl_to_string (tree, int);
static const char *expr_to_string (tree);
static const char *fndecl_to_string (tree, int);
static const char *op_to_string	(enum tree_code);
static const char *parm_to_string (int);
static const char *type_to_string (tree, int);

static void dump_alias_template_specialization (cxx_pretty_printer *, tree, int);
static void dump_type (cxx_pretty_printer *, tree, int);
static void dump_typename (cxx_pretty_printer *, tree, int);
static void dump_simple_decl (cxx_pretty_printer *, tree, tree, int);
static void dump_decl (cxx_pretty_printer *, tree, int);
static void dump_template_decl (cxx_pretty_printer *, tree, int);
static void dump_function_decl (cxx_pretty_printer *, tree, int);
static void dump_expr (cxx_pretty_printer *, tree, int);
static void dump_unary_op (cxx_pretty_printer *, const char *, tree, int);
static void dump_binary_op (cxx_pretty_printer *, const char *, tree, int);
static void dump_aggr_type (cxx_pretty_printer *, tree, int);
static void dump_type_prefix (cxx_pretty_printer *, tree, int);
static void dump_type_suffix (cxx_pretty_printer *, tree, int);
static void dump_function_name (cxx_pretty_printer *, tree, int);
static void dump_call_expr_args (cxx_pretty_printer *, tree, int, bool);
static void dump_aggr_init_expr_args (cxx_pretty_printer *, tree, int, bool);
static void dump_expr_list (cxx_pretty_printer *, tree, int);
static void dump_global_iord (cxx_pretty_printer *, tree);
static void dump_parameters (cxx_pretty_printer *, tree, int);
static void dump_ref_qualifier (cxx_pretty_printer *, tree, int);
static void dump_exception_spec (cxx_pretty_printer *, tree, int);
static void dump_template_argument (cxx_pretty_printer *, tree, int);
static void dump_template_argument_list (cxx_pretty_printer *, tree, int);
static void dump_template_parameter (cxx_pretty_printer *, tree, int);
static void dump_template_bindings (cxx_pretty_printer *, tree, tree,
                                    vec<tree, va_gc> *);
static void dump_scope (cxx_pretty_printer *, tree, int);
static void dump_template_parms (cxx_pretty_printer *, tree, int, int);
static int get_non_default_template_args_count (tree, int);
static const char *function_category (tree);
static void maybe_print_constexpr_context (diagnostic_context *);
static void maybe_print_instantiation_context (diagnostic_context *);
static void print_instantiation_full_context (diagnostic_context *);
static void print_instantiation_partial_context (diagnostic_context *,
						 struct tinst_level *,
						 location_t);
static void cp_diagnostic_starter (diagnostic_context *, diagnostic_info *);
static void cp_diagnostic_finalizer (diagnostic_context *, diagnostic_info *);
static void cp_print_error_function (diagnostic_context *, diagnostic_info *);

static bool cp_printer (pretty_printer *, text_info *, const char *,
			int, bool, bool, bool);

void
init_error (void)
{
  diagnostic_starter (global_dc) = cp_diagnostic_starter;
  diagnostic_finalizer (global_dc) = cp_diagnostic_finalizer;
  diagnostic_format_decoder (global_dc) = cp_printer;

  new (cxx_pp) cxx_pretty_printer ();
}

/* Dump a scope, if deemed necessary.  */

static void
dump_scope (cxx_pretty_printer *pp, tree scope, int flags)
{
  int f = flags & (TFF_SCOPE | TFF_CHASE_TYPEDEF);

  if (scope == NULL_TREE)
    return;

  if (TREE_CODE (scope) == NAMESPACE_DECL)
    {
      if (scope != global_namespace)
	{
          dump_decl (pp, scope, f);
	  pp_cxx_colon_colon (pp);
	}
    }
  else if (AGGREGATE_TYPE_P (scope))
    {
      dump_type (pp, scope, f);
      pp_cxx_colon_colon (pp);
    }
  else if ((flags & TFF_SCOPE) && TREE_CODE (scope) == FUNCTION_DECL)
    {
      dump_function_decl (pp, scope, f);
      pp_cxx_colon_colon (pp);
    }
}

/* Dump the template ARGument under control of FLAGS.  */

static void
dump_template_argument (cxx_pretty_printer *pp, tree arg, int flags)
{
  if (ARGUMENT_PACK_P (arg))
    dump_template_argument_list (pp, ARGUMENT_PACK_ARGS (arg),
				 /* No default args in argument packs.  */
				 flags|TFF_NO_OMIT_DEFAULT_TEMPLATE_ARGUMENTS);
  else if (TYPE_P (arg) || TREE_CODE (arg) == TEMPLATE_DECL)
    dump_type (pp, arg, flags & ~TFF_CLASS_KEY_OR_ENUM);
  else
    {
      if (TREE_CODE (arg) == TREE_LIST)
	arg = TREE_VALUE (arg);

      dump_expr (pp, arg, (flags | TFF_EXPR_IN_PARENS) & ~TFF_CLASS_KEY_OR_ENUM);
    }
}

/* Count the number of template arguments ARGS whose value does not
   match the (optional) default template parameter in PARAMS  */

static int
get_non_default_template_args_count (tree args, int flags)
{
  int n = TREE_VEC_LENGTH (INNERMOST_TEMPLATE_ARGS (args));

  if (/* We use this flag when generating debug information.  We don't
	 want to expand templates at this point, for this may generate
	 new decls, which gets decl counts out of sync, which may in
	 turn cause codegen differences between compilations with and
	 without -g.  */
      (flags & TFF_NO_OMIT_DEFAULT_TEMPLATE_ARGUMENTS) != 0
      || !flag_pretty_templates)
    return n;

  return GET_NON_DEFAULT_TEMPLATE_ARGS_COUNT (INNERMOST_TEMPLATE_ARGS (args));
}

/* Dump a template-argument-list ARGS (always a TREE_VEC) under control
   of FLAGS.  */

static void
dump_template_argument_list (cxx_pretty_printer *pp, tree args, int flags)
{
  int n = get_non_default_template_args_count (args, flags);
  int need_comma = 0;
  int i;

  for (i = 0; i < n; ++i)
    {
      tree arg = TREE_VEC_ELT (args, i);

      /* Only print a comma if we know there is an argument coming. In
         the case of an empty template argument pack, no actual
         argument will be printed.  */
      if (need_comma
          && (!ARGUMENT_PACK_P (arg)
              || TREE_VEC_LENGTH (ARGUMENT_PACK_ARGS (arg)) > 0))
	pp_separate_with_comma (pp);

      dump_template_argument (pp, arg, flags);
      need_comma = 1;
    }
}

/* Dump a template parameter PARM (a TREE_LIST) under control of FLAGS.  */

static void
dump_template_parameter (cxx_pretty_printer *pp, tree parm, int flags)
{
  tree p;
  tree a;

  if (parm == error_mark_node)
   return;

  p = TREE_VALUE (parm);
  a = TREE_PURPOSE (parm);

  if (TREE_CODE (p) == TYPE_DECL)
    {
      if (flags & TFF_DECL_SPECIFIERS)
	{
	  pp_cxx_ws_string (pp, "class");
          if (TEMPLATE_TYPE_PARAMETER_PACK (TREE_TYPE (p)))
            pp_cxx_ws_string (pp, "...");
	  if (DECL_NAME (p))
	    pp_cxx_tree_identifier (pp, DECL_NAME (p));
	}
      else if (DECL_NAME (p))
	pp_cxx_tree_identifier (pp, DECL_NAME (p));
      else
	pp_cxx_canonical_template_parameter (pp, TREE_TYPE (p));
    }
  else
    dump_decl (pp, p, flags | TFF_DECL_SPECIFIERS);

  if ((flags & TFF_FUNCTION_DEFAULT_ARGUMENTS) && a != NULL_TREE)
    {
      pp_cxx_whitespace (pp);
      pp_equal (pp);
      pp_cxx_whitespace (pp);
      if (TREE_CODE (p) == TYPE_DECL || TREE_CODE (p) == TEMPLATE_DECL)
	dump_type (pp, a, flags & ~TFF_CHASE_TYPEDEF);
      else
	dump_expr (pp, a, flags | TFF_EXPR_IN_PARENS);
    }
}

/* Dump, under control of FLAGS, a template-parameter-list binding.
   PARMS is a TREE_LIST of TREE_VEC of TREE_LIST and ARGS is a
   TREE_VEC.  */

static void
dump_template_bindings (cxx_pretty_printer *pp, tree parms, tree args,
                        vec<tree, va_gc> *typenames)
{
  bool need_semicolon = false;
  int i;
  tree t;

  while (parms)
    {
      tree p = TREE_VALUE (parms);
      int lvl = TMPL_PARMS_DEPTH (parms);
      int arg_idx = 0;
      int i;
      tree lvl_args = NULL_TREE;

      /* Don't crash if we had an invalid argument list.  */
      if (TMPL_ARGS_DEPTH (args) >= lvl)
	lvl_args = TMPL_ARGS_LEVEL (args, lvl);

      for (i = 0; i < TREE_VEC_LENGTH (p); ++i)
	{
	  tree arg = NULL_TREE;

	  /* Don't crash if we had an invalid argument list.  */
	  if (lvl_args && NUM_TMPL_ARGS (lvl_args) > arg_idx)
	    arg = TREE_VEC_ELT (lvl_args, arg_idx);

	  if (need_semicolon)
	    pp_separate_with_semicolon (pp);
	  dump_template_parameter (pp, TREE_VEC_ELT (p, i),
                                   TFF_PLAIN_IDENTIFIER);
	  pp_cxx_whitespace (pp);
	  pp_equal (pp);
	  pp_cxx_whitespace (pp);
	  if (arg)
	    {
	      if (ARGUMENT_PACK_P (arg))
		pp_cxx_left_brace (pp);
	      dump_template_argument (pp, arg, TFF_PLAIN_IDENTIFIER);
	      if (ARGUMENT_PACK_P (arg))
		pp_cxx_right_brace (pp);
	    }
	  else
	    pp_string (pp, M_("<missing>"));

	  ++arg_idx;
	  need_semicolon = true;
	}

      parms = TREE_CHAIN (parms);
    }

  /* Don't bother with typenames for a partial instantiation.  */
  if (vec_safe_is_empty (typenames) || uses_template_parms (args))
    return;

  FOR_EACH_VEC_SAFE_ELT (typenames, i, t)
    {
      if (need_semicolon)
	pp_separate_with_semicolon (pp);
      dump_type (pp, t, TFF_PLAIN_IDENTIFIER);
      pp_cxx_whitespace (pp);
      pp_equal (pp);
      pp_cxx_whitespace (pp);
      push_deferring_access_checks (dk_no_check);
      t = tsubst (t, args, tf_none, NULL_TREE);
      pop_deferring_access_checks ();
      /* Strip typedefs.  We can't just use TFF_CHASE_TYPEDEF because
	 pp_simple_type_specifier doesn't know about it.  */
      t = strip_typedefs (t);
      dump_type (pp, t, TFF_PLAIN_IDENTIFIER);
    }
}

/* Dump a human-readable equivalent of the alias template
   specialization of T.  */

static void
dump_alias_template_specialization (cxx_pretty_printer *pp, tree t, int flags)
{
  tree name;

  gcc_assert (alias_template_specialization_p (t));

  if (!(flags & TFF_UNQUALIFIED_NAME))
    dump_scope (pp, CP_DECL_CONTEXT (TYPE_NAME (t)), flags);
  name = TYPE_IDENTIFIER (t);
  pp_cxx_tree_identifier (pp, name);
  dump_template_parms (pp, TYPE_TEMPLATE_INFO (t),
		       /*primary=*/false,
		       flags & ~TFF_TEMPLATE_HEADER);
}

/* Dump a human-readable equivalent of TYPE.  FLAGS controls the
   format.  */

static void
dump_type (cxx_pretty_printer *pp, tree t, int flags)
{
  if (t == NULL_TREE)
    return;

  /* Don't print e.g. "struct mytypedef".  */
  if (TYPE_P (t) && typedef_variant_p (t))
    {
      tree decl = TYPE_NAME (t);
      if ((flags & TFF_CHASE_TYPEDEF)
	       || DECL_SELF_REFERENCE_P (decl)
	       || (!flag_pretty_templates
		   && DECL_LANG_SPECIFIC (decl) && DECL_TEMPLATE_INFO (decl)))
	t = strip_typedefs (t);
      else if (alias_template_specialization_p (t))
	{
	  dump_alias_template_specialization (pp, t, flags);
	  return;
	}
      else if (same_type_p (t, TREE_TYPE (decl)))
	t = decl;
      else
	{
	  pp_cxx_cv_qualifier_seq (pp, t);
	  pp_cxx_tree_identifier (pp, TYPE_IDENTIFIER (t));
	  return;
	}
    }

  if (TYPE_PTRMEMFUNC_P (t))
    goto offset_type;

  switch (TREE_CODE (t))
    {
    case LANG_TYPE:
      if (t == init_list_type_node)
	pp_string (pp, M_("<brace-enclosed initializer list>"));
      else if (t == unknown_type_node)
	pp_string (pp, M_("<unresolved overloaded function type>"));
      else
	{
	  pp_cxx_cv_qualifier_seq (pp, t);
	  pp_cxx_tree_identifier (pp, TYPE_IDENTIFIER (t));
	}
      break;

    case TREE_LIST:
      /* A list of function parms.  */
      dump_parameters (pp, t, flags);
      break;

    case IDENTIFIER_NODE:
      pp_cxx_tree_identifier (pp, t);
      break;

    case TREE_BINFO:
      dump_type (pp, BINFO_TYPE (t), flags);
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
    case ENUMERAL_TYPE:
      dump_aggr_type (pp, t, flags);
      break;

    case TYPE_DECL:
      if (flags & TFF_CHASE_TYPEDEF)
	{
	  dump_type (pp, DECL_ORIGINAL_TYPE (t)
		     ? DECL_ORIGINAL_TYPE (t) : TREE_TYPE (t), flags);
	  break;
	}
      /* Else fall through.  */

    case TEMPLATE_DECL:
    case NAMESPACE_DECL:
      dump_decl (pp, t, flags & ~TFF_DECL_SPECIFIERS);
      break;

    case INTEGER_TYPE:
    case REAL_TYPE:
    case VOID_TYPE:
    case BOOLEAN_TYPE:
    case COMPLEX_TYPE:
    case VECTOR_TYPE:
    case FIXED_POINT_TYPE:
      pp_type_specifier_seq (pp, t);
      break;

    case TEMPLATE_TEMPLATE_PARM:
      /* For parameters inside template signature.  */
      if (TYPE_IDENTIFIER (t))
	pp_cxx_tree_identifier (pp, TYPE_IDENTIFIER (t));
      else
	pp_cxx_canonical_template_parameter (pp, t);
      break;

    case BOUND_TEMPLATE_TEMPLATE_PARM:
      {
	tree args = TYPE_TI_ARGS (t);
	pp_cxx_cv_qualifier_seq (pp, t);
	pp_cxx_tree_identifier (pp, TYPE_IDENTIFIER (t));
	pp_cxx_begin_template_argument_list (pp);
	dump_template_argument_list (pp, args, flags);
	pp_cxx_end_template_argument_list (pp);
      }
      break;

    case TEMPLATE_TYPE_PARM:
      pp_cxx_cv_qualifier_seq (pp, t);
      if (TYPE_IDENTIFIER (t))
	pp_cxx_tree_identifier (pp, TYPE_IDENTIFIER (t));
      else
	pp_cxx_canonical_template_parameter
	  (pp, TEMPLATE_TYPE_PARM_INDEX (t));
      break;

      /* This is not always necessary for pointers and such, but doing this
	 reduces code size.  */
    case ARRAY_TYPE:
    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case OFFSET_TYPE:
    offset_type:
    case FUNCTION_TYPE:
    case METHOD_TYPE:
    {
      dump_type_prefix (pp, t, flags);
      dump_type_suffix (pp, t, flags);
      break;
    }
    case TYPENAME_TYPE:
      if (! (flags & TFF_CHASE_TYPEDEF)
	  && DECL_ORIGINAL_TYPE (TYPE_NAME (t)))
	{
	  dump_decl (pp, TYPE_NAME (t), TFF_PLAIN_IDENTIFIER);
	  break;
	}
      pp_cxx_cv_qualifier_seq (pp, t);
      pp_cxx_ws_string (pp,
			 TYPENAME_IS_ENUM_P (t) ? "enum"
			 : TYPENAME_IS_CLASS_P (t) ? "class"
			 : "typename");
      dump_typename (pp, t, flags);
      break;

    case UNBOUND_CLASS_TEMPLATE:
      if (! (flags & TFF_UNQUALIFIED_NAME))
	{
	  dump_type (pp, TYPE_CONTEXT (t), flags);
	  pp_cxx_colon_colon (pp);
	}
      pp_cxx_ws_string (pp, "template");
      dump_type (pp, DECL_NAME (TYPE_NAME (t)), flags);
      break;

    case TYPEOF_TYPE:
      pp_cxx_ws_string (pp, "__typeof__");
      pp_cxx_whitespace (pp);
      pp_cxx_left_paren (pp);
      dump_expr (pp, TYPEOF_TYPE_EXPR (t), flags & ~TFF_EXPR_IN_PARENS);
      pp_cxx_right_paren (pp);
      break;

    case UNDERLYING_TYPE:
      pp_cxx_ws_string (pp, "__underlying_type");
      pp_cxx_whitespace (pp);
      pp_cxx_left_paren (pp);
      dump_expr (pp, UNDERLYING_TYPE_TYPE (t), flags & ~TFF_EXPR_IN_PARENS);
      pp_cxx_right_paren (pp);
      break;

    case TYPE_PACK_EXPANSION:
      dump_type (pp, PACK_EXPANSION_PATTERN (t), flags);
      pp_cxx_ws_string (pp, "...");
      break;

    case TYPE_ARGUMENT_PACK:
      dump_template_argument (pp, t, flags);
      break;

    case DECLTYPE_TYPE:
      pp_cxx_ws_string (pp, "decltype");
      pp_cxx_whitespace (pp);
      pp_cxx_left_paren (pp);
      dump_expr (pp, DECLTYPE_TYPE_EXPR (t), flags & ~TFF_EXPR_IN_PARENS);
      pp_cxx_right_paren (pp);
      break;

    case NULLPTR_TYPE:
      pp_string (pp, "std::nullptr_t");
      break;

    default:
      pp_unsupported_tree (pp, t);
      /* Fall through to error.  */

    case ERROR_MARK:
      pp_string (pp, M_("<type error>"));
      break;
    }
}

/* Dump a TYPENAME_TYPE. We need to notice when the context is itself
   a TYPENAME_TYPE.  */

static void
dump_typename (cxx_pretty_printer *pp, tree t, int flags)
{
  tree ctx = TYPE_CONTEXT (t);

  if (TREE_CODE (ctx) == TYPENAME_TYPE)
    dump_typename (pp, ctx, flags);
  else
    dump_type (pp, ctx, flags & ~TFF_CLASS_KEY_OR_ENUM);
  pp_cxx_colon_colon (pp);
  dump_decl (pp, TYPENAME_TYPE_FULLNAME (t), flags);
}

/* Return the name of the supplied aggregate, or enumeral type.  */

const char *
class_key_or_enum_as_string (tree t)
{
  if (TREE_CODE (t) == ENUMERAL_TYPE) 
    {
      if (SCOPED_ENUM_P (t))
        return "enum class";
      else
        return "enum";
    }
  else if (TREE_CODE (t) == UNION_TYPE)
    return "union";
  else if (TYPE_LANG_SPECIFIC (t) && CLASSTYPE_DECLARED_CLASS (t))
    return "class";
  else
    return "struct";
}

/* Print out a class declaration T under the control of FLAGS,
   in the form `class foo'.  */

static void
dump_aggr_type (cxx_pretty_printer *pp, tree t, int flags)
{
  tree name;
  const char *variety = class_key_or_enum_as_string (t);
  int typdef = 0;
  int tmplate = 0;

  pp_cxx_cv_qualifier_seq (pp, t);

  if (flags & TFF_CLASS_KEY_OR_ENUM)
    pp_cxx_ws_string (pp, variety);

  name = TYPE_NAME (t);

  if (name)
    {
      typdef = (!DECL_ARTIFICIAL (name)
		/* An alias specialization is not considered to be a
		   typedef.  */
		&& !alias_template_specialization_p (t));

      if ((typdef
	   && ((flags & TFF_CHASE_TYPEDEF)
	       || (!flag_pretty_templates && DECL_LANG_SPECIFIC (name)
		   && DECL_TEMPLATE_INFO (name))))
	  || DECL_SELF_REFERENCE_P (name))
	{
	  t = TYPE_MAIN_VARIANT (t);
	  name = TYPE_NAME (t);
	  typdef = 0;
	}

      tmplate = !typdef && TREE_CODE (t) != ENUMERAL_TYPE
		&& TYPE_LANG_SPECIFIC (t) && CLASSTYPE_TEMPLATE_INFO (t)
		&& (TREE_CODE (CLASSTYPE_TI_TEMPLATE (t)) != TEMPLATE_DECL
		    || PRIMARY_TEMPLATE_P (CLASSTYPE_TI_TEMPLATE (t)));
      
      if (! (flags & TFF_UNQUALIFIED_NAME))
	dump_scope (pp, CP_DECL_CONTEXT (name), flags | TFF_SCOPE);
      flags &= ~TFF_UNQUALIFIED_NAME;
      if (tmplate)
	{
	  /* Because the template names are mangled, we have to locate
	     the most general template, and use that name.  */
	  tree tpl = TYPE_TI_TEMPLATE (t);

	  while (DECL_TEMPLATE_INFO (tpl))
	    tpl = DECL_TI_TEMPLATE (tpl);
	  name = tpl;
	}
      name = DECL_NAME (name);
    }

  if (name == 0 || ANON_AGGRNAME_P (name))
    {
      if (flags & TFF_CLASS_KEY_OR_ENUM)
	pp_string (pp, M_("<anonymous>"));
      else
	pp_printf (pp, M_("<anonymous %s>"), variety);
    }
  else if (LAMBDA_TYPE_P (t))
    {
      /* A lambda's "type" is essentially its signature.  */
      pp_string (pp, M_("<lambda"));
      if (lambda_function (t))
	dump_parameters (pp,
                         FUNCTION_FIRST_USER_PARMTYPE (lambda_function (t)),
			 flags);
      pp_greater (pp);
    }
  else
    pp_cxx_tree_identifier (pp, name);
  if (tmplate)
    dump_template_parms (pp, TYPE_TEMPLATE_INFO (t),
			 !CLASSTYPE_USE_TEMPLATE (t),
			 flags & ~TFF_TEMPLATE_HEADER);
}

/* Dump into the obstack the initial part of the output for a given type.
   This is necessary when dealing with things like functions returning
   functions.  Examples:

   return type of `int (* fee ())()': pointer -> function -> int.  Both
   pointer (and reference and offset) and function (and member) types must
   deal with prefix and suffix.

   Arrays must also do this for DECL nodes, like int a[], and for things like
   int *[]&.  */

static void
dump_type_prefix (cxx_pretty_printer *pp, tree t, int flags)
{
  if (TYPE_PTRMEMFUNC_P (t))
    {
      t = TYPE_PTRMEMFUNC_FN_TYPE (t);
      goto offset_type;
    }

  switch (TREE_CODE (t))
    {
    case POINTER_TYPE:
    case REFERENCE_TYPE:
      {
	tree sub = TREE_TYPE (t);

	dump_type_prefix (pp, sub, flags);
	if (TREE_CODE (sub) == ARRAY_TYPE
	    || TREE_CODE (sub) == FUNCTION_TYPE)
	  {
	    pp_cxx_whitespace (pp);
	    pp_cxx_left_paren (pp);
	    pp_c_attributes_display (pp, TYPE_ATTRIBUTES (sub));
	  }
	if (TYPE_PTR_P (t))
	  pp_star (pp);
	else if (TREE_CODE (t) == REFERENCE_TYPE)
	{
	  if (TYPE_REF_IS_RVALUE (t))
	    pp_ampersand_ampersand (pp);
	  else
	    pp_ampersand (pp);
	}
	pp->padding = pp_before;
	pp_cxx_cv_qualifier_seq (pp, t);
      }
      break;

    case OFFSET_TYPE:
    offset_type:
      dump_type_prefix (pp, TREE_TYPE (t), flags);
      if (TREE_CODE (t) == OFFSET_TYPE)	/* pmfs deal with this in d_t_p */
	{
	  pp_maybe_space (pp);
	  if (TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE)
	     pp_cxx_left_paren (pp);
	  dump_type (pp, TYPE_OFFSET_BASETYPE (t), flags);
	  pp_cxx_colon_colon (pp);
	}
      pp_cxx_star (pp);
      pp_cxx_cv_qualifier_seq (pp, t);
      pp->padding = pp_before;
      break;

      /* This can be reached without a pointer when dealing with
	 templates, e.g. std::is_function.  */
    case FUNCTION_TYPE:
      dump_type_prefix (pp, TREE_TYPE (t), flags);
      break;

    case METHOD_TYPE:
      dump_type_prefix (pp, TREE_TYPE (t), flags);
      pp_maybe_space (pp);
      pp_cxx_left_paren (pp);
      dump_aggr_type (pp, TYPE_METHOD_BASETYPE (t), flags);
      pp_cxx_colon_colon (pp);
      break;

    case ARRAY_TYPE:
      dump_type_prefix (pp, TREE_TYPE (t), flags);
      break;

    case ENUMERAL_TYPE:
    case IDENTIFIER_NODE:
    case INTEGER_TYPE:
    case BOOLEAN_TYPE:
    case REAL_TYPE:
    case RECORD_TYPE:
    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_TEMPLATE_PARM:
    case BOUND_TEMPLATE_TEMPLATE_PARM:
    case TREE_LIST:
    case TYPE_DECL:
    case TREE_VEC:
    case UNION_TYPE:
    case LANG_TYPE:
    case VOID_TYPE:
    case TYPENAME_TYPE:
    case COMPLEX_TYPE:
    case VECTOR_TYPE:
    case TYPEOF_TYPE:
    case UNDERLYING_TYPE:
    case DECLTYPE_TYPE:
    case TYPE_PACK_EXPANSION:
    case FIXED_POINT_TYPE:
    case NULLPTR_TYPE:
      dump_type (pp, t, flags);
      pp->padding = pp_before;
      break;

    default:
      pp_unsupported_tree (pp, t);
      /* fall through.  */
    case ERROR_MARK:
      pp_string (pp, M_("<typeprefixerror>"));
      break;
    }
}

/* Dump the suffix of type T, under control of FLAGS.  This is the part
   which appears after the identifier (or function parms).  */

static void
dump_type_suffix (cxx_pretty_printer *pp, tree t, int flags)
{
  if (TYPE_PTRMEMFUNC_P (t))
    t = TYPE_PTRMEMFUNC_FN_TYPE (t);

  switch (TREE_CODE (t))
    {
    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case OFFSET_TYPE:
      if (TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE
	  || TREE_CODE (TREE_TYPE (t)) == FUNCTION_TYPE)
	pp_cxx_right_paren (pp);
      dump_type_suffix (pp, TREE_TYPE (t), flags);
      break;

    case FUNCTION_TYPE:
    case METHOD_TYPE:
      {
	tree arg;
	if (TREE_CODE (t) == METHOD_TYPE)
	  /* Can only be reached through a pointer.  */
	  pp_cxx_right_paren (pp);
	arg = TYPE_ARG_TYPES (t);
	if (TREE_CODE (t) == METHOD_TYPE)
	  arg = TREE_CHAIN (arg);

	/* Function pointers don't have default args.  Not in standard C++,
	   anyway; they may in g++, but we'll just pretend otherwise.  */
	dump_parameters (pp, arg, flags & ~TFF_FUNCTION_DEFAULT_ARGUMENTS);

	pp->padding = pp_before;
	pp_cxx_cv_qualifiers (pp, type_memfn_quals (t));
	dump_ref_qualifier (pp, t, flags);
	dump_exception_spec (pp, TYPE_RAISES_EXCEPTIONS (t), flags);
	dump_type_suffix (pp, TREE_TYPE (t), flags);
	break;
      }

    case ARRAY_TYPE:
      pp_maybe_space (pp);
      pp_cxx_left_bracket (pp);
      if (TYPE_DOMAIN (t))
	{
	  tree dtype = TYPE_DOMAIN (t);
	  tree max = TYPE_MAX_VALUE (dtype);
	  if (integer_all_onesp (max))
	    pp_character (pp, '0');
	  else if (tree_fits_shwi_p (max))
	    pp_wide_integer (pp, tree_to_shwi (max) + 1);
	  else
	    {
	      STRIP_NOPS (max);
	      if (TREE_CODE (max) == SAVE_EXPR)
		max = TREE_OPERAND (max, 0);
	      if (TREE_CODE (max) == MINUS_EXPR
		  || TREE_CODE (max) == PLUS_EXPR)
		{
		  max = TREE_OPERAND (max, 0);
		  while (CONVERT_EXPR_P (max))
		    max = TREE_OPERAND (max, 0);
		}
	      else
		max = fold_build2_loc (input_location,
				       PLUS_EXPR, dtype, max,
				       build_int_cst (dtype, 1));
	      dump_expr (pp, max, flags & ~TFF_EXPR_IN_PARENS);
	    }
	}
      pp_cxx_right_bracket (pp);
      dump_type_suffix (pp, TREE_TYPE (t), flags);
      break;

    case ENUMERAL_TYPE:
    case IDENTIFIER_NODE:
    case INTEGER_TYPE:
    case BOOLEAN_TYPE:
    case REAL_TYPE:
    case RECORD_TYPE:
    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_TEMPLATE_PARM:
    case BOUND_TEMPLATE_TEMPLATE_PARM:
    case TREE_LIST:
    case TYPE_DECL:
    case TREE_VEC:
    case UNION_TYPE:
    case LANG_TYPE:
    case VOID_TYPE:
    case TYPENAME_TYPE:
    case COMPLEX_TYPE:
    case VECTOR_TYPE:
    case TYPEOF_TYPE:
    case UNDERLYING_TYPE:
    case DECLTYPE_TYPE:
    case TYPE_PACK_EXPANSION:
    case FIXED_POINT_TYPE:
    case NULLPTR_TYPE:
      break;

    default:
      pp_unsupported_tree (pp, t);
    case ERROR_MARK:
      /* Don't mark it here, we should have already done in
	 dump_type_prefix.  */
      break;
    }
}

static void
dump_global_iord (cxx_pretty_printer *pp, tree t)
{
  const char *p = NULL;

  if (DECL_GLOBAL_CTOR_P (t))
    p = M_("(static initializers for %s)");
  else if (DECL_GLOBAL_DTOR_P (t))
    p = M_("(static destructors for %s)");
  else
    gcc_unreachable ();

  pp_printf (pp, p, LOCATION_FILE (input_location));
}

static void
dump_simple_decl (cxx_pretty_printer *pp, tree t, tree type, int flags)
{
  if (flags & TFF_DECL_SPECIFIERS)
    {
      if (VAR_P (t)
	  && DECL_DECLARED_CONSTEXPR_P (t))
	pp_cxx_ws_string (pp, "constexpr");
      dump_type_prefix (pp, type, flags & ~TFF_UNQUALIFIED_NAME);
      pp_maybe_space (pp);
    }
  if (! (flags & TFF_UNQUALIFIED_NAME)
      && TREE_CODE (t) != PARM_DECL
      && (!DECL_INITIAL (t)
	  || TREE_CODE (DECL_INITIAL (t)) != TEMPLATE_PARM_INDEX))
    dump_scope (pp, CP_DECL_CONTEXT (t), flags);
  flags &= ~TFF_UNQUALIFIED_NAME;
  if ((flags & TFF_DECL_SPECIFIERS)
      && DECL_TEMPLATE_PARM_P (t) 
      && TEMPLATE_PARM_PARAMETER_PACK (DECL_INITIAL (t)))
    pp_string (pp, "...");
  if (DECL_NAME (t))
    {
      if (TREE_CODE (t) == FIELD_DECL && DECL_NORMAL_CAPTURE_P (t))
	{
	  pp_less (pp);
	  pp_string (pp, IDENTIFIER_POINTER (DECL_NAME (t)) + 2);
	  pp_string (pp, " capture>");
	}
      else
	dump_decl (pp, DECL_NAME (t), flags);
    }
  else
    pp_string (pp, M_("<anonymous>"));
  if (flags & TFF_DECL_SPECIFIERS)
    dump_type_suffix (pp, type, flags);
}

/* Dump a human readable string for the decl T under control of FLAGS.  */

static void
dump_decl (cxx_pretty_printer *pp, tree t, int flags)
{
  if (t == NULL_TREE)
    return;

  /* If doing Objective-C++, give Objective-C a chance to demangle
     Objective-C method names.  */
  if (c_dialect_objc ())
    {
      const char *demangled = objc_maybe_printable_name (t, flags);
      if (demangled)
	{
	  pp_string (pp, demangled);
	  return;
	}
    }

  switch (TREE_CODE (t))
    {
    case TYPE_DECL:
      /* Don't say 'typedef class A' */
      if (DECL_ARTIFICIAL (t) && !DECL_SELF_REFERENCE_P (t))
	{
	  if ((flags & TFF_DECL_SPECIFIERS)
	      && TREE_CODE (TREE_TYPE (t)) == TEMPLATE_TYPE_PARM)
	    {
	      /* Say `class T' not just `T'.  */
	      pp_cxx_ws_string (pp, "class");

	      /* Emit the `...' for a parameter pack.  */
	      if (TEMPLATE_TYPE_PARAMETER_PACK (TREE_TYPE (t)))
		pp_cxx_ws_string (pp, "...");
	    }

	  dump_type (pp, TREE_TYPE (t), flags);
	  break;
	}
      if (TYPE_DECL_ALIAS_P (t)
	  && (flags & TFF_DECL_SPECIFIERS
	      || flags & TFF_CLASS_KEY_OR_ENUM))
	{
	  pp_cxx_ws_string (pp, "using");
	  dump_decl (pp, DECL_NAME (t), flags);
	  pp_cxx_whitespace (pp);
	  pp_cxx_ws_string (pp, "=");
	  pp_cxx_whitespace (pp);
	  dump_type (pp, DECL_ORIGINAL_TYPE (t), flags);
	  break;
	}
      if ((flags & TFF_DECL_SPECIFIERS)
	  && !DECL_SELF_REFERENCE_P (t))
	pp_cxx_ws_string (pp, "typedef");
      dump_simple_decl (pp, t, DECL_ORIGINAL_TYPE (t)
			? DECL_ORIGINAL_TYPE (t) : TREE_TYPE (t),
			flags);
      break;

    case VAR_DECL:
      if (DECL_NAME (t) && VTABLE_NAME_P (DECL_NAME (t)))
	{
	  pp_string (pp, M_("vtable for "));
	  gcc_assert (TYPE_P (DECL_CONTEXT (t)));
	  dump_type (pp, DECL_CONTEXT (t), flags);
	  break;
	}
      /* Else fall through.  */
    case FIELD_DECL:
    case PARM_DECL:
      dump_simple_decl (pp, t, TREE_TYPE (t), flags);
      break;

    case RESULT_DECL:
      pp_string (pp, M_("<return value> "));
      dump_simple_decl (pp, t, TREE_TYPE (t), flags);
      break;

    case NAMESPACE_DECL:
      if (flags & TFF_DECL_SPECIFIERS)
	pp->declaration (t);
      else
	{
	  if (! (flags & TFF_UNQUALIFIED_NAME))
	    dump_scope (pp, CP_DECL_CONTEXT (t), flags);
	  flags &= ~TFF_UNQUALIFIED_NAME;
	  if (DECL_NAME (t) == NULL_TREE)
            {
              if (!(pp->flags & pp_c_flag_gnu_v3))
                pp_cxx_ws_string (pp, M_("{anonymous}"));
              else
                pp_cxx_ws_string (pp, M_("(anonymous namespace)"));
            }
	  else
	    pp_cxx_tree_identifier (pp, DECL_NAME (t));
	}
      break;

    case SCOPE_REF:
      dump_type (pp, TREE_OPERAND (t, 0), flags);
      pp_colon_colon (pp);
      dump_decl (pp, TREE_OPERAND (t, 1), TFF_UNQUALIFIED_NAME);
      break;

    case ARRAY_REF:
      dump_decl (pp, TREE_OPERAND (t, 0), flags);
      pp_cxx_left_bracket (pp);
      dump_decl (pp, TREE_OPERAND (t, 1), flags);
      pp_cxx_right_bracket (pp);
      break;

    case ARRAY_NOTATION_REF:
      dump_decl (pp, ARRAY_NOTATION_ARRAY (t), flags | TFF_EXPR_IN_PARENS);
      pp_cxx_left_bracket (pp);
      dump_decl (pp, ARRAY_NOTATION_START (t), flags | TFF_EXPR_IN_PARENS);
      pp_colon (pp);
      dump_decl (pp, ARRAY_NOTATION_LENGTH (t), flags | TFF_EXPR_IN_PARENS);
      pp_colon (pp);
      dump_decl (pp, ARRAY_NOTATION_STRIDE (t), flags | TFF_EXPR_IN_PARENS);
      pp_cxx_right_bracket (pp);
      break;

      /* So that we can do dump_decl on an aggr type.  */
    case RECORD_TYPE:
    case UNION_TYPE:
    case ENUMERAL_TYPE:
      dump_type (pp, t, flags);
      break;

    case BIT_NOT_EXPR:
      /* This is a pseudo destructor call which has not been folded into
	 a PSEUDO_DTOR_EXPR yet.  */
      pp_cxx_complement (pp);
      dump_type (pp, TREE_OPERAND (t, 0), flags);
      break;

    case TYPE_EXPR:
      gcc_unreachable ();
      break;

      /* These special cases are duplicated here so that other functions
	 can feed identifiers to error and get them demangled properly.  */
    case IDENTIFIER_NODE:
      if (IDENTIFIER_TYPENAME_P (t))
	{
	  pp_cxx_ws_string (pp, "operator");
	  /* Not exactly IDENTIFIER_TYPE_VALUE.  */
	  dump_type (pp, TREE_TYPE (t), flags);
	  break;
	}
      else
	pp_cxx_tree_identifier (pp, t);
      break;

    case OVERLOAD:
      if (OVL_CHAIN (t))
	{
	  t = OVL_CURRENT (t);
	  if (DECL_CLASS_SCOPE_P (t))
	    {
	      dump_type (pp, DECL_CONTEXT (t), flags);
	      pp_cxx_colon_colon (pp);
	    }
	  else if (!DECL_FILE_SCOPE_P (t))
	    {
	      dump_decl (pp, DECL_CONTEXT (t), flags);
	      pp_cxx_colon_colon (pp);
	    }
	  dump_decl (pp, DECL_NAME (t), flags);
	  break;
	}

      /* If there's only one function, just treat it like an ordinary
	 FUNCTION_DECL.  */
      t = OVL_CURRENT (t);
      /* Fall through.  */

    case FUNCTION_DECL:
      if (! DECL_LANG_SPECIFIC (t))
	pp_string (pp, M_("<built-in>"));
      else if (DECL_GLOBAL_CTOR_P (t) || DECL_GLOBAL_DTOR_P (t))
	dump_global_iord (pp, t);
      else
	dump_function_decl (pp, t, flags);
      break;

    case TEMPLATE_DECL:
      dump_template_decl (pp, t, flags);
      break;

    case TEMPLATE_ID_EXPR:
      {
	tree name = TREE_OPERAND (t, 0);
	tree args = TREE_OPERAND (t, 1);

	if (is_overloaded_fn (name))
	  name = DECL_NAME (get_first_fn (name));
	dump_decl (pp, name, flags);
	pp_cxx_begin_template_argument_list (pp);
	if (args == error_mark_node)
	  pp_string (pp, M_("<template arguments error>"));
	else if (args)
	  dump_template_argument_list (pp, args, flags);
      	pp_cxx_end_template_argument_list (pp);
      }
      break;

    case LABEL_DECL:
      pp_cxx_tree_identifier (pp, DECL_NAME (t));
      break;

    case CONST_DECL:
      if ((TREE_TYPE (t) != NULL_TREE && NEXT_CODE (t) == ENUMERAL_TYPE)
	  || (DECL_INITIAL (t) &&
	      TREE_CODE (DECL_INITIAL (t)) == TEMPLATE_PARM_INDEX))
	dump_simple_decl (pp, t, TREE_TYPE (t), flags);
      else if (DECL_NAME (t))
	dump_decl (pp, DECL_NAME (t), flags);
      else if (DECL_INITIAL (t))
	dump_expr (pp, DECL_INITIAL (t), flags | TFF_EXPR_IN_PARENS);
      else
	pp_string (pp, M_("<enumerator>"));
      break;

    case USING_DECL:
      pp_cxx_ws_string (pp, "using");
      dump_type (pp, USING_DECL_SCOPE (t), flags);
      pp_cxx_colon_colon (pp);
      dump_decl (pp, DECL_NAME (t), flags);
      break;

    case STATIC_ASSERT:
      pp->declaration (t);
      break;

    case BASELINK:
      dump_decl (pp, BASELINK_FUNCTIONS (t), flags);
      break;

    case NON_DEPENDENT_EXPR:
      dump_expr (pp, t, flags);
      break;

    case TEMPLATE_TYPE_PARM:
      if (flags & TFF_DECL_SPECIFIERS)
	pp->declaration (t);
      else
	pp->type_id (t);
      break;

    case UNBOUND_CLASS_TEMPLATE:
    case TYPE_PACK_EXPANSION:
    case TREE_BINFO:
      dump_type (pp, t, flags);
      break;

    default:
      pp_unsupported_tree (pp, t);
      /* Fall through to error.  */

    case ERROR_MARK:
      pp_string (pp, M_("<declaration error>"));
      break;
    }
}

/* Dump a template declaration T under control of FLAGS. This means the
   'template <...> leaders plus the 'class X' or 'void fn(...)' part.  */

static void
dump_template_decl (cxx_pretty_printer *pp, tree t, int flags)
{
  tree orig_parms = DECL_TEMPLATE_PARMS (t);
  tree parms;
  int i;

  if (flags & TFF_TEMPLATE_HEADER)
    {
      for (parms = orig_parms = nreverse (orig_parms);
	   parms;
	   parms = TREE_CHAIN (parms))
	{
	  tree inner_parms = INNERMOST_TEMPLATE_PARMS (parms);
	  int len = TREE_VEC_LENGTH (inner_parms);

	  pp_cxx_ws_string (pp, "template");
	  pp_cxx_begin_template_argument_list (pp);

	  /* If we've shown the template prefix, we'd better show the
	     parameters' and decl's type too.  */
	    flags |= TFF_DECL_SPECIFIERS;

	  for (i = 0; i < len; i++)
	    {
	      if (i)
		pp_separate_with_comma (pp);
	      dump_template_parameter (pp, TREE_VEC_ELT (inner_parms, i),
                                       flags);
	    }
	  pp_cxx_end_template_argument_list (pp);
	  pp_cxx_whitespace (pp);
	}
      nreverse(orig_parms);

      if (DECL_TEMPLATE_TEMPLATE_PARM_P (t))
	{
	  /* Say `template<arg> class TT' not just `template<arg> TT'.  */
	  pp_cxx_ws_string (pp, "class");

	  /* If this is a parameter pack, print the ellipsis.  */
	  if (TEMPLATE_TYPE_PARAMETER_PACK (TREE_TYPE (t)))
	    pp_cxx_ws_string (pp, "...");
	}
    }

  if (DECL_CLASS_TEMPLATE_P (t))
    dump_type (pp, TREE_TYPE (t),
	       ((flags & ~TFF_CLASS_KEY_OR_ENUM) | TFF_TEMPLATE_NAME
		| (flags & TFF_DECL_SPECIFIERS ? TFF_CLASS_KEY_OR_ENUM : 0)));
  else if (DECL_TEMPLATE_RESULT (t)
           && (VAR_P (DECL_TEMPLATE_RESULT (t))
	       /* Alias template.  */
	       || DECL_TYPE_TEMPLATE_P (t)))
    dump_decl (pp, DECL_TEMPLATE_RESULT (t), flags | TFF_TEMPLATE_NAME);
  else
    {
      gcc_assert (TREE_TYPE (t));
      switch (NEXT_CODE (t))
	{
	case METHOD_TYPE:
	case FUNCTION_TYPE:
	  dump_function_decl (pp, t, flags | TFF_TEMPLATE_NAME);
	  break;
	default:
	  /* This case can occur with some invalid code.  */
	  dump_type (pp, TREE_TYPE (t),
		     (flags & ~TFF_CLASS_KEY_OR_ENUM) | TFF_TEMPLATE_NAME
		     | (flags & TFF_DECL_SPECIFIERS
			? TFF_CLASS_KEY_OR_ENUM : 0));
	}
    }
}

/* find_typenames looks through the type of the function template T
   and returns a vec containing any typedefs, decltypes or TYPENAME_TYPEs
   it finds.  */

struct find_typenames_t
{
  struct pointer_set_t *p_set;
  vec<tree, va_gc> *typenames;
};

static tree
find_typenames_r (tree *tp, int *walk_subtrees, void *data)
{
  struct find_typenames_t *d = (struct find_typenames_t *)data;
  tree mv = NULL_TREE;

  if (TYPE_P (*tp) && is_typedef_decl (TYPE_NAME (*tp)))
    /* Add the type of the typedef without any additional cv-quals.  */
    mv = TREE_TYPE (TYPE_NAME (*tp));
  else if (TREE_CODE (*tp) == TYPENAME_TYPE
	   || TREE_CODE (*tp) == DECLTYPE_TYPE)
    /* Add the typename without any cv-qualifiers.  */
    mv = TYPE_MAIN_VARIANT (*tp);

  if (TREE_CODE (*tp) == TYPE_PACK_EXPANSION)
    {
      /* Don't mess with parameter packs since we don't remember
	 the pack expansion context for a particular typename.  */
      *walk_subtrees = false;
      return NULL_TREE;
    }

  if (mv && (mv == *tp || !pointer_set_insert (d->p_set, mv)))
    vec_safe_push (d->typenames, mv);

  /* Search into class template arguments, which cp_walk_subtrees
     doesn't do.  */
  if (CLASS_TYPE_P (*tp) && CLASSTYPE_TEMPLATE_INFO (*tp))
    cp_walk_tree (&CLASSTYPE_TI_ARGS (*tp), find_typenames_r,
		  data, d->p_set);

  return NULL_TREE;
}

static vec<tree, va_gc> *
find_typenames (tree t)
{
  struct find_typenames_t ft;
  ft.p_set = pointer_set_create ();
  ft.typenames = NULL;
  cp_walk_tree (&TREE_TYPE (DECL_TEMPLATE_RESULT (t)),
		find_typenames_r, &ft, ft.p_set);
  pointer_set_destroy (ft.p_set);
  return ft.typenames;
}

/* Output the "[with ...]" clause for a template instantiation T iff
   TEMPLATE_PARMS, TEMPLATE_ARGS and FLAGS are suitable.  T may be NULL if
   formatting a deduction/substitution diagnostic rather than an
   instantiation.  */

static void
dump_substitution (cxx_pretty_printer *pp,
                   tree t, tree template_parms, tree template_args,
                   int flags)
{
  if (template_parms != NULL_TREE && template_args != NULL_TREE
      && !(flags & TFF_NO_TEMPLATE_BINDINGS))
    {
      vec<tree, va_gc> *typenames = t ? find_typenames (t) : NULL;
      pp_cxx_whitespace (pp);
      pp_cxx_left_bracket (pp);
      pp->translate_string ("with");
      pp_cxx_whitespace (pp);
      dump_template_bindings (pp, template_parms, template_args, typenames);
      pp_cxx_right_bracket (pp);
    }
}

/* Dump the lambda function FN including its 'mutable' qualifier and any
   template bindings.  */

static void
dump_lambda_function (cxx_pretty_printer *pp,
		      tree fn, tree template_parms, tree template_args,
		      int flags)
{
  /* A lambda's signature is essentially its "type".  */
  dump_type (pp, DECL_CONTEXT (fn), flags);
  if (!(TYPE_QUALS (class_of_this_parm (TREE_TYPE (fn))) & TYPE_QUAL_CONST))
    {
      pp->padding = pp_before;
      pp_c_ws_string (pp, "mutable");
    }
  dump_substitution (pp, fn, template_parms, template_args, flags);
}

/* Pretty print a function decl. There are several ways we want to print a
   function declaration. The TFF_ bits in FLAGS tells us how to behave.
   As error can only apply the '#' flag once to give 0 and 1 for V, there
   is %D which doesn't print the throw specs, and %F which does.  */

static void
dump_function_decl (cxx_pretty_printer *pp, tree t, int flags)
{
  tree fntype;
  tree parmtypes;
  tree cname = NULL_TREE;
  tree template_args = NULL_TREE;
  tree template_parms = NULL_TREE;
  int show_return = flags & TFF_RETURN_TYPE || flags & TFF_DECL_SPECIFIERS;
  int do_outer_scope = ! (flags & TFF_UNQUALIFIED_NAME);
  tree exceptions;

  flags &= ~(TFF_UNQUALIFIED_NAME | TFF_TEMPLATE_NAME);
  if (TREE_CODE (t) == TEMPLATE_DECL)
    t = DECL_TEMPLATE_RESULT (t);

  /* Save the exceptions, in case t is a specialization and we are
     emitting an error about incompatible specifications.  */
  exceptions = TYPE_RAISES_EXCEPTIONS (TREE_TYPE (t));

  /* Pretty print template instantiations only.  */
  if (DECL_USE_TEMPLATE (t) && DECL_TEMPLATE_INFO (t)
      && flag_pretty_templates)
    {
      tree tmpl;

      template_args = DECL_TI_ARGS (t);
      tmpl = most_general_template (t);
      if (tmpl && TREE_CODE (tmpl) == TEMPLATE_DECL)
	{
	  template_parms = DECL_TEMPLATE_PARMS (tmpl);
	  t = tmpl;
	}
    }

  if (DECL_NAME (t) && LAMBDA_FUNCTION_P (t))
    return dump_lambda_function (pp, t, template_parms, template_args, flags);

  fntype = TREE_TYPE (t);
  parmtypes = FUNCTION_FIRST_USER_PARMTYPE (t);

  if (DECL_CLASS_SCOPE_P (t))
    cname = DECL_CONTEXT (t);
  /* This is for partially instantiated template methods.  */
  else if (TREE_CODE (fntype) == METHOD_TYPE)
    cname = TREE_TYPE (TREE_VALUE (parmtypes));

  if (flags & TFF_DECL_SPECIFIERS)
    {
      if (DECL_STATIC_FUNCTION_P (t))
	pp_cxx_ws_string (pp, "static");
      else if (DECL_VIRTUAL_P (t))
	pp_cxx_ws_string (pp, "virtual");

      if (DECL_DECLARED_CONSTEXPR_P (STRIP_TEMPLATE (t)))
	pp_cxx_ws_string (pp, "constexpr");
    }

  /* Print the return type?  */
  if (show_return)
    show_return = !DECL_CONV_FN_P (t)  && !DECL_CONSTRUCTOR_P (t)
		  && !DECL_DESTRUCTOR_P (t);
  if (show_return)
    {
      tree ret = fndecl_declared_return_type (t);
      dump_type_prefix (pp, ret, flags);
    }

  /* Print the function name.  */
  if (!do_outer_scope)
    /* Nothing.  */;
  else if (cname)
    {
      dump_type (pp, cname, flags);
      pp_cxx_colon_colon (pp);
    }
  else
    dump_scope (pp, CP_DECL_CONTEXT (t), flags);

  dump_function_name (pp, t, flags);

  if (!(flags & TFF_NO_FUNCTION_ARGUMENTS))
    {
      dump_parameters (pp, parmtypes, flags);

      if (TREE_CODE (fntype) == METHOD_TYPE)
	{
	  pp->padding = pp_before;
	  pp_cxx_cv_qualifier_seq (pp, class_of_this_parm (fntype));
	  dump_ref_qualifier (pp, fntype, flags);
	}

      if (flags & TFF_EXCEPTION_SPECIFICATION)
	{
	  pp->padding = pp_before;
	  dump_exception_spec (pp, exceptions, flags);
	}

      if (show_return)
	dump_type_suffix (pp, TREE_TYPE (fntype), flags);

      dump_substitution (pp, t, template_parms, template_args, flags);
    }
  else if (template_args)
    {
      bool need_comma = false;
      int i;
      pp_cxx_begin_template_argument_list (pp);
      template_args = INNERMOST_TEMPLATE_ARGS (template_args);
      for (i = 0; i < TREE_VEC_LENGTH (template_args); ++i)
	{
	  tree arg = TREE_VEC_ELT (template_args, i);
	  if (need_comma)
	    pp_separate_with_comma (pp);
	  if (ARGUMENT_PACK_P (arg))
	    pp_cxx_left_brace (pp);
	  dump_template_argument (pp, arg, TFF_PLAIN_IDENTIFIER);
	  if (ARGUMENT_PACK_P (arg))
	    pp_cxx_right_brace (pp);
	  need_comma = true;
	}
      pp_cxx_end_template_argument_list (pp);
    }
}

/* Print a parameter list. If this is for a member function, the
   member object ptr (and any other hidden args) should have
   already been removed.  */

static void
dump_parameters (cxx_pretty_printer *pp, tree parmtypes, int flags)
{
  int first = 1;
  flags &= ~TFF_SCOPE;
  pp_cxx_left_paren (pp);

  for (first = 1; parmtypes != void_list_node;
       parmtypes = TREE_CHAIN (parmtypes))
    {
      if (!first)
	pp_separate_with_comma (pp);
      first = 0;
      if (!parmtypes)
	{
	  pp_cxx_ws_string (pp, "...");
	  break;
	}

      dump_type (pp, TREE_VALUE (parmtypes), flags);

      if ((flags & TFF_FUNCTION_DEFAULT_ARGUMENTS) && TREE_PURPOSE (parmtypes))
	{
	  pp_cxx_whitespace (pp);
	  pp_equal (pp);
	  pp_cxx_whitespace (pp);
	  dump_expr (pp, TREE_PURPOSE (parmtypes), flags | TFF_EXPR_IN_PARENS);
	}
    }

  pp_cxx_right_paren (pp);
}

/* Print ref-qualifier of a FUNCTION_TYPE or METHOD_TYPE. FLAGS are ignored. */

static void
dump_ref_qualifier (cxx_pretty_printer *pp, tree t, int flags ATTRIBUTE_UNUSED)
{
  if (FUNCTION_REF_QUALIFIED (t))
    {
      pp->padding = pp_before;
      if (FUNCTION_RVALUE_QUALIFIED (t))
        pp_cxx_ws_string (pp, "&&");
      else
        pp_cxx_ws_string (pp, "&");
    }
}

/* Print an exception specification. T is the exception specification.  */

static void
dump_exception_spec (cxx_pretty_printer *pp, tree t, int flags)
{
  if (t && TREE_PURPOSE (t))
    {
      pp_cxx_ws_string (pp, "noexcept");
      if (!integer_onep (TREE_PURPOSE (t)))
	{
	  pp_cxx_whitespace (pp);
	  pp_cxx_left_paren (pp);
	  if (DEFERRED_NOEXCEPT_SPEC_P (t))
	    pp_cxx_ws_string (pp, "<uninstantiated>");
	  else
	    dump_expr (pp, TREE_PURPOSE (t), flags);
	  pp_cxx_right_paren (pp);
	}
    }
  else if (t)
    {
      pp_cxx_ws_string (pp, "throw");
      pp_cxx_whitespace (pp);
      pp_cxx_left_paren (pp);
      if (TREE_VALUE (t) != NULL_TREE)
	while (1)
	  {
	    dump_type (pp, TREE_VALUE (t), flags);
	    t = TREE_CHAIN (t);
	    if (!t)
	      break;
	    pp_separate_with_comma (pp);
	  }
      pp_cxx_right_paren (pp);
    }
}

/* Handle the function name for a FUNCTION_DECL node, grokking operators
   and destructors properly.  */

static void
dump_function_name (cxx_pretty_printer *pp, tree t, int flags)
{
  tree name = DECL_NAME (t);

  /* We can get here with a decl that was synthesized by language-
     independent machinery (e.g. coverage.c) in which case it won't
     have a lang_specific structure attached and DECL_CONSTRUCTOR_P
     will crash.  In this case it is safe just to print out the
     literal name.  */
  if (!DECL_LANG_SPECIFIC (t))
    {
      pp_cxx_tree_identifier (pp, name);
      return;
    }

  if (TREE_CODE (t) == TEMPLATE_DECL)
    t = DECL_TEMPLATE_RESULT (t);

  /* Don't let the user see __comp_ctor et al.  */
  if (DECL_CONSTRUCTOR_P (t)
      || DECL_DESTRUCTOR_P (t))
    {
      if (LAMBDA_TYPE_P (DECL_CONTEXT (t)))
	name = get_identifier ("<lambda>");
      else if (TYPE_ANONYMOUS_P (DECL_CONTEXT (t)))
	name = get_identifier ("<constructor>");
      else
	name = constructor_name (DECL_CONTEXT (t));
    }

  if (DECL_DESTRUCTOR_P (t))
    {
      pp_cxx_complement (pp);
      dump_decl (pp, name, TFF_PLAIN_IDENTIFIER);
    }
  else if (DECL_CONV_FN_P (t))
    {
      /* This cannot use the hack that the operator's return
	 type is stashed off of its name because it may be
	 used for error reporting.  In the case of conflicting
	 declarations, both will have the same name, yet
	 the types will be different, hence the TREE_TYPE field
	 of the first name will be clobbered by the second.  */
      pp_cxx_ws_string (pp, "operator");
      dump_type (pp, TREE_TYPE (TREE_TYPE (t)), flags);
    }
  else if (name && IDENTIFIER_OPNAME_P (name))
    pp_cxx_tree_identifier (pp, name);
  else if (name && UDLIT_OPER_P (name))
    pp_cxx_tree_identifier (pp, name);
  else
    dump_decl (pp, name, flags);

  if (DECL_TEMPLATE_INFO (t)
      && !DECL_FRIEND_PSEUDO_TEMPLATE_INSTANTIATION (t)
      && (TREE_CODE (DECL_TI_TEMPLATE (t)) != TEMPLATE_DECL
	  || PRIMARY_TEMPLATE_P (DECL_TI_TEMPLATE (t))))
    dump_template_parms (pp, DECL_TEMPLATE_INFO (t), !DECL_USE_TEMPLATE (t),
                         flags);
}

/* Dump the template parameters from the template info INFO under control of
   FLAGS. PRIMARY indicates whether this is a primary template decl, or
   specialization (partial or complete). For partial specializations we show
   the specialized parameter values. For a primary template we show no
   decoration.  */

static void
dump_template_parms (cxx_pretty_printer *pp, tree info,
                     int primary, int flags)
{
  tree args = info ? TI_ARGS (info) : NULL_TREE;

  if (primary && flags & TFF_TEMPLATE_NAME)
    return;
  flags &= ~(TFF_CLASS_KEY_OR_ENUM | TFF_TEMPLATE_NAME);
  pp_cxx_begin_template_argument_list (pp);

  /* Be careful only to print things when we have them, so as not
     to crash producing error messages.  */
  if (args && !primary)
    {
      int len, ix;
      len = get_non_default_template_args_count (args, flags);

      args = INNERMOST_TEMPLATE_ARGS (args);
      for (ix = 0; ix != len; ix++)
	{
	  tree arg = TREE_VEC_ELT (args, ix);

          /* Only print a comma if we know there is an argument coming. In
             the case of an empty template argument pack, no actual
             argument will be printed.  */
          if (ix
              && (!ARGUMENT_PACK_P (arg)
                  || TREE_VEC_LENGTH (ARGUMENT_PACK_ARGS (arg)) > 0))
            pp_separate_with_comma (pp);
          
          if (!arg)
            pp_string (pp, M_("<template parameter error>"));
          else
            dump_template_argument (pp, arg, flags);
        }
    }
  else if (primary)
    {
      tree tpl = TI_TEMPLATE (info);
      tree parms = DECL_TEMPLATE_PARMS (tpl);
      int len, ix;

      parms = TREE_CODE (parms) == TREE_LIST ? TREE_VALUE (parms) : NULL_TREE;
      len = parms ? TREE_VEC_LENGTH (parms) : 0;

      for (ix = 0; ix != len; ix++)
	{
	  tree parm;

          if (TREE_VEC_ELT (parms, ix) == error_mark_node)
            {
              pp_string (pp, M_("<template parameter error>"));
              continue;
            }

          parm = TREE_VALUE (TREE_VEC_ELT (parms, ix));

	  if (ix)
	    pp_separate_with_comma (pp);

	  dump_decl (pp, parm, flags & ~TFF_DECL_SPECIFIERS);
	}
    }
  pp_cxx_end_template_argument_list (pp);
}

/* Print out the arguments of CALL_EXPR T as a parenthesized list using
   flags FLAGS.  Skip over the first argument if SKIPFIRST is true.  */

static void
dump_call_expr_args (cxx_pretty_printer *pp, tree t, int flags, bool skipfirst)
{
  tree arg;
  call_expr_arg_iterator iter;
  
  pp_cxx_left_paren (pp);
  FOR_EACH_CALL_EXPR_ARG (arg, iter, t)
    {
      if (skipfirst)
	skipfirst = false;
      else
	{
	  dump_expr (pp, arg, flags | TFF_EXPR_IN_PARENS);
	  if (more_call_expr_args_p (&iter))
	    pp_separate_with_comma (pp);
	}
    }
  pp_cxx_right_paren (pp);
}

/* Print out the arguments of AGGR_INIT_EXPR T as a parenthesized list
   using flags FLAGS.  Skip over the first argument if SKIPFIRST is
   true.  */

static void
dump_aggr_init_expr_args (cxx_pretty_printer *pp, tree t, int flags,
                          bool skipfirst)
{
  tree arg;
  aggr_init_expr_arg_iterator iter;
  
  pp_cxx_left_paren (pp);
  FOR_EACH_AGGR_INIT_EXPR_ARG (arg, iter, t)
    {
      if (skipfirst)
	skipfirst = false;
      else
	{
	  dump_expr (pp, arg, flags | TFF_EXPR_IN_PARENS);
	  if (more_aggr_init_expr_args_p (&iter))
	    pp_separate_with_comma (pp);
	}
    }
  pp_cxx_right_paren (pp);
}

/* Print out a list of initializers (subr of dump_expr).  */

static void
dump_expr_list (cxx_pretty_printer *pp, tree l, int flags)
{
  while (l)
    {
      dump_expr (pp, TREE_VALUE (l), flags | TFF_EXPR_IN_PARENS);
      l = TREE_CHAIN (l);
      if (l)
	pp_separate_with_comma (pp);
    }
}

/* Print out a vector of initializers (subr of dump_expr).  */

static void
dump_expr_init_vec (cxx_pretty_printer *pp, vec<constructor_elt, va_gc> *v,
                    int flags)
{
  unsigned HOST_WIDE_INT idx;
  tree value;

  FOR_EACH_CONSTRUCTOR_VALUE (v, idx, value)
    {
      dump_expr (pp, value, flags | TFF_EXPR_IN_PARENS);
      if (idx != v->length () - 1)
	pp_separate_with_comma (pp);
    }
}


/* We've gotten an indirect REFERENCE (an OBJ_TYPE_REF) to a virtual
   function.  Resolve it to a close relative -- in the sense of static
   type -- variant being overridden.  That is close to what was written in
   the source code.  Subroutine of dump_expr.  */

static tree
resolve_virtual_fun_from_obj_type_ref (tree ref)
{
  tree obj_type = TREE_TYPE (OBJ_TYPE_REF_OBJECT (ref));
  HOST_WIDE_INT index = tree_to_uhwi (OBJ_TYPE_REF_TOKEN (ref));
  tree fun = BINFO_VIRTUALS (TYPE_BINFO (TREE_TYPE (obj_type)));
  while (index)
    {
      fun = TREE_CHAIN (fun);
      index -= (TARGET_VTABLE_USES_DESCRIPTORS
		? TARGET_VTABLE_USES_DESCRIPTORS : 1);
    }

  return BV_FN (fun);
}

/* Print out an expression E under control of FLAGS.  */

static void
dump_expr (cxx_pretty_printer *pp, tree t, int flags)
{
  tree op;

  if (t == 0)
    return;

  if (STATEMENT_CLASS_P (t))
    {
      pp_cxx_ws_string (pp, M_("<statement>"));
      return;
    }

  switch (TREE_CODE (t))
    {
    case VAR_DECL:
    case PARM_DECL:
    case FIELD_DECL:
    case CONST_DECL:
    case FUNCTION_DECL:
    case TEMPLATE_DECL:
    case NAMESPACE_DECL:
    case LABEL_DECL:
    case OVERLOAD:
    case TYPE_DECL:
    case IDENTIFIER_NODE:
      dump_decl (pp, t, ((flags & ~(TFF_DECL_SPECIFIERS|TFF_RETURN_TYPE
                                    |TFF_TEMPLATE_HEADER))
                         | TFF_NO_FUNCTION_ARGUMENTS));
      break;

    case SSA_NAME:
      if (SSA_NAME_VAR (t)
	  && !DECL_ARTIFICIAL (SSA_NAME_VAR (t)))
	dump_expr (pp, SSA_NAME_VAR (t), flags);
      else
	pp_cxx_ws_string (pp, M_("<unknown>"));
      break;

    case INTEGER_CST:
    case REAL_CST:
    case STRING_CST:
    case COMPLEX_CST:
      pp->constant (t);
      break;

    case USERDEF_LITERAL:
      pp_cxx_userdef_literal (pp, t);
      break;

    case THROW_EXPR:
      /* While waiting for caret diagnostics, avoid printing
	 __cxa_allocate_exception, __cxa_throw, and the like.  */
      pp_cxx_ws_string (pp, M_("<throw-expression>"));
      break;

    case PTRMEM_CST:
      pp_ampersand (pp);
      dump_type (pp, PTRMEM_CST_CLASS (t), flags);
      pp_cxx_colon_colon (pp);
      pp_cxx_tree_identifier (pp, DECL_NAME (PTRMEM_CST_MEMBER (t)));
      break;

    case COMPOUND_EXPR:
      pp_cxx_left_paren (pp);
      dump_expr (pp, TREE_OPERAND (t, 0), flags | TFF_EXPR_IN_PARENS);
      pp_separate_with_comma (pp);
      dump_expr (pp, TREE_OPERAND (t, 1), flags | TFF_EXPR_IN_PARENS);
      pp_cxx_right_paren (pp);
      break;

    case COND_EXPR:
      pp_cxx_left_paren (pp);
      dump_expr (pp, TREE_OPERAND (t, 0), flags | TFF_EXPR_IN_PARENS);
      pp_string (pp, " ? ");
      dump_expr (pp, TREE_OPERAND (t, 1), flags | TFF_EXPR_IN_PARENS);
      pp_string (pp, " : ");
      dump_expr (pp, TREE_OPERAND (t, 2), flags | TFF_EXPR_IN_PARENS);
      pp_cxx_right_paren (pp);
      break;

    case SAVE_EXPR:
      if (TREE_HAS_CONSTRUCTOR (t))
	{
	  pp_cxx_ws_string (pp, "new");
	  pp_cxx_whitespace (pp);
	  dump_type (pp, TREE_TYPE (TREE_TYPE (t)), flags);
	}
      else
	dump_expr (pp, TREE_OPERAND (t, 0), flags | TFF_EXPR_IN_PARENS);
      break;

    case AGGR_INIT_EXPR:
      {
	tree fn = NULL_TREE;

	if (TREE_CODE (AGGR_INIT_EXPR_FN (t)) == ADDR_EXPR)
	  fn = TREE_OPERAND (AGGR_INIT_EXPR_FN (t), 0);

	if (fn && TREE_CODE (fn) == FUNCTION_DECL)
	  {
	    if (DECL_CONSTRUCTOR_P (fn))
	      dump_type (pp, DECL_CONTEXT (fn), flags);
	    else
	      dump_decl (pp, fn, 0);
	  }
	else
	  dump_expr (pp, AGGR_INIT_EXPR_FN (t), 0);
      }
      dump_aggr_init_expr_args (pp, t, flags, true);
      break;

    case CALL_EXPR:
      {
	tree fn = CALL_EXPR_FN (t);
	bool skipfirst = false;

	if (TREE_CODE (fn) == ADDR_EXPR)
	  fn = TREE_OPERAND (fn, 0);

	/* Nobody is interested in seeing the guts of vcalls.  */
	if (TREE_CODE (fn) == OBJ_TYPE_REF)
	  fn = resolve_virtual_fun_from_obj_type_ref (fn);

	if (TREE_TYPE (fn) != NULL_TREE
	    && NEXT_CODE (fn) == METHOD_TYPE
	    && call_expr_nargs (t))
	  {
	    tree ob = CALL_EXPR_ARG (t, 0);
	    if (TREE_CODE (ob) == ADDR_EXPR)
	      {
		dump_expr (pp, TREE_OPERAND (ob, 0),
                           flags | TFF_EXPR_IN_PARENS);
		pp_cxx_dot (pp);
	      }
	    else if (TREE_CODE (ob) != PARM_DECL
		     || strcmp (IDENTIFIER_POINTER (DECL_NAME (ob)), "this"))
	      {
		dump_expr (pp, ob, flags | TFF_EXPR_IN_PARENS);
		pp_cxx_arrow (pp);
	      }
	    skipfirst = true;
	  }
	if (flag_sanitize & SANITIZE_UNDEFINED
	    && is_ubsan_builtin_p (fn))
	  {
	    pp_string (cxx_pp, M_("<ubsan routine call>"));
	    break;
	  }
	dump_expr (pp, fn, flags | TFF_EXPR_IN_PARENS);
	dump_call_expr_args (pp, t, flags, skipfirst);
      }
      break;

    case TARGET_EXPR:
      /* Note that this only works for G++ target exprs.  If somebody
	 builds a general TARGET_EXPR, there's no way to represent that
	 it initializes anything other that the parameter slot for the
	 default argument.  Note we may have cleared out the first
	 operand in expand_expr, so don't go killing ourselves.  */
      if (TREE_OPERAND (t, 1))
	dump_expr (pp, TREE_OPERAND (t, 1), flags | TFF_EXPR_IN_PARENS);
      break;

    case POINTER_PLUS_EXPR:
      dump_binary_op (pp, "+", t, flags);
      break;

    case INIT_EXPR:
    case MODIFY_EXPR:
      dump_binary_op (pp, assignment_operator_name_info[NOP_EXPR].name,
		      t, flags);
      break;

    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case EXACT_DIV_EXPR:
      dump_binary_op (pp, operator_name_info[TREE_CODE (t)].name, t, flags);
      break;

    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case RDIV_EXPR:
      dump_binary_op (pp, "/", t, flags);
      break;

    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
      dump_binary_op (pp, "%", t, flags);
      break;

    case COMPONENT_REF:
      {
	tree ob = TREE_OPERAND (t, 0);
	if (INDIRECT_REF_P (ob))
	  {
	    ob = TREE_OPERAND (ob, 0);
	    if (TREE_CODE (ob) != PARM_DECL
		|| (DECL_NAME (ob)
		    && strcmp (IDENTIFIER_POINTER (DECL_NAME (ob)), "this")))
	      {
		dump_expr (pp, ob, flags | TFF_EXPR_IN_PARENS);
		if (TREE_CODE (TREE_TYPE (ob)) == REFERENCE_TYPE)
		  pp_cxx_dot (pp);
		else
		  pp_cxx_arrow (pp);
	      }
	  }
	else
	  {
	    dump_expr (pp, ob, flags | TFF_EXPR_IN_PARENS);
	    pp_cxx_dot (pp);
	  }
	dump_expr (pp, TREE_OPERAND (t, 1), flags & ~TFF_EXPR_IN_PARENS);
      }
      break;

    case ARRAY_REF:
      dump_expr (pp, TREE_OPERAND (t, 0), flags | TFF_EXPR_IN_PARENS);
      pp_cxx_left_bracket (pp);
      dump_expr (pp, TREE_OPERAND (t, 1), flags | TFF_EXPR_IN_PARENS);
      pp_cxx_right_bracket (pp);
      break;

    case ARRAY_NOTATION_REF:
      dump_expr (pp, ARRAY_NOTATION_ARRAY (t), flags | TFF_EXPR_IN_PARENS);
      pp_cxx_left_bracket (pp);
      dump_expr (pp, ARRAY_NOTATION_START (t), flags | TFF_EXPR_IN_PARENS);
      pp_colon (pp);
      dump_expr (pp, ARRAY_NOTATION_LENGTH (t), flags | TFF_EXPR_IN_PARENS);
      pp_colon (pp);
      dump_expr (pp, ARRAY_NOTATION_STRIDE (t), flags | TFF_EXPR_IN_PARENS);
      pp_cxx_right_bracket (pp);
      break;

    case UNARY_PLUS_EXPR:
      dump_unary_op (pp, "+", t, flags);
      break;

    case ADDR_EXPR:
      if (TREE_CODE (TREE_OPERAND (t, 0)) == FUNCTION_DECL
	  || TREE_CODE (TREE_OPERAND (t, 0)) == STRING_CST
	  /* An ADDR_EXPR can have reference type.  In that case, we
	     shouldn't print the `&' doing so indicates to the user
	     that the expression has pointer type.  */
	  || (TREE_TYPE (t)
	      && TREE_CODE (TREE_TYPE (t)) == REFERENCE_TYPE))
	dump_expr (pp, TREE_OPERAND (t, 0), flags | TFF_EXPR_IN_PARENS);
      else if (TREE_CODE (TREE_OPERAND (t, 0)) == LABEL_DECL)
	dump_unary_op (pp, "&&", t, flags);
      else
	dump_unary_op (pp, "&", t, flags);
      break;

    case INDIRECT_REF:
      if (TREE_HAS_CONSTRUCTOR (t))
	{
	  t = TREE_OPERAND (t, 0);
	  gcc_assert (TREE_CODE (t) == CALL_EXPR);
	  dump_expr (pp, CALL_EXPR_FN (t), flags | TFF_EXPR_IN_PARENS);
	  dump_call_expr_args (pp, t, flags, true);
	}
      else
	{
	  if (TREE_OPERAND (t,0) != NULL_TREE
	      && TREE_TYPE (TREE_OPERAND (t, 0))
	      && NEXT_CODE (TREE_OPERAND (t, 0)) == REFERENCE_TYPE)
	    dump_expr (pp, TREE_OPERAND (t, 0), flags);
	  else
	    dump_unary_op (pp, "*", t, flags);
	}
      break;

    case MEM_REF:
      if (TREE_CODE (TREE_OPERAND (t, 0)) == ADDR_EXPR
	  && integer_zerop (TREE_OPERAND (t, 1)))
	dump_expr (pp, TREE_OPERAND (TREE_OPERAND (t, 0), 0), flags);
      else
	{
	  pp_cxx_star (pp);
	  if (!integer_zerop (TREE_OPERAND (t, 1)))
	    {
	      pp_cxx_left_paren (pp);
	      if (!integer_onep (TYPE_SIZE_UNIT
				 (TREE_TYPE (TREE_TYPE (TREE_OPERAND (t, 0))))))
		{
		  pp_cxx_left_paren (pp);
		  dump_type (pp, ptr_type_node, flags);
		  pp_cxx_right_paren (pp);
		}
	    }
	  dump_expr (pp, TREE_OPERAND (t, 0), flags);
	  if (!integer_zerop (TREE_OPERAND (t, 1)))
	    {
	      pp_cxx_ws_string (pp, "+");
	      dump_expr (pp, fold_convert (ssizetype, TREE_OPERAND (t, 1)),
                         flags);
	      pp_cxx_right_paren (pp);
	    }
	}
      break;

    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
      dump_unary_op (pp, operator_name_info [TREE_CODE (t)].name, t, flags);
      break;

    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
      pp_cxx_left_paren (pp);
      dump_expr (pp, TREE_OPERAND (t, 0), flags | TFF_EXPR_IN_PARENS);
      pp_cxx_ws_string (pp, operator_name_info[TREE_CODE (t)].name);
      pp_cxx_right_paren (pp);
      break;

    case NON_LVALUE_EXPR:
      /* FIXME: This is a KLUDGE workaround for a parsing problem.  There
	 should be another level of INDIRECT_REF so that I don't have to do
	 this.  */
      if (TREE_TYPE (t) != NULL_TREE && NEXT_CODE (t) == POINTER_TYPE)
	{
	  tree next = TREE_TYPE (TREE_TYPE (t));

	  while (TYPE_PTR_P (next))
	    next = TREE_TYPE (next);

	  if (TREE_CODE (next) == FUNCTION_TYPE)
	    {
	      if (flags & TFF_EXPR_IN_PARENS)
		pp_cxx_left_paren (pp);
	      pp_cxx_star (pp);
	      dump_expr (pp, TREE_OPERAND (t, 0), flags & ~TFF_EXPR_IN_PARENS);
	      if (flags & TFF_EXPR_IN_PARENS)
		pp_cxx_right_paren (pp);
	      break;
	    }
	  /* Else fall through.  */
	}
      dump_expr (pp, TREE_OPERAND (t, 0), flags | TFF_EXPR_IN_PARENS);
      break;

    CASE_CONVERT:
    case IMPLICIT_CONV_EXPR:
    case VIEW_CONVERT_EXPR:
      {
	tree op = TREE_OPERAND (t, 0);
	tree ttype = TREE_TYPE (t);
	tree optype = TREE_TYPE (op);

	if (TREE_CODE (ttype) != TREE_CODE (optype)
	    && POINTER_TYPE_P (ttype)
	    && POINTER_TYPE_P (optype)
	    && same_type_p (TREE_TYPE (optype),
			    TREE_TYPE (ttype)))
	  {
	    if (TREE_CODE (ttype) == REFERENCE_TYPE)
	      dump_unary_op (pp, "*", t, flags);
	    else
	      dump_unary_op (pp, "&", t, flags);
	  }
	else if (!same_type_p (TREE_TYPE (op), TREE_TYPE (t)))
	  {
	    /* It is a cast, but we cannot tell whether it is a
	       reinterpret or static cast. Use the C style notation.  */
	    if (flags & TFF_EXPR_IN_PARENS)
	      pp_cxx_left_paren (pp);
	    pp_cxx_left_paren (pp);
	    dump_type (pp, TREE_TYPE (t), flags);
	    pp_cxx_right_paren (pp);
	    dump_expr (pp, op, flags | TFF_EXPR_IN_PARENS);
	    if (flags & TFF_EXPR_IN_PARENS)
	      pp_cxx_right_paren (pp);
	  }
	else
	  dump_expr (pp, op, flags);
	break;
      }

    case CONSTRUCTOR:
      if (TREE_TYPE (t) && TYPE_PTRMEMFUNC_P (TREE_TYPE (t)))
	{
	  tree idx = build_ptrmemfunc_access_expr (t, pfn_identifier);

	  if (integer_zerop (idx))
	    {
	      /* A NULL pointer-to-member constant.  */
	      pp_cxx_left_paren (pp);
	      pp_cxx_left_paren (pp);
	      dump_type (pp, TREE_TYPE (t), flags);
	      pp_cxx_right_paren (pp);
	      pp_character (pp, '0');
	      pp_cxx_right_paren (pp);
	      break;
	    }
	  else if (tree_fits_shwi_p (idx))
	    {
	      tree virtuals;
	      unsigned HOST_WIDE_INT n;

	      t = TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (TREE_TYPE (t)));
	      t = TYPE_METHOD_BASETYPE (t);
	      virtuals = BINFO_VIRTUALS (TYPE_BINFO (TYPE_MAIN_VARIANT (t)));

	      n = tree_to_shwi (idx);

	      /* Map vtable index back one, to allow for the null pointer to
		 member.  */
	      --n;

	      while (n > 0 && virtuals)
		{
		  --n;
		  virtuals = TREE_CHAIN (virtuals);
		}
	      if (virtuals)
		{
		  dump_expr (pp, BV_FN (virtuals),
			     flags | TFF_EXPR_IN_PARENS);
		  break;
		}
	    }
	}
      if (TREE_TYPE (t) && LAMBDA_TYPE_P (TREE_TYPE (t)))
	pp_string (pp, "<lambda closure object>");
      if (TREE_TYPE (t) && EMPTY_CONSTRUCTOR_P (t))
	{
	  dump_type (pp, TREE_TYPE (t), 0);
	  pp_cxx_left_paren (pp);
	  pp_cxx_right_paren (pp);
	}
      else
	{
	  if (!BRACE_ENCLOSED_INITIALIZER_P (t))
	    dump_type (pp, TREE_TYPE (t), 0);
	  pp_cxx_left_brace (pp);
	  dump_expr_init_vec (pp, CONSTRUCTOR_ELTS (t), flags);
	  pp_cxx_right_brace (pp);
	}

      break;

    case OFFSET_REF:
      {
	tree ob = TREE_OPERAND (t, 0);
	if (is_dummy_object (ob))
	  {
	    t = TREE_OPERAND (t, 1);
	    if (TREE_CODE (t) == FUNCTION_DECL)
	      /* A::f */
	      dump_expr (pp, t, flags | TFF_EXPR_IN_PARENS);
	    else if (BASELINK_P (t))
	      dump_expr (pp, OVL_CURRENT (BASELINK_FUNCTIONS (t)),
			 flags | TFF_EXPR_IN_PARENS);
	    else
	      dump_decl (pp, t, flags);
	  }
	else
	  {
	    if (INDIRECT_REF_P (ob))
	      {
		dump_expr (pp, TREE_OPERAND (ob, 0), flags | TFF_EXPR_IN_PARENS);
		pp_cxx_arrow (pp);
		pp_cxx_star (pp);
	      }
	    else
	      {
		dump_expr (pp, ob, flags | TFF_EXPR_IN_PARENS);
		pp_cxx_dot (pp);
		pp_cxx_star (pp);
	      }
	    dump_expr (pp, TREE_OPERAND (t, 1), flags | TFF_EXPR_IN_PARENS);
	  }
	break;
      }

    case TEMPLATE_PARM_INDEX:
      dump_decl (pp, TEMPLATE_PARM_DECL (t), flags & ~TFF_DECL_SPECIFIERS);
      break;

    case CAST_EXPR:
      if (TREE_OPERAND (t, 0) == NULL_TREE
	  || TREE_CHAIN (TREE_OPERAND (t, 0)))
	{
	  dump_type (pp, TREE_TYPE (t), flags);
	  pp_cxx_left_paren (pp);
	  dump_expr_list (pp, TREE_OPERAND (t, 0), flags);
	  pp_cxx_right_paren (pp);
	}
      else
	{
	  pp_cxx_left_paren (pp);
	  dump_type (pp, TREE_TYPE (t), flags);
	  pp_cxx_right_paren (pp);
	  pp_cxx_left_paren (pp);
	  dump_expr_list (pp, TREE_OPERAND (t, 0), flags);
	  pp_cxx_right_paren (pp);
	}
      break;

    case STATIC_CAST_EXPR:
      pp_cxx_ws_string (pp, "static_cast");
      goto cast;
    case REINTERPRET_CAST_EXPR:
      pp_cxx_ws_string (pp, "reinterpret_cast");
      goto cast;
    case CONST_CAST_EXPR:
      pp_cxx_ws_string (pp, "const_cast");
      goto cast;
    case DYNAMIC_CAST_EXPR:
      pp_cxx_ws_string (pp, "dynamic_cast");
    cast:
      pp_cxx_begin_template_argument_list (pp);
      dump_type (pp, TREE_TYPE (t), flags);
      pp_cxx_end_template_argument_list (pp);
      pp_cxx_left_paren (pp);
      dump_expr (pp, TREE_OPERAND (t, 0), flags);
      pp_cxx_right_paren (pp);
      break;

    case ARROW_EXPR:
      dump_expr (pp, TREE_OPERAND (t, 0), flags);
      pp_cxx_arrow (pp);
      break;

    case SIZEOF_EXPR:
    case ALIGNOF_EXPR:
      if (TREE_CODE (t) == SIZEOF_EXPR)
	pp_cxx_ws_string (pp, "sizeof");
      else
	{
	  gcc_assert (TREE_CODE (t) == ALIGNOF_EXPR);
	  pp_cxx_ws_string (pp, "__alignof__");
	}
      op = TREE_OPERAND (t, 0);
      if (PACK_EXPANSION_P (op))
	{
	  pp_string (pp, "...");
	  op = PACK_EXPANSION_PATTERN (op);
	}
      pp_cxx_whitespace (pp);
      pp_cxx_left_paren (pp);
      if (TREE_CODE (t) == SIZEOF_EXPR && SIZEOF_EXPR_TYPE_P (t))
	dump_type (pp, TREE_TYPE (op), flags);
      else if (TYPE_P (TREE_OPERAND (t, 0)))
	dump_type (pp, op, flags);
      else
	dump_expr (pp, op, flags);
      pp_cxx_right_paren (pp);
      break;

    case AT_ENCODE_EXPR:
      pp_cxx_ws_string (pp, "@encode");
      pp_cxx_whitespace (pp);
      pp_cxx_left_paren (pp);
      dump_type (pp, TREE_OPERAND (t, 0), flags);
      pp_cxx_right_paren (pp);
      break;

    case NOEXCEPT_EXPR:
      pp_cxx_ws_string (pp, "noexcept");
      pp_cxx_whitespace (pp);
      pp_cxx_left_paren (pp);
      dump_expr (pp, TREE_OPERAND (t, 0), flags);
      pp_cxx_right_paren (pp);
      break;

    case REALPART_EXPR:
    case IMAGPART_EXPR:
      pp_cxx_ws_string (pp, operator_name_info[TREE_CODE (t)].name);
      pp_cxx_whitespace (pp);
      dump_expr (pp, TREE_OPERAND (t, 0), flags);
      break;

    case DEFAULT_ARG:
      pp_string (pp, M_("<unparsed>"));
      break;

    case TRY_CATCH_EXPR:
    case WITH_CLEANUP_EXPR:
    case CLEANUP_POINT_EXPR:
      dump_expr (pp, TREE_OPERAND (t, 0), flags);
      break;

    case PSEUDO_DTOR_EXPR:
      dump_expr (pp, TREE_OPERAND (t, 0), flags);
      pp_cxx_dot (pp);
      if (TREE_OPERAND (t, 1))
	{
	  dump_type (pp, TREE_OPERAND (t, 1), flags);
	  pp_cxx_colon_colon (pp);
	}
      pp_cxx_complement (pp);
      dump_type (pp, TREE_OPERAND (t, 2), flags);
      break;

    case TEMPLATE_ID_EXPR:
      dump_decl (pp, t, flags);
      break;

    case BIND_EXPR:
    case STMT_EXPR:
    case EXPR_STMT:
    case STATEMENT_LIST:
      /* We don't yet have a way of dumping statements in a
	 human-readable format.  */
      pp_string (pp, "({...})");
      break;

    case LOOP_EXPR:
      pp_string (pp, "while (1) { ");
      dump_expr (pp, TREE_OPERAND (t, 0), flags & ~TFF_EXPR_IN_PARENS);
      pp_cxx_right_brace (pp);
      break;

    case EXIT_EXPR:
      pp_string (pp, "if (");
      dump_expr (pp, TREE_OPERAND (t, 0), flags & ~TFF_EXPR_IN_PARENS);
      pp_string (pp, ") break; ");
      break;

    case BASELINK:
      dump_expr (pp, BASELINK_FUNCTIONS (t), flags & ~TFF_EXPR_IN_PARENS);
      break;

    case EMPTY_CLASS_EXPR:
      dump_type (pp, TREE_TYPE (t), flags);
      pp_cxx_left_paren (pp);
      pp_cxx_right_paren (pp);
      break;

    case NON_DEPENDENT_EXPR:
      dump_expr (pp, TREE_OPERAND (t, 0), flags);
      break;

    case ARGUMENT_PACK_SELECT:
      dump_template_argument (pp, ARGUMENT_PACK_SELECT_FROM_PACK (t), flags);
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
    case ENUMERAL_TYPE:
    case REAL_TYPE:
    case VOID_TYPE:
    case BOOLEAN_TYPE:
    case INTEGER_TYPE:
    case COMPLEX_TYPE:
    case VECTOR_TYPE:
      pp_type_specifier_seq (pp, t);
      break;

    case TYPENAME_TYPE:
      /* We get here when we want to print a dependent type as an
         id-expression, without any disambiguator decoration.  */
      pp->id_expression (t);
      break;

    case TEMPLATE_TYPE_PARM:
    case TEMPLATE_TEMPLATE_PARM:
    case BOUND_TEMPLATE_TEMPLATE_PARM:
      dump_type (pp, t, flags);
      break;

    case TRAIT_EXPR:
      pp_cxx_trait_expression (pp, t);
      break;

    case VA_ARG_EXPR:
      pp_cxx_va_arg_expression (pp, t);
      break;

    case OFFSETOF_EXPR:
      pp_cxx_offsetof_expression (pp, t);
      break;

    case SCOPE_REF:
      dump_decl (pp, t, flags);
      break;

    case EXPR_PACK_EXPANSION:
    case TYPEID_EXPR:
    case MEMBER_REF:
    case DOTSTAR_EXPR:
    case NEW_EXPR:
    case VEC_NEW_EXPR:
    case DELETE_EXPR:
    case VEC_DELETE_EXPR:
    case MODOP_EXPR:
    case ABS_EXPR:
    case CONJ_EXPR:
    case VECTOR_CST:
    case FIXED_CST:
    case UNORDERED_EXPR:
    case ORDERED_EXPR:
    case UNLT_EXPR:
    case UNLE_EXPR:
    case UNGT_EXPR:
    case UNGE_EXPR:
    case UNEQ_EXPR:
    case LTGT_EXPR:
    case COMPLEX_EXPR:
    case BIT_FIELD_REF:
    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:
      pp->expression (t);
      break;

    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case TRUTH_XOR_EXPR:
      if (flags & TFF_EXPR_IN_PARENS)
	pp_cxx_left_paren (pp);
      pp->expression (t);
      if (flags & TFF_EXPR_IN_PARENS)
	pp_cxx_right_paren (pp);
      break;

    case OBJ_TYPE_REF:
      dump_expr (pp, resolve_virtual_fun_from_obj_type_ref (t), flags);
      break;

    case LAMBDA_EXPR:
      pp_string (pp, M_("<lambda>"));
      break;

    case PAREN_EXPR:
      pp_cxx_left_paren (pp);
      dump_expr (pp, TREE_OPERAND (t, 0), flags | TFF_EXPR_IN_PARENS);
      pp_cxx_right_paren (pp);
      break;

      /*  This list is incomplete, but should suffice for now.
	  It is very important that `sorry' does not call
	  `report_error_function'.  That could cause an infinite loop.  */
    default:
      pp_unsupported_tree (pp, t);
      /* fall through to ERROR_MARK...  */
    case ERROR_MARK:
      pp_string (pp, M_("<expression error>"));
      break;
    }
}

static void
dump_binary_op (cxx_pretty_printer *pp, const char *opstring, tree t,
                int flags)
{
  pp_cxx_left_paren (pp);
  dump_expr (pp, TREE_OPERAND (t, 0), flags | TFF_EXPR_IN_PARENS);
  pp_cxx_whitespace (pp);
  if (opstring)
    pp_cxx_ws_string (pp, opstring);
  else
    pp_string (pp, M_("<unknown operator>"));
  pp_cxx_whitespace (pp);
  dump_expr (pp, TREE_OPERAND (t, 1), flags | TFF_EXPR_IN_PARENS);
  pp_cxx_right_paren (pp);
}

static void
dump_unary_op (cxx_pretty_printer *pp, const char *opstring, tree t, int flags)
{
  if (flags & TFF_EXPR_IN_PARENS)
    pp_cxx_left_paren (pp);
  pp_cxx_ws_string (pp, opstring);
  dump_expr (pp, TREE_OPERAND (t, 0), flags & ~TFF_EXPR_IN_PARENS);
  if (flags & TFF_EXPR_IN_PARENS)
    pp_cxx_right_paren (pp);
}

static void
reinit_cxx_pp (void)
{
  pp_clear_output_area (cxx_pp);
  cxx_pp->padding = pp_none;
  pp_indentation (cxx_pp) = 0;
  pp_needs_newline (cxx_pp) = false;
  cxx_pp->enclosing_scope = current_function_decl;
}

/* Same as pp_formatted_text, except the return string is a separate
   copy and has a GGC storage duration, e.g. an indefinite lifetime.  */

inline const char *
pp_ggc_formatted_text (pretty_printer *pp)
{
  return ggc_strdup (pp_formatted_text (pp));
}

/* Exported interface to stringifying types, exprs and decls under TFF_*
   control.  */

const char *
type_as_string (tree typ, int flags)
{
  reinit_cxx_pp ();
  pp_translate_identifiers (cxx_pp) = false;
  dump_type (cxx_pp, typ, flags);
  return pp_ggc_formatted_text (cxx_pp);
}

const char *
type_as_string_translate (tree typ, int flags)
{
  reinit_cxx_pp ();
  dump_type (cxx_pp, typ, flags);
  return pp_ggc_formatted_text (cxx_pp);
}

const char *
expr_as_string (tree decl, int flags)
{
  reinit_cxx_pp ();
  pp_translate_identifiers (cxx_pp) = false;
  dump_expr (cxx_pp, decl, flags);
  return pp_ggc_formatted_text (cxx_pp);
}

/* Wrap decl_as_string with options appropriate for dwarf.  */

const char *
decl_as_dwarf_string (tree decl, int flags)
{
  const char *name;
  /* Curiously, reinit_cxx_pp doesn't reset the flags field, so setting the flag
     here will be adequate to get the desired behaviour.  */
  cxx_pp->flags |= pp_c_flag_gnu_v3;
  name = decl_as_string (decl, flags);
  /* Subsequent calls to the pretty printer shouldn't use this style.  */
  cxx_pp->flags &= ~pp_c_flag_gnu_v3;
  return name;
}

const char *
decl_as_string (tree decl, int flags)
{
  reinit_cxx_pp ();
  pp_translate_identifiers (cxx_pp) = false;
  dump_decl (cxx_pp, decl, flags);
  return pp_ggc_formatted_text (cxx_pp);
}

const char *
decl_as_string_translate (tree decl, int flags)
{
  reinit_cxx_pp ();
  dump_decl (cxx_pp, decl, flags);
  return pp_ggc_formatted_text (cxx_pp);
}

/* Wrap lang_decl_name with options appropriate for dwarf.  */

const char *
lang_decl_dwarf_name (tree decl, int v, bool translate)
{
  const char *name;
  /* Curiously, reinit_cxx_pp doesn't reset the flags field, so setting the flag
     here will be adequate to get the desired behaviour.  */
  cxx_pp->flags |= pp_c_flag_gnu_v3;
  name = lang_decl_name (decl, v, translate);
  /* Subsequent calls to the pretty printer shouldn't use this style.  */
  cxx_pp->flags &= ~pp_c_flag_gnu_v3;
  return name;
}

/* Generate the three forms of printable names for cxx_printable_name.  */

const char *
lang_decl_name (tree decl, int v, bool translate)
{
  if (v >= 2)
    return (translate
	    ? decl_as_string_translate (decl, TFF_DECL_SPECIFIERS)
	    : decl_as_string (decl, TFF_DECL_SPECIFIERS));

  reinit_cxx_pp ();
  pp_translate_identifiers (cxx_pp) = translate;
  if (v == 1
      && (DECL_CLASS_SCOPE_P (decl)
	  || (DECL_NAMESPACE_SCOPE_P (decl)
	      && CP_DECL_CONTEXT (decl) != global_namespace)))
    {
      dump_type (cxx_pp, CP_DECL_CONTEXT (decl), TFF_PLAIN_IDENTIFIER);
      pp_cxx_colon_colon (cxx_pp);
    }

  if (TREE_CODE (decl) == FUNCTION_DECL)
    dump_function_name (cxx_pp, decl, TFF_PLAIN_IDENTIFIER);
  else if ((DECL_NAME (decl) == NULL_TREE)
           && TREE_CODE (decl) == NAMESPACE_DECL)
    dump_decl (cxx_pp, decl, TFF_PLAIN_IDENTIFIER | TFF_UNQUALIFIED_NAME);
  else
    dump_decl (cxx_pp, DECL_NAME (decl), TFF_PLAIN_IDENTIFIER);

  return pp_ggc_formatted_text (cxx_pp);
}

/* Return the location of a tree passed to %+ formats.  */

location_t
location_of (tree t)
{
  if (TYPE_P (t))
    {
      t = TYPE_MAIN_DECL (t);
      if (t == NULL_TREE)
	return input_location;
    }
  else if (TREE_CODE (t) == OVERLOAD)
    t = OVL_FUNCTION (t);

  if (DECL_P (t))
    return DECL_SOURCE_LOCATION (t);
  return EXPR_LOC_OR_LOC (t, input_location);
}

/* Now the interfaces from error et al to dump_type et al. Each takes an
   on/off VERBOSE flag and supply the appropriate TFF_ flags to a dump_
   function.  */

static const char *
decl_to_string (tree decl, int verbose)
{
  int flags = 0;

  if (TREE_CODE (decl) == TYPE_DECL || TREE_CODE (decl) == RECORD_TYPE
      || TREE_CODE (decl) == UNION_TYPE || TREE_CODE (decl) == ENUMERAL_TYPE)
    flags = TFF_CLASS_KEY_OR_ENUM;
  if (verbose)
    flags |= TFF_DECL_SPECIFIERS;
  else if (TREE_CODE (decl) == FUNCTION_DECL)
    flags |= TFF_DECL_SPECIFIERS | TFF_RETURN_TYPE;
  flags |= TFF_TEMPLATE_HEADER;

  reinit_cxx_pp ();
  dump_decl (cxx_pp, decl, flags);
  return pp_ggc_formatted_text (cxx_pp);
}

static const char *
expr_to_string (tree decl)
{
  reinit_cxx_pp ();
  dump_expr (cxx_pp, decl, 0);
  return pp_ggc_formatted_text (cxx_pp);
}

static const char *
fndecl_to_string (tree fndecl, int verbose)
{
  int flags;

  flags = TFF_EXCEPTION_SPECIFICATION | TFF_DECL_SPECIFIERS
    | TFF_TEMPLATE_HEADER;
  if (verbose)
    flags |= TFF_FUNCTION_DEFAULT_ARGUMENTS;
  reinit_cxx_pp ();
  dump_decl (cxx_pp, fndecl, flags);
  return pp_ggc_formatted_text (cxx_pp);
}


static const char *
code_to_string (enum tree_code c)
{
  return get_tree_code_name (c);
}

const char *
language_to_string (enum languages c)
{
  switch (c)
    {
    case lang_c:
      return "C";

    case lang_cplusplus:
      return "C++";

    case lang_java:
      return "Java";

    default:
      gcc_unreachable ();
    }
  return NULL;
}

/* Return the proper printed version of a parameter to a C++ function.  */

static const char *
parm_to_string (int p)
{
  reinit_cxx_pp ();
  if (p < 0)
    pp_string (cxx_pp, "'this'");
  else
    pp_decimal_int (cxx_pp, p + 1);
  return pp_ggc_formatted_text (cxx_pp);
}

static const char *
op_to_string (enum tree_code p)
{
  tree id = operator_name_info[p].identifier;
  return id ? IDENTIFIER_POINTER (id) : M_("<unknown>");
}

static const char *
type_to_string (tree typ, int verbose)
{
  int flags = 0;
  if (verbose)
    flags |= TFF_CLASS_KEY_OR_ENUM;
  flags |= TFF_TEMPLATE_HEADER;

  reinit_cxx_pp ();
  dump_type (cxx_pp, typ, flags);
  /* If we're printing a type that involves typedefs, also print the
     stripped version.  But sometimes the stripped version looks
     exactly the same, so we don't want it after all.  To avoid printing
     it in that case, we play ugly obstack games.  */
  if (typ && TYPE_P (typ) && typ != TYPE_CANONICAL (typ)
      && !uses_template_parms (typ))
    {
      int aka_start; char *p;
      struct obstack *ob = pp_buffer (cxx_pp)->obstack;
      /* Remember the end of the initial dump.  */
      int len = obstack_object_size (ob);
      tree aka = strip_typedefs (typ);
      pp_string (cxx_pp, " {aka");
      pp_cxx_whitespace (cxx_pp);
      /* And remember the start of the aka dump.  */
      aka_start = obstack_object_size (ob);
      dump_type (cxx_pp, aka, flags);
      pp_right_brace (cxx_pp);
      p = (char*)obstack_base (ob);
      /* If they are identical, cut off the aka with a NUL.  */
      if (memcmp (p, p+aka_start, len) == 0)
	p[len] = '\0';
    }
  return pp_ggc_formatted_text (cxx_pp);
}

static const char *
assop_to_string (enum tree_code p)
{
  tree id = assignment_operator_name_info[(int) p].identifier;
  return id ? IDENTIFIER_POINTER (id) : M_("{unknown}");
}

static const char *
args_to_string (tree p, int verbose)
{
  int flags = 0;
  if (verbose)
    flags |= TFF_CLASS_KEY_OR_ENUM;

  if (p == NULL_TREE)
    return "";

  if (TYPE_P (TREE_VALUE (p)))
    return type_as_string_translate (p, flags);

  reinit_cxx_pp ();
  for (; p; p = TREE_CHAIN (p))
    {
      if (TREE_VALUE (p) == null_node)
	pp_cxx_ws_string (cxx_pp, "NULL");
      else
	dump_type (cxx_pp, error_type (TREE_VALUE (p)), flags);
      if (TREE_CHAIN (p))
	pp_separate_with_comma (cxx_pp);
    }
  return pp_ggc_formatted_text (cxx_pp);
}

/* Pretty-print a deduction substitution (from deduction_tsubst_fntype).  P
   is a TREE_LIST with purpose the TEMPLATE_DECL, value the template
   arguments.  */

static const char *
subst_to_string (tree p)
{
  tree decl = TREE_PURPOSE (p);
  tree targs = TREE_VALUE (p);
  tree tparms = DECL_TEMPLATE_PARMS (decl);
  int flags = (TFF_DECL_SPECIFIERS|TFF_TEMPLATE_HEADER
	       |TFF_NO_TEMPLATE_BINDINGS);

  if (p == NULL_TREE)
    return "";

  reinit_cxx_pp ();
  dump_template_decl (cxx_pp, TREE_PURPOSE (p), flags);
  dump_substitution (cxx_pp, NULL, tparms, targs, /*flags=*/0);
  return pp_ggc_formatted_text (cxx_pp);
}

static const char *
cv_to_string (tree p, int v)
{
  reinit_cxx_pp ();
  cxx_pp->padding = v ? pp_before : pp_none;
  pp_cxx_cv_qualifier_seq (cxx_pp, p);
  return pp_ggc_formatted_text (cxx_pp);
}

static const char *
eh_spec_to_string (tree p, int /*v*/)
{
  int flags = 0;
  reinit_cxx_pp ();
  dump_exception_spec (cxx_pp, p, flags);
  return pp_ggc_formatted_text (cxx_pp);
}

/* Langhook for print_error_function.  */
void
cxx_print_error_function (diagnostic_context *context, const char *file,
			  diagnostic_info *diagnostic)
{
  lhd_print_error_function (context, file, diagnostic);
  pp_set_prefix (context->printer, file);
  maybe_print_instantiation_context (context);
}

static void
cp_diagnostic_starter (diagnostic_context *context,
		       diagnostic_info *diagnostic)
{
  diagnostic_report_current_module (context, diagnostic->location);
  cp_print_error_function (context, diagnostic);
  maybe_print_instantiation_context (context);
  maybe_print_constexpr_context (context);
  pp_set_prefix (context->printer, diagnostic_build_prefix (context,
								 diagnostic));
}

static void
cp_diagnostic_finalizer (diagnostic_context *context,
			 diagnostic_info *diagnostic)
{
  virt_loc_aware_diagnostic_finalizer (context, diagnostic);
  pp_destroy_prefix (context->printer);
}

/* Print current function onto BUFFER, in the process of reporting
   a diagnostic message.  Called from cp_diagnostic_starter.  */
static void
cp_print_error_function (diagnostic_context *context,
			 diagnostic_info *diagnostic)
{
  /* If we are in an instantiation context, current_function_decl is likely
     to be wrong, so just rely on print_instantiation_full_context.  */
  if (current_instantiation ())
    return;
  if (diagnostic_last_function_changed (context, diagnostic))
    {
      const char *old_prefix = context->printer->prefix;
      const char *file = LOCATION_FILE (diagnostic->location);
      tree abstract_origin = diagnostic_abstract_origin (diagnostic);
      char *new_prefix = (file && abstract_origin == NULL)
			 ? file_name_as_prefix (context, file) : NULL;

      pp_set_prefix (context->printer, new_prefix);

      if (current_function_decl == NULL)
	pp_string (context->printer, _("At global scope:"));
      else
	{
	  tree fndecl, ao;

	  if (abstract_origin)
	    {
	      ao = BLOCK_ABSTRACT_ORIGIN (abstract_origin);
	      while (TREE_CODE (ao) == BLOCK
		     && BLOCK_ABSTRACT_ORIGIN (ao)
		     && BLOCK_ABSTRACT_ORIGIN (ao) != ao)
		ao = BLOCK_ABSTRACT_ORIGIN (ao);
	      gcc_assert (TREE_CODE (ao) == FUNCTION_DECL);
	      fndecl = ao;
	    }
	  else
	    fndecl = current_function_decl;

	  pp_printf (context->printer, function_category (fndecl),
		     cxx_printable_name_translate (fndecl, 2));

	  while (abstract_origin)
	    {
	      location_t *locus;
	      tree block = abstract_origin;

	      locus = &BLOCK_SOURCE_LOCATION (block);
	      fndecl = NULL;
	      block = BLOCK_SUPERCONTEXT (block);
	      while (block && TREE_CODE (block) == BLOCK
		     && BLOCK_ABSTRACT_ORIGIN (block))
		{
		  ao = BLOCK_ABSTRACT_ORIGIN (block);

		  while (TREE_CODE (ao) == BLOCK
			 && BLOCK_ABSTRACT_ORIGIN (ao)
			 && BLOCK_ABSTRACT_ORIGIN (ao) != ao)
		    ao = BLOCK_ABSTRACT_ORIGIN (ao);

		  if (TREE_CODE (ao) == FUNCTION_DECL)
		    {
		      fndecl = ao;
		      break;
		    }
		  else if (TREE_CODE (ao) != BLOCK)
		    break;

		  block = BLOCK_SUPERCONTEXT (block);
		}
	      if (fndecl)
		abstract_origin = block;
	      else
		{
		  while (block && TREE_CODE (block) == BLOCK)
		    block = BLOCK_SUPERCONTEXT (block);

		  if (block && TREE_CODE (block) == FUNCTION_DECL)
		    fndecl = block;
		  abstract_origin = NULL;
		}
	      if (fndecl)
		{
		  expanded_location s = expand_location (*locus);
		  pp_character (context->printer, ',');
		  pp_newline (context->printer);
		  if (s.file != NULL)
		    {
		      if (context->show_column && s.column != 0)
			pp_printf (context->printer,
				   _("    inlined from %qs at %r%s:%d:%d%R"),
				   cxx_printable_name_translate (fndecl, 2),
				   "locus", s.file, s.line, s.column);
		      else
			pp_printf (context->printer,
				   _("    inlined from %qs at %r%s:%d%R"),
				   cxx_printable_name_translate (fndecl, 2),
				   "locus", s.file, s.line);

		    }
		  else
		    pp_printf (context->printer, _("    inlined from %qs"),
			       cxx_printable_name_translate (fndecl, 2));
		}
	    }
	  pp_character (context->printer, ':');
	}
      pp_newline (context->printer);

      diagnostic_set_last_function (context, diagnostic);
      pp_destroy_prefix (context->printer);
      context->printer->prefix = old_prefix;
    }
}

/* Returns a description of FUNCTION using standard terminology.  The
   result is a format string of the form "In CATEGORY %qs".  */
static const char *
function_category (tree fn)
{
  /* We can get called from the middle-end for diagnostics of function
     clones.  Make sure we have language specific information before
     dereferencing it.  */
  if (DECL_LANG_SPECIFIC (STRIP_TEMPLATE (fn))
      && DECL_FUNCTION_MEMBER_P (fn))
    {
      if (DECL_STATIC_FUNCTION_P (fn))
	return _("In static member function %qs");
      else if (DECL_COPY_CONSTRUCTOR_P (fn))
	return _("In copy constructor %qs");
      else if (DECL_CONSTRUCTOR_P (fn))
	return _("In constructor %qs");
      else if (DECL_DESTRUCTOR_P (fn))
	return _("In destructor %qs");
      else if (LAMBDA_FUNCTION_P (fn))
	return _("In lambda function");
      else
	return _("In member function %qs");
    }
  else
    return _("In function %qs");
}

/* Report the full context of a current template instantiation,
   onto BUFFER.  */
static void
print_instantiation_full_context (diagnostic_context *context)
{
  struct tinst_level *p = current_instantiation ();
  location_t location = input_location;

  if (p)
    {
      pp_verbatim (context->printer,
		   TREE_CODE (p->decl) == TREE_LIST
		   ? _("%s: In substitution of %qS:\n")
		   : _("%s: In instantiation of %q#D:\n"),
		   LOCATION_FILE (location),
		   p->decl);

      location = p->locus;
      p = p->next;
    }

  print_instantiation_partial_context (context, p, location);
}

/* Helper function of print_instantiation_partial_context() that
   prints a single line of instantiation context.  */

static void
print_instantiation_partial_context_line (diagnostic_context *context,
					  const struct tinst_level *t,
					  location_t loc, bool recursive_p)
{
  if (loc == UNKNOWN_LOCATION)
    return;

  expanded_location xloc = expand_location (loc);

  if (context->show_column)
    pp_verbatim (context->printer, _("%r%s:%d:%d:%R   "),
		 "locus", xloc.file, xloc.line, xloc.column);
  else
    pp_verbatim (context->printer, _("%r%s:%d:%R   "),
		 "locus", xloc.file, xloc.line);

  if (t != NULL)
    {
      if (TREE_CODE (t->decl) == TREE_LIST)
	pp_verbatim (context->printer,
		     recursive_p
		     ? _("recursively required by substitution of %qS\n")
		     : _("required by substitution of %qS\n"),
		     t->decl);
      else
	pp_verbatim (context->printer,
		     recursive_p
		     ? _("recursively required from %q#D\n")
		     : _("required from %q#D\n"),
		     t->decl);
    }
  else
    {
      pp_verbatim (context->printer,
		   recursive_p
		   ? _("recursively required from here")
		   : _("required from here"));
    }
}

/* Same as print_instantiation_full_context but less verbose.  */

static void
print_instantiation_partial_context (diagnostic_context *context,
				     struct tinst_level *t0, location_t loc)
{
  struct tinst_level *t;
  int n_total = 0;
  int n;
  location_t prev_loc = loc;

  for (t = t0; t != NULL; t = t->next)
    if (prev_loc != t->locus)
      {
	prev_loc = t->locus;
	n_total++;
      }

  t = t0;

  if (template_backtrace_limit
      && n_total > template_backtrace_limit) 
    {
      int skip = n_total - template_backtrace_limit;
      int head = template_backtrace_limit / 2;

      /* Avoid skipping just 1.  If so, skip 2.  */
      if (skip == 1)
       {
         skip = 2;
         head = (template_backtrace_limit - 1) / 2;
       }
     
      for (n = 0; n < head; n++)
	{
	  gcc_assert (t != NULL);
	  if (loc != t->locus)
	    print_instantiation_partial_context_line (context, t, loc,
						      /*recursive_p=*/false);
	  loc = t->locus;
	  t = t->next;
	}
      if (t != NULL && skip > 0)
	{
	  expanded_location xloc;
	  xloc = expand_location (loc);
	  if (context->show_column)
	    pp_verbatim (context->printer,
			 _("%r%s:%d:%d:%R   [ skipping %d instantiation "
			   "contexts, use -ftemplate-backtrace-limit=0 to "
			   "disable ]\n"),
			 "locus", xloc.file, xloc.line, xloc.column, skip);
	  else
	    pp_verbatim (context->printer,
			 _("%r%s:%d:%R   [ skipping %d instantiation "
			   "contexts, use -ftemplate-backtrace-limit=0 to "
			   "disable ]\n"),
			 "locus", xloc.file, xloc.line, skip);
	  
	  do {
	    loc = t->locus;
	    t = t->next;
	  } while (t != NULL && --skip > 0);
	}
    }
  
  while (t != NULL)
    {
      while (t->next != NULL && t->locus == t->next->locus)
	{
	  loc = t->locus;
	  t = t->next;
	}
      print_instantiation_partial_context_line (context, t, loc,
						t->locus == loc);
      loc = t->locus;
      t = t->next;
    }
  print_instantiation_partial_context_line (context, NULL, loc,
					    /*recursive_p=*/false);
  pp_newline (context->printer);
}

/* Called from cp_thing to print the template context for an error.  */
static void
maybe_print_instantiation_context (diagnostic_context *context)
{
  if (!problematic_instantiation_changed () || current_instantiation () == 0)
    return;

  record_last_problematic_instantiation ();
  print_instantiation_full_context (context);
}

/* Report the bare minimum context of a template instantiation.  */
void
print_instantiation_context (void)
{
  print_instantiation_partial_context
    (global_dc, current_instantiation (), input_location);
  pp_newline (global_dc->printer);
  diagnostic_flush_buffer (global_dc);
}

/* Report what constexpr call(s) we're trying to expand, if any.  */

void
maybe_print_constexpr_context (diagnostic_context *context)
{
  vec<tree> call_stack = cx_error_context ();
  unsigned ix;
  tree t;

  FOR_EACH_VEC_ELT (call_stack, ix, t)
    {
      expanded_location xloc = expand_location (EXPR_LOCATION (t));
      const char *s = expr_as_string (t, 0);
      if (context->show_column)
	pp_verbatim (context->printer,
		     _("%r%s:%d:%d:%R   in constexpr expansion of %qs"),
		     "locus", xloc.file, xloc.line, xloc.column, s);
      else
	pp_verbatim (context->printer,
		     _("%r%s:%d:%R   in constexpr expansion of %qs"),
		     "locus", xloc.file, xloc.line, s);
      pp_newline (context->printer);
    }
}

/* Called from output_format -- during diagnostic message processing --
   to handle C++ specific format specifier with the following meanings:
   %A   function argument-list.
   %C	tree code.
   %D   declaration.
   %E   expression.
   %F   function declaration.
   %L	language as used in extern "lang".
   %O	binary operator.
   %P   function parameter whose position is indicated by an integer.
   %Q	assignment operator.
   %S   substitution (template + args)
   %T   type.
   %V   cv-qualifier.
   %X   exception-specification.  */
static bool
cp_printer (pretty_printer *pp, text_info *text, const char *spec,
	    int precision, bool wide, bool set_locus, bool verbose)
{
  const char *result;
  tree t = NULL;
#define next_tree    (t = va_arg (*text->args_ptr, tree))
#define next_tcode   ((enum tree_code) va_arg (*text->args_ptr, int))
#define next_lang    ((enum languages) va_arg (*text->args_ptr, int))
#define next_int     va_arg (*text->args_ptr, int)

  if (precision != 0 || wide)
    return false;

  if (text->locus == NULL)
    set_locus = false;

  switch (*spec)
    {
    case 'A': result = args_to_string (next_tree, verbose);	break;
    case 'C': result = code_to_string (next_tcode);		break;
    case 'D':
      {
	tree temp = next_tree;
	if (VAR_P (temp)
	    && DECL_HAS_DEBUG_EXPR_P (temp))
	  {
	    temp = DECL_DEBUG_EXPR (temp);
	    if (!DECL_P (temp))
	      {
		result = expr_to_string (temp);
		break;
	      }
	  }
	result = decl_to_string (temp, verbose);
      }
      break;
    case 'E': result = expr_to_string (next_tree);		break;
    case 'F': result = fndecl_to_string (next_tree, verbose);	break;
    case 'L': result = language_to_string (next_lang);		break;
    case 'O': result = op_to_string (next_tcode);		break;
    case 'P': result = parm_to_string (next_int);		break;
    case 'Q': result = assop_to_string (next_tcode);		break;
    case 'S': result = subst_to_string (next_tree);		break;
    case 'T': result = type_to_string (next_tree, verbose);	break;
    case 'V': result = cv_to_string (next_tree, verbose);	break;
    case 'X': result = eh_spec_to_string (next_tree, verbose);  break;

    case 'K':
      percent_K_format (text);
      return true;

    default:
      return false;
    }

  pp_string (pp, result);
  if (set_locus && t != NULL)
    *text->locus = location_of (t);
  return true;
#undef next_tree
#undef next_tcode
#undef next_lang
#undef next_int
}

/* Warn about the use of C++0x features when appropriate.  */
void
maybe_warn_cpp0x (cpp0x_warn_str str)
{
  if ((cxx_dialect == cxx98) && !in_system_header_at (input_location))
    /* We really want to suppress this warning in system headers,
       because libstdc++ uses variadic templates even when we aren't
       in C++0x mode. */
    switch (str)
      {
      case CPP0X_INITIALIZER_LISTS:
	pedwarn (input_location, 0, 
		 "extended initializer lists "
		 "only available with -std=c++11 or -std=gnu++11");
	break;
      case CPP0X_EXPLICIT_CONVERSION:
	pedwarn (input_location, 0,
		 "explicit conversion operators "
		 "only available with -std=c++11 or -std=gnu++11");
	break;
      case CPP0X_VARIADIC_TEMPLATES:
	pedwarn (input_location, 0,
		 "variadic templates "
		 "only available with -std=c++11 or -std=gnu++11");
	break;
      case CPP0X_LAMBDA_EXPR:
	pedwarn (input_location, 0,
		 "lambda expressions "
		  "only available with -std=c++11 or -std=gnu++11");
	break;
      case CPP0X_AUTO:
	pedwarn (input_location, 0,
		 "C++11 auto only available with -std=c++11 or -std=gnu++11");
	break;
      case CPP0X_SCOPED_ENUMS:
	pedwarn (input_location, 0,
		 "scoped enums only available with -std=c++11 or -std=gnu++11");
	break;
      case CPP0X_DEFAULTED_DELETED:
	pedwarn (input_location, 0,
		 "defaulted and deleted functions "
		 "only available with -std=c++11 or -std=gnu++11");
	break;
      case CPP0X_INLINE_NAMESPACES:
	pedwarn (input_location, OPT_Wpedantic,
		 "inline namespaces "
		 "only available with -std=c++11 or -std=gnu++11");
	break;
      case CPP0X_OVERRIDE_CONTROLS:
	pedwarn (input_location, 0,
		 "override controls (override/final) "
		 "only available with -std=c++11 or -std=gnu++11");
        break;
      case CPP0X_NSDMI:
	pedwarn (input_location, 0,
		 "non-static data member initializers "
		 "only available with -std=c++11 or -std=gnu++11");
        break;
      case CPP0X_USER_DEFINED_LITERALS:
	pedwarn (input_location, 0,
		 "user-defined literals "
		 "only available with -std=c++11 or -std=gnu++11");
	break;
      case CPP0X_DELEGATING_CTORS:
	pedwarn (input_location, 0,
		 "delegating constructors "
		 "only available with -std=c++11 or -std=gnu++11");
        break;
      case CPP0X_INHERITING_CTORS:
	pedwarn (input_location, 0,
		 "inheriting constructors "
		 "only available with -std=c++11 or -std=gnu++11");
        break;
      case CPP0X_ATTRIBUTES:
	pedwarn (input_location, 0,
		 "c++11 attributes "
		 "only available with -std=c++11 or -std=gnu++11");
	break;
      case CPP0X_REF_QUALIFIER:
	pedwarn (input_location, 0,
		 "ref-qualifiers "
		 "only available with -std=c++11 or -std=gnu++11");
	break;
      default:
	gcc_unreachable ();
      }
}

/* Warn about the use of variadic templates when appropriate.  */
void
maybe_warn_variadic_templates (void)
{
  maybe_warn_cpp0x (CPP0X_VARIADIC_TEMPLATES);
}


/* Issue an ISO C++98 pedantic warning at LOCATION, conditional on
   option OPT with text GMSGID.  Use this function to report
   diagnostics for constructs that are invalid C++98, but valid
   C++0x.  */
bool
pedwarn_cxx98 (location_t location, int opt, const char *gmsgid, ...)
{
  diagnostic_info diagnostic;
  va_list ap;
  bool ret;

  va_start (ap, gmsgid);
  diagnostic_set_info (&diagnostic, gmsgid, &ap, location,
		       (cxx_dialect == cxx98) ? DK_PEDWARN : DK_WARNING);
  diagnostic.option_index = opt;
  ret = report_diagnostic (&diagnostic);
  va_end (ap);
  return ret;
}

/* Issue a diagnostic that NAME cannot be found in SCOPE.  DECL is what
   we found when we tried to do the lookup.  LOCATION is the location of
   the NAME identifier.  */

void
qualified_name_lookup_error (tree scope, tree name,
			     tree decl, location_t location)
{
  if (scope == error_mark_node)
    ; /* We already complained.  */
  else if (TYPE_P (scope))
    {
      if (!COMPLETE_TYPE_P (scope))
	error_at (location, "incomplete type %qT used in nested name specifier",
		  scope);
      else if (TREE_CODE (decl) == TREE_LIST)
	{
	  error_at (location, "reference to %<%T::%D%> is ambiguous",
		    scope, name);
	  print_candidates (decl);
	}
      else
	error_at (location, "%qD is not a member of %qT", name, scope);
    }
  else if (scope != global_namespace)
    {
      error_at (location, "%qD is not a member of %qD", name, scope);
      suggest_alternatives_for (location, name);
    }
  else
    {
      error_at (location, "%<::%D%> has not been declared", name);
      suggest_alternatives_for (location, name);
    }
}
