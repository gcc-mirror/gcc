/* Call-backs for C++ error reporting.
   This code is non-reentrant.
   Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 2002
   Free Software Foundation, Inc.
   This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "tree.h"
#include "cp-tree.h"
#include "real.h"
#include "obstack.h"
#include "toplev.h"
#include "flags.h"
#include "diagnostic.h"

enum pad { none, before, after };

#define sorry_for_unsupported_tree(T)                                      \
   sorry ("`%s' not supported by %s", tree_code_name[(int) TREE_CODE (T)], \
             __FUNCTION__)

#define print_scope_operator(BUFFER)  output_add_string ((BUFFER), "::")
#define print_left_paren(BUFFER)      output_add_character ((BUFFER), '(')
#define print_right_paren(BUFFER)     output_add_character ((BUFFER), ')')
#define print_left_bracket(BUFFER)    output_add_character ((BUFFER), '[')
#define print_right_bracket(BUFFER)   output_add_character ((BUFFER), ']')
#define print_template_argument_list_start(BUFFER) \
   print_non_consecutive_character ((BUFFER), '<')
#define print_template_argument_list_end(BUFFER)  \
   print_non_consecutive_character ((BUFFER), '>')
#define print_whitespace(BUFFER, TFI)        \
   do {                                      \
     output_add_space (BUFFER);              \
     put_whitespace (TFI) = none;            \
   } while (0)
#define print_tree_identifier(BUFFER, TID) \
   output_add_string ((BUFFER), IDENTIFIER_POINTER (TID))
#define print_identifier(BUFFER, ID) output_add_string ((BUFFER), (ID))
#define separate_with_comma(BUFFER) output_add_string ((BUFFER), ", ")

/* The global buffer where we dump everything.  It is there only for
   transitional purpose.  It is expected, in the near future, to be
   completely removed.  */
static output_buffer scratch_buffer_rec;
static output_buffer *scratch_buffer = &scratch_buffer_rec;

# define NEXT_CODE(T) (TREE_CODE (TREE_TYPE (T)))

#define reinit_global_formatting_buffer() \
   output_clear_message_text (scratch_buffer)

static const char *args_to_string		PARAMS ((tree, int));
static const char *assop_to_string		PARAMS ((enum tree_code, int));
static const char *code_to_string		PARAMS ((enum tree_code, int));
static const char *cv_to_string			PARAMS ((tree, int));
static const char *decl_to_string		PARAMS ((tree, int));
static const char *expr_to_string		PARAMS ((tree, int));
static const char *fndecl_to_string		PARAMS ((tree, int));
static const char *op_to_string			PARAMS ((enum tree_code, int));
static const char *parm_to_string		PARAMS ((int, int));
static const char *type_to_string		PARAMS ((tree, int));

static void dump_type PARAMS ((tree, int));
static void dump_typename PARAMS ((tree, int));
static void dump_simple_decl PARAMS ((tree, tree, int));
static void dump_decl PARAMS ((tree, int));
static void dump_template_decl PARAMS ((tree, int));
static void dump_function_decl PARAMS ((tree, int));
static void dump_expr PARAMS ((tree, int));
static void dump_unary_op PARAMS ((const char *, tree, int));
static void dump_binary_op PARAMS ((const char *, tree, int));
static void dump_aggr_type PARAMS ((tree, int));
static enum pad dump_type_prefix PARAMS ((tree, int));
static void dump_type_suffix PARAMS ((tree, int));
static void dump_function_name PARAMS ((tree, int));
static void dump_expr_list PARAMS ((tree, int));
static void dump_global_iord PARAMS ((tree));
static enum pad dump_qualifiers PARAMS ((tree, enum pad));
static void dump_char PARAMS ((int));
static void dump_parameters PARAMS ((tree, int));
static void dump_exception_spec PARAMS ((tree, int));
static const char *class_key_or_enum PARAMS ((tree));
static void dump_template_argument PARAMS ((tree, int));
static void dump_template_argument_list PARAMS ((tree, int));
static void dump_template_parameter PARAMS ((tree, int));
static void dump_template_bindings PARAMS ((tree, tree));
static void dump_scope PARAMS ((tree, int));
static void dump_template_parms PARAMS ((tree, int, int));

static const char *function_category PARAMS ((tree));
static void lang_print_error_function PARAMS ((diagnostic_context *,
                                               const char *));
static void maybe_print_instantiation_context PARAMS ((output_buffer *));
static void print_instantiation_full_context PARAMS ((output_buffer *));
static void print_instantiation_partial_context PARAMS ((output_buffer *, tree,
                                                         const char *, int));
static void cp_diagnostic_starter PARAMS ((output_buffer *,
                                           diagnostic_context *));
static void cp_diagnostic_finalizer PARAMS ((output_buffer *,
                                             diagnostic_context *));
static void cp_print_error_function PARAMS ((output_buffer *,
                                             diagnostic_context *));

static int cp_printer PARAMS ((output_buffer *));
static void print_non_consecutive_character PARAMS ((output_buffer *, int));
static void print_integer PARAMS ((output_buffer *, HOST_WIDE_INT));
static tree locate_error PARAMS ((const char *, va_list));

void
init_error ()
{
  print_error_function = lang_print_error_function;
  diagnostic_starter (global_dc) = cp_diagnostic_starter;
  diagnostic_finalizer (global_dc) = cp_diagnostic_finalizer;
  diagnostic_format_decoder (global_dc) = cp_printer;

  init_output_buffer (scratch_buffer, /* prefix */NULL, /* line-width */0);
}

/* Dump a scope, if deemed necessary.  */

static void
dump_scope (scope, flags)
     tree scope;
     int flags;
{
  int f = ~TFF_RETURN_TYPE & (flags & (TFF_SCOPE | TFF_CHASE_TYPEDEF));

  if (scope == NULL_TREE)
    return;

  if (TREE_CODE (scope) == NAMESPACE_DECL)
    {
      if (scope != global_namespace)
        {
          dump_decl (scope, f);
          print_scope_operator (scratch_buffer);
        }
    }
  else if (AGGREGATE_TYPE_P (scope))
    {
      dump_type (scope, f);
      print_scope_operator (scratch_buffer);
    }
  else if ((flags & TFF_SCOPE) && TREE_CODE (scope) == FUNCTION_DECL)
    {
      dump_function_decl (scope, f);
      print_scope_operator (scratch_buffer);
    }
}

/* Dump type qualifiers, providing padding as requested. Return an
   indication of whether we dumped something.  */

static enum pad
dump_qualifiers (t, p)
     tree t;
     enum pad p;
{
  static const int masks[] =
    {TYPE_QUAL_CONST, TYPE_QUAL_VOLATILE, TYPE_QUAL_RESTRICT};
  static const char *const names[] =
    {"const", "volatile", "__restrict"};
  int ix;
  int quals = TYPE_QUALS (t);
  int do_after = p == after;

  if (quals)
    {
      for (ix = 0; ix != 3; ix++)
        if (masks[ix] & quals)
          {
            if (p == before)
              output_add_space (scratch_buffer);
            p = before;
            print_identifier (scratch_buffer, names[ix]);
          }
      if (do_after)
        output_add_space (scratch_buffer);
    }
  else
    p = none;
  return p;
}

/* This must be large enough to hold any printed integer or floating-point
   value.  */
static char digit_buffer[128];

/* Dump the template ARGument under control of FLAGS.  */

static void
dump_template_argument (arg, flags)
     tree arg;
     int flags;
{
  if (TYPE_P (arg) || TREE_CODE (arg) == TEMPLATE_DECL)
    dump_type (arg, flags & ~TFF_CLASS_KEY_OR_ENUM);
  else
    dump_expr (arg, (flags | TFF_EXPR_IN_PARENS) & ~TFF_CLASS_KEY_OR_ENUM);
}

/* Dump a template-argument-list ARGS (always a TREE_VEC) under control
   of FLAGS.  */

static void
dump_template_argument_list (args, flags)
     tree args;
     int flags;
{
  int n = TREE_VEC_LENGTH (args);
  int need_comma = 0;
  int i;

  for (i = 0; i< n; ++i)
    {
      if (need_comma)
        separate_with_comma (scratch_buffer);
      dump_template_argument (TREE_VEC_ELT (args, i), flags);
      need_comma = 1;
    }
}

/* Dump a template parameter PARM (a TREE_LIST) under control of FLAGS.  */

static void
dump_template_parameter (parm, flags)
     tree parm;
     int flags;
{
  tree p = TREE_VALUE (parm);
  tree a = TREE_PURPOSE (parm);

  if (TREE_CODE (p) == TYPE_DECL)
    {
      if (flags & TFF_DECL_SPECIFIERS)
        {
          print_identifier (scratch_buffer, "class");
          if (DECL_NAME (p))
            {
              output_add_space (scratch_buffer);
              print_tree_identifier (scratch_buffer, DECL_NAME (p));
            }
        }
      else if (DECL_NAME (p))
        print_tree_identifier (scratch_buffer, DECL_NAME (p));
      else
        print_identifier (scratch_buffer, "{template default argument error}");
    }
  else
    dump_decl (p, flags | TFF_DECL_SPECIFIERS);

  if ((flags & TFF_FUNCTION_DEFAULT_ARGUMENTS) && a != NULL_TREE)
    {
      output_add_string (scratch_buffer, " = ");
      if (TREE_CODE (p) == TYPE_DECL || TREE_CODE (p) == TEMPLATE_DECL)
        dump_type (a, flags & ~TFF_CHASE_TYPEDEF);
      else
        dump_expr (a, flags | TFF_EXPR_IN_PARENS);
    }
}

/* Dump, under control of FLAGS, a template-parameter-list binding.
   PARMS is a TREE_LIST of TREE_VEC of TREE_LIST and ARGS is a
   TREE_VEC.  */

static void
dump_template_bindings (parms, args)
     tree parms, args;
{
  int need_comma = 0;

  while (parms)
    {
      tree p = TREE_VALUE (parms);
      int lvl = TMPL_PARMS_DEPTH (parms);
      int arg_idx = 0;
      int i;

      for (i = 0; i < TREE_VEC_LENGTH (p); ++i)
	{
	  tree arg = NULL_TREE;

	  /* Don't crash if we had an invalid argument list.  */
	  if (TMPL_ARGS_DEPTH (args) >= lvl)
	    {
	      tree lvl_args = TMPL_ARGS_LEVEL (args, lvl);
	      if (NUM_TMPL_ARGS (lvl_args) > arg_idx)
		arg = TREE_VEC_ELT (lvl_args, arg_idx);
	    }

	  if (need_comma)
	    separate_with_comma (scratch_buffer);
	  dump_template_parameter (TREE_VEC_ELT (p, i), TFF_PLAIN_IDENTIFIER);
	  output_add_string (scratch_buffer, " = ");
	  if (arg)
	    dump_template_argument (arg, TFF_PLAIN_IDENTIFIER);
	  else
	    print_identifier (scratch_buffer, "<missing>");

	  ++arg_idx;
	  need_comma = 1;
	}

      parms = TREE_CHAIN (parms);
    }
}

/* Dump into the obstack a human-readable equivalent of TYPE.  FLAGS
   controls the format.  */

static void
dump_type (t, flags)
     tree t;
     int flags;
{
  if (t == NULL_TREE)
    return;

  if (TYPE_PTRMEMFUNC_P (t))
    goto offset_type;

  switch (TREE_CODE (t))
    {
    case UNKNOWN_TYPE:
      print_identifier (scratch_buffer, "<unknown type>");
      break;

    case TREE_LIST:
      /* A list of function parms.  */
      dump_parameters (t, flags);
      break;

    case IDENTIFIER_NODE:
      print_tree_identifier (scratch_buffer, t);
      break;

    case TREE_VEC:
      dump_type (BINFO_TYPE (t), flags);
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
    case ENUMERAL_TYPE:
      dump_aggr_type (t, flags);
      break;

    case TYPE_DECL:
      if (flags & TFF_CHASE_TYPEDEF)
        {
          dump_type (DECL_ORIGINAL_TYPE (t)
                     ? DECL_ORIGINAL_TYPE (t) : TREE_TYPE (t), flags);
          break;
        }
      /* else fallthrough */

    case TEMPLATE_DECL:
    case NAMESPACE_DECL:
      dump_decl (t, flags & ~TFF_DECL_SPECIFIERS);
      break;

    case COMPLEX_TYPE:
      output_add_string (scratch_buffer, "__complex__ ");
      dump_type (TREE_TYPE (t), flags);
      break;

    case VECTOR_TYPE:
      output_add_string (scratch_buffer, "vector ");
      {
	/* The subtype of a VECTOR_TYPE is something like intQI_type_node,
	   which has no name and is not very useful for diagnostics.  So
	   look up the equivalent C type and print its name.  */
	tree elt = TREE_TYPE (t);
	elt = type_for_mode (TYPE_MODE (elt), TREE_UNSIGNED (elt));
	dump_type (elt, flags);
      }
      break;

    case INTEGER_TYPE:
      if (!TREE_UNSIGNED (TYPE_MAIN_VARIANT (t)) && TREE_UNSIGNED (t))
	output_add_string (scratch_buffer, "unsigned ");
      else if (TREE_UNSIGNED (TYPE_MAIN_VARIANT (t)) && !TREE_UNSIGNED (t))
	output_add_string (scratch_buffer, "signed ");

      /* fall through.  */
    case REAL_TYPE:
    case VOID_TYPE:
    case BOOLEAN_TYPE:
      {
	tree type;
	dump_qualifiers (t, after);
	type = flags & TFF_CHASE_TYPEDEF ? TYPE_MAIN_VARIANT (t) : t;
	if (TYPE_NAME (type) && TYPE_IDENTIFIER (type))
	  print_tree_identifier (scratch_buffer, TYPE_IDENTIFIER (type));
	else
	  /* Types like intQI_type_node and friends have no names.
	     These don't come up in user error messages, but it's nice
	     to be able to print them from the debugger.  */
	  print_identifier (scratch_buffer, "<anonymous>");
      }
      break;

    case TEMPLATE_TEMPLATE_PARM:
      /* For parameters inside template signature. */
      if (TYPE_IDENTIFIER (t))
	print_tree_identifier (scratch_buffer, TYPE_IDENTIFIER (t));
      else
	print_identifier
          (scratch_buffer, "<anonymous template template parameter>");
      break;

    case BOUND_TEMPLATE_TEMPLATE_PARM:
      {
	tree args = TYPE_TI_ARGS (t);
	print_tree_identifier (scratch_buffer, TYPE_IDENTIFIER (t));
	print_template_argument_list_start (scratch_buffer);
        dump_template_argument_list (args, flags);
	print_template_argument_list_end (scratch_buffer);
      }
      break;

    case TEMPLATE_TYPE_PARM:
      dump_qualifiers (t, after);
      if (TYPE_IDENTIFIER (t))
	print_tree_identifier (scratch_buffer, TYPE_IDENTIFIER (t));
      else
	print_identifier
          (scratch_buffer, "<anonymous template type parameter>");
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
      dump_type_prefix (t, flags);
      dump_type_suffix (t, flags);
      break;
    }
    case TYPENAME_TYPE:
      if (IMPLICIT_TYPENAME_P (t))
        output_add_string (scratch_buffer, "typename ");
      dump_typename (t, flags);
      break;

    case UNBOUND_CLASS_TEMPLATE:
      dump_type (TYPE_CONTEXT (t), flags);
      print_scope_operator (scratch_buffer);
      print_identifier (scratch_buffer, "template ");
      dump_type (DECL_NAME (TYPE_NAME (t)), flags);
      break;

    case TYPEOF_TYPE:
      output_add_string (scratch_buffer, "__typeof (");
      dump_expr (TYPE_FIELDS (t), flags & ~TFF_EXPR_IN_PARENS);
      print_right_paren (scratch_buffer);
      break;

    default:
      sorry_for_unsupported_tree (t);
      /* Fall through to error. */

    case ERROR_MARK:
      print_identifier (scratch_buffer, "<type error>");
      break;
    }
}

/* Dump a TYPENAME_TYPE. We need to notice when the context is itself
   a TYPENAME_TYPE.  */

static void
dump_typename (t, flags)
     tree t;
     int flags;
{
  tree ctx = TYPE_CONTEXT (t);

  if (TREE_CODE (ctx) == TYPENAME_TYPE)
    dump_typename (ctx, flags);
  else
    dump_type (ctx, flags & ~TFF_CLASS_KEY_OR_ENUM);
  print_scope_operator (scratch_buffer);
  dump_decl (TYPENAME_TYPE_FULLNAME (t), flags);
}

/* Return the name of the supplied aggregate, or enumeral type.  */

static const char *
class_key_or_enum (t)
     tree t;
{
  if (TREE_CODE (t) == ENUMERAL_TYPE)
    return "enum";
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
dump_aggr_type (t, flags)
     tree t;
     int flags;
{
  tree name;
  const char *variety = class_key_or_enum (t);
  int typdef = 0;
  int tmplate = 0;

  dump_qualifiers (t, after);

  if (flags & TFF_CLASS_KEY_OR_ENUM)
    {
      print_identifier (scratch_buffer, variety);
      output_add_space (scratch_buffer);
    }

  if (flags & TFF_CHASE_TYPEDEF)
    t = TYPE_MAIN_VARIANT (t);

  name = TYPE_NAME (t);

  if (name)
    {
      typdef = !DECL_ARTIFICIAL (name);
      tmplate = !typdef && TREE_CODE (t) != ENUMERAL_TYPE
                && TYPE_LANG_SPECIFIC (t) && CLASSTYPE_TEMPLATE_INFO (t)
                && (CLASSTYPE_TEMPLATE_SPECIALIZATION (t)
                    || TREE_CODE (CLASSTYPE_TI_TEMPLATE (t)) != TEMPLATE_DECL
                    || DECL_TEMPLATE_SPECIALIZATION (CLASSTYPE_TI_TEMPLATE (t))
                    || PRIMARY_TEMPLATE_P (CLASSTYPE_TI_TEMPLATE (t)));
      dump_scope (CP_DECL_CONTEXT (name), flags | TFF_SCOPE);
      if (tmplate)
        {
          /* Because the template names are mangled, we have to locate
             the most general template, and use that name.  */
          tree tpl = CLASSTYPE_TI_TEMPLATE (t);

          while (DECL_TEMPLATE_INFO (tpl))
            tpl = DECL_TI_TEMPLATE (tpl);
          name = tpl;
        }
      name = DECL_NAME (name);
    }

  if (name == 0 || ANON_AGGRNAME_P (name))
    {
      if (flags & TFF_CLASS_KEY_OR_ENUM)
        print_identifier (scratch_buffer, "<anonymous>");
      else
        output_printf (scratch_buffer, "<anonymous %s>", variety);
    }
  else
    print_tree_identifier (scratch_buffer, name);
  if (tmplate)
    dump_template_parms (TYPE_TEMPLATE_INFO (t),
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
   int *[]&.

   Return indicates how you should pad an object name after this. I.e. you
   want to pad non-*, non-& cores, but not pad * or & types.  */

static enum pad
dump_type_prefix (t, flags)
     tree t;
     int flags;
{
  enum pad padding = before;

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

	padding = dump_type_prefix (sub, flags);
	/* A tree for a member pointer looks like pointer to offset,
	   so let the OFFSET_TYPE case handle it.  */
	if (!TYPE_PTRMEM_P (t))
	  {
	    if (TREE_CODE (sub) == ARRAY_TYPE)
              {
                output_add_space (scratch_buffer);
                print_left_paren (scratch_buffer);
              }
            output_add_character
              (scratch_buffer, "&*"[TREE_CODE (t) == POINTER_TYPE]);
	    padding = dump_qualifiers (t, before);
	  }
      }
      break;

    case OFFSET_TYPE:
    offset_type:
      padding = dump_type_prefix (TREE_TYPE (t), flags);
      if (TREE_CODE (t) == OFFSET_TYPE)	/* pmfs deal with this in d_t_p */
	{
	  if (padding != none)
	    output_add_space (scratch_buffer);
	  dump_type (TYPE_OFFSET_BASETYPE (t), flags);
	  print_scope_operator (scratch_buffer);
	}
      output_add_character (scratch_buffer, '*');
      padding = dump_qualifiers (t, none);
      break;

      /* Can only be reached through function pointer -- this would not be
         correct if FUNCTION_DECLs used it.  */
    case FUNCTION_TYPE:
      padding = dump_type_prefix (TREE_TYPE (t), flags);
      if (padding != none)
        output_add_space (scratch_buffer);
      print_left_paren (scratch_buffer);
      padding = none;
      break;

    case METHOD_TYPE:
      padding = dump_type_prefix (TREE_TYPE (t), flags);
      if (padding != none)
        output_add_space (scratch_buffer);
      print_left_paren (scratch_buffer);
      padding = none;
      dump_aggr_type (TYPE_METHOD_BASETYPE (t), flags);
      print_scope_operator (scratch_buffer);
      break;

    case ARRAY_TYPE:
      padding = dump_type_prefix (TREE_TYPE (t), flags);
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
    case UNKNOWN_TYPE:
    case VOID_TYPE:
    case TYPENAME_TYPE:
    case COMPLEX_TYPE:
    case VECTOR_TYPE:
      dump_type (t, flags);
      padding = before;
      break;

    default:
      sorry_for_unsupported_tree (t);
      /* fall through.  */
    case ERROR_MARK:
      print_identifier (scratch_buffer, "<typeprefixerror>");
      break;
    }
  return padding;
}

/* Dump the suffix of type T, under control of FLAGS.  This is the part
   which appears after the identifier (or function parms).  */

static void
dump_type_suffix (t, flags)
     tree t;
     int flags;
{
  if (TYPE_PTRMEMFUNC_P (t))
    t = TYPE_PTRMEMFUNC_FN_TYPE (t);

  switch (TREE_CODE (t))
    {
    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case OFFSET_TYPE:
      if (TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE)
	print_right_paren (scratch_buffer);
      dump_type_suffix (TREE_TYPE (t), flags);
      break;

      /* Can only be reached through function pointer */
    case FUNCTION_TYPE:
    case METHOD_TYPE:
      {
	tree arg;
        print_right_paren (scratch_buffer);
	arg = TYPE_ARG_TYPES (t);
	if (TREE_CODE (t) == METHOD_TYPE)
	  arg = TREE_CHAIN (arg);

	/* Function pointers don't have default args.  Not in standard C++,
	   anyway; they may in g++, but we'll just pretend otherwise.  */
	dump_parameters (arg, flags & ~TFF_FUNCTION_DEFAULT_ARGUMENTS);

	if (TREE_CODE (t) == METHOD_TYPE)
	  dump_qualifiers
	    (TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (t))), before);
	dump_exception_spec (TYPE_RAISES_EXCEPTIONS (t), flags);
	dump_type_suffix (TREE_TYPE (t), flags);
	break;
      }

    case ARRAY_TYPE:
      print_left_bracket (scratch_buffer);
      if (TYPE_DOMAIN (t))
	{
	  if (host_integerp (TYPE_MAX_VALUE (TYPE_DOMAIN (t)), 0))
	    print_integer
              (scratch_buffer,
               tree_low_cst (TYPE_MAX_VALUE (TYPE_DOMAIN (t)), 0) + 1);
	  else if (TREE_CODE (TYPE_MAX_VALUE (TYPE_DOMAIN (t))) == MINUS_EXPR)
	    dump_expr (TREE_OPERAND (TYPE_MAX_VALUE (TYPE_DOMAIN (t)), 0),
	               flags & ~TFF_EXPR_IN_PARENS);
	  else
	    dump_expr (fold (cp_build_binary_op
			     (PLUS_EXPR, TYPE_MAX_VALUE (TYPE_DOMAIN (t)),
			      integer_one_node)),
	               flags & ~TFF_EXPR_IN_PARENS);
	}
      print_right_bracket (scratch_buffer);
      dump_type_suffix (TREE_TYPE (t), flags);
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
    case UNKNOWN_TYPE:
    case VOID_TYPE:
    case TYPENAME_TYPE:
    case COMPLEX_TYPE:
    case VECTOR_TYPE:
      break;

    default:
      sorry_for_unsupported_tree (t);
    case ERROR_MARK:
      /* Don't mark it here, we should have already done in
         dump_type_prefix.  */
      break;
    }
}

static void
dump_global_iord (t)
     tree t;
{
  const char *p = NULL;

  if (DECL_GLOBAL_CTOR_P (t))
    p = "initializers";
  else if (DECL_GLOBAL_DTOR_P (t))
    p = "destructors";
  else
    abort ();

  output_printf (scratch_buffer, "(static %s for %s)", p, input_filename);
}

static void
dump_simple_decl (t, type, flags)
     tree t;
     tree type;
     int flags;
{
  if (flags & TFF_DECL_SPECIFIERS)
    {
      if (dump_type_prefix (type, flags) != none)
        output_add_space (scratch_buffer);
    }
  if (!DECL_INITIAL (t) || TREE_CODE (DECL_INITIAL (t)) != TEMPLATE_PARM_INDEX)
    dump_scope (CP_DECL_CONTEXT (t), flags);
  if (DECL_NAME (t))
    dump_decl (DECL_NAME (t), flags);
  else
    print_identifier (scratch_buffer, "<anonymous>");
  if (flags & TFF_DECL_SPECIFIERS)
    dump_type_suffix (type, flags);
}

/* Dump a human readable string for the decl T under control of FLAGS.  */

static void
dump_decl (t, flags)
     tree t;
     int flags;
{
  if (t == NULL_TREE)
    return;

  switch (TREE_CODE (t))
    {
    case TYPE_DECL:
      {
	/* Don't say 'typedef class A' */
        if (DECL_ARTIFICIAL (t))
	  {
	    if ((flags & TFF_DECL_SPECIFIERS)
	        && TREE_CODE (TREE_TYPE (t)) == TEMPLATE_TYPE_PARM)
	      /* Say `class T' not just `T'. */
	      output_add_string (scratch_buffer, "class ");

	    dump_type (TREE_TYPE (t), flags);
	    break;
	  }
      }
      if (flags & TFF_DECL_SPECIFIERS)
	output_add_string (scratch_buffer, "typedef ");
      dump_simple_decl (t, DECL_ORIGINAL_TYPE (t)
			? DECL_ORIGINAL_TYPE (t) : TREE_TYPE (t),
	                flags);
      break;

    case VAR_DECL:
      if (DECL_NAME (t) && VTABLE_NAME_P (DECL_NAME (t)))
	{
	  output_add_string (scratch_buffer, "vtable for ");
	  my_friendly_assert (TYPE_P (DECL_CONTEXT (t)), 20010720);
	  dump_type (DECL_CONTEXT (t), flags);
	  break;
	}
      /* else fall through */
    case FIELD_DECL:
    case PARM_DECL:
      dump_simple_decl (t, TREE_TYPE (t), flags);
      break;

    case RESULT_DECL:
      output_add_string (scratch_buffer, "<return value> ");
      dump_simple_decl (t, TREE_TYPE (t), flags);
      break;

    case NAMESPACE_DECL:
      dump_scope (CP_DECL_CONTEXT (t), flags);
      if (DECL_NAME (t) == anonymous_namespace_name)
	print_identifier (scratch_buffer, "<unnamed>");
      else
	print_tree_identifier (scratch_buffer, DECL_NAME (t));
      break;

    case SCOPE_REF:
      dump_decl (TREE_OPERAND (t, 0), flags & ~TFF_DECL_SPECIFIERS);
      print_scope_operator (scratch_buffer); 
      dump_decl (TREE_OPERAND (t, 1), flags);
      break;

    case ARRAY_REF:
      dump_decl (TREE_OPERAND (t, 0), flags);
      print_left_bracket (scratch_buffer);
      dump_decl (TREE_OPERAND (t, 1), flags);
      print_right_bracket (scratch_buffer);
      break;

      /* So that we can do dump_decl on an aggr type.  */
    case RECORD_TYPE:
    case UNION_TYPE:
    case ENUMERAL_TYPE:
      dump_type (t, flags);
      break;

    case TYPE_EXPR:
      abort ();
      break;

      /* These special cases are duplicated here so that other functions
	 can feed identifiers to error and get them demangled properly.  */
    case IDENTIFIER_NODE:
      if (IDENTIFIER_TYPENAME_P (t))
	{
	  output_add_string (scratch_buffer, "operator ");
	  /* Not exactly IDENTIFIER_TYPE_VALUE.  */
	  dump_type (TREE_TYPE (t), flags);
	  break;
	}
      else
	print_tree_identifier (scratch_buffer, t);
      break;

    case OVERLOAD:
      t = OVL_CURRENT (t);
      /* Fall through.  */

    case FUNCTION_DECL:
      if (DECL_GLOBAL_CTOR_P (t) || DECL_GLOBAL_DTOR_P (t))
	dump_global_iord (t);
      else if (! DECL_LANG_SPECIFIC (t))
	print_identifier (scratch_buffer, "<internal>");
      else
        dump_function_decl (t, flags);
      break;

    case TEMPLATE_DECL:
      dump_template_decl (t, flags);
      break;

    case TEMPLATE_ID_EXPR:
      {
	tree args;
	tree name = TREE_OPERAND (t, 0);
	if (is_overloaded_fn (name))
	  name = DECL_NAME (get_first_fn (name));
	dump_decl (name, flags);
	print_template_argument_list_start (scratch_buffer);
	for (args = TREE_OPERAND (t, 1); args; args = TREE_CHAIN (args))
	  {
	    dump_template_argument (TREE_VALUE (args), flags);
	    if (TREE_CHAIN (args))
	      separate_with_comma (scratch_buffer);
	  }
	print_template_argument_list_end (scratch_buffer);
      }
      break;

    case LOOKUP_EXPR:
      dump_decl (TREE_OPERAND (t, 0), flags);
      break;

    case LABEL_DECL:
      print_tree_identifier (scratch_buffer, DECL_NAME (t));
      break;

    case CONST_DECL:
      if ((TREE_TYPE (t) != NULL_TREE && NEXT_CODE (t) == ENUMERAL_TYPE)
	  || (DECL_INITIAL (t) &&
	      TREE_CODE (DECL_INITIAL (t)) == TEMPLATE_PARM_INDEX))
	dump_simple_decl (t, TREE_TYPE (t), flags);
      else if (DECL_NAME (t))
	dump_decl (DECL_NAME (t), flags);
      else if (DECL_INITIAL (t))
	dump_expr (DECL_INITIAL (t), flags | TFF_EXPR_IN_PARENS);
      else
	print_identifier (scratch_buffer, "enumerator");
      break;

    case USING_DECL:
      output_add_string (scratch_buffer, "using ");
      dump_type (DECL_INITIAL (t), flags);
      print_scope_operator (scratch_buffer);
      print_tree_identifier (scratch_buffer, DECL_NAME (t));
      break;

    default:
      sorry_for_unsupported_tree (t);
      /* Fallthrough to error.  */

    case ERROR_MARK:
      print_identifier (scratch_buffer, "<declaration error>");
      break;
    }
}

/* Dump a template declaration T under control of FLAGS. This means the
   'template <...> leaders plus the 'class X' or 'void fn(...)' part.  */

static void
dump_template_decl (t, flags)
     tree t;
     int flags;
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

          output_add_string (scratch_buffer, "template<");

	  /* If we've shown the template prefix, we'd better show the
	     parameters' and decl's type too.  */
	    flags |= TFF_DECL_SPECIFIERS;

          for (i = 0; i < len; i++)
            {
              if (i)
                separate_with_comma (scratch_buffer);
              dump_template_parameter (TREE_VEC_ELT (inner_parms, i), flags);
            }
          print_template_argument_list_end (scratch_buffer);
          output_add_space (scratch_buffer);
        }
      nreverse(orig_parms);

      if (DECL_TEMPLATE_TEMPLATE_PARM_P (t))
	/* Say `template<arg> class TT' not just `template<arg> TT'. */
	output_add_string (scratch_buffer, "class ");
    }

  if (TREE_CODE (DECL_TEMPLATE_RESULT (t)) == TYPE_DECL)
    dump_type (TREE_TYPE (t),
               ((flags & ~TFF_CLASS_KEY_OR_ENUM) | TFF_TEMPLATE_NAME
                | (flags & TFF_DECL_SPECIFIERS ? TFF_CLASS_KEY_OR_ENUM : 0)));
  else if (TREE_CODE (DECL_TEMPLATE_RESULT (t)) == VAR_DECL)
    dump_decl (DECL_TEMPLATE_RESULT (t), flags | TFF_TEMPLATE_NAME);
  else if (TREE_TYPE (t) == NULL_TREE)
    abort ();
  else
    switch (NEXT_CODE (t))
    {
      case METHOD_TYPE:
      case FUNCTION_TYPE:
        dump_function_decl (t, flags | TFF_TEMPLATE_NAME);
        break;
      default:
        /* This case can occur with some illegal code.  */
        dump_type (TREE_TYPE (t),
                   (flags & ~TFF_CLASS_KEY_OR_ENUM) | TFF_TEMPLATE_NAME
                   | (flags & TFF_DECL_SPECIFIERS ? TFF_CLASS_KEY_OR_ENUM : 0));
    }
}

/* Pretty print a function decl. There are several ways we want to print a
   function declaration. The TFF_ bits in FLAGS tells us how to behave.
   As error can only apply the '#' flag once to give 0 and 1 for V, there
   is %D which doesn't print the throw specs, and %F which does. */

static void
dump_function_decl (t, flags)
     tree t;
     int flags;
{
  tree fntype;
  tree parmtypes;
  tree cname = NULL_TREE;
  tree template_args = NULL_TREE;
  tree template_parms = NULL_TREE;
  int show_return = flags & TFF_RETURN_TYPE || flags & TFF_DECL_SPECIFIERS;

  if (TREE_CODE (t) == TEMPLATE_DECL)
    t = DECL_TEMPLATE_RESULT (t);

  /* Pretty print template instantiations only.  */
  if (DECL_USE_TEMPLATE (t) && DECL_TEMPLATE_INFO (t))
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

  fntype = TREE_TYPE (t);
  parmtypes = FUNCTION_FIRST_USER_PARMTYPE (t);

  if (DECL_CLASS_SCOPE_P (t))
    cname = DECL_CONTEXT (t);
  /* this is for partially instantiated template methods */
  else if (TREE_CODE (fntype) == METHOD_TYPE)
    cname = TREE_TYPE (TREE_VALUE (parmtypes));

  if (!(flags & TFF_DECL_SPECIFIERS))
    /* OK */;
  else if (DECL_STATIC_FUNCTION_P (t))
    print_identifier (scratch_buffer, "static ");
  else if (DECL_VIRTUAL_P (t))
    print_identifier (scratch_buffer, "virtual ");

  /* Print the return type?  */
  if (show_return)
    show_return = !DECL_CONV_FN_P (t)  && !DECL_CONSTRUCTOR_P (t)
                  && !DECL_DESTRUCTOR_P (t);
  if (show_return)
    {
      dump_type_prefix (TREE_TYPE (fntype), flags);
      output_add_space (scratch_buffer);
    }

  /* Print the function name.  */
  if (cname)
    {
      dump_type (cname, flags);
      print_scope_operator (scratch_buffer);
    }
  else
    dump_scope (CP_DECL_CONTEXT (t), flags);

  dump_function_name (t, flags);

  if (1)
    {
      dump_parameters (parmtypes, flags);

      if (TREE_CODE (fntype) == METHOD_TYPE)
	dump_qualifiers (TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (fntype))),
			 before);

      if (flags & TFF_EXCEPTION_SPECIFICATION)
	dump_exception_spec (TYPE_RAISES_EXCEPTIONS (fntype), flags);

      if (show_return)
	dump_type_suffix (TREE_TYPE (fntype), flags);
    }

  /* If T is a template instantiation, dump the parameter binding.  */
  if (template_parms != NULL_TREE && template_args != NULL_TREE)
    {
      output_add_string (scratch_buffer, " [with ");
      dump_template_bindings (template_parms, template_args);
      print_right_bracket (scratch_buffer);
    }
}

/* Print a parameter list. If this is for a member function, the
   member object ptr (and any other hidden args) should have
   already been removed. */

static void
dump_parameters (parmtypes, flags)
     tree parmtypes;
     int flags;
{
  int first;

  print_left_paren (scratch_buffer);

  for (first = 1; parmtypes != void_list_node;
       parmtypes = TREE_CHAIN (parmtypes))
    {
      if (!first)
        separate_with_comma (scratch_buffer);
      first = 0;
      if (!parmtypes)
        {
          print_identifier (scratch_buffer, "...");
          break;
        }
      dump_type (TREE_VALUE (parmtypes), flags);

      if ((flags & TFF_FUNCTION_DEFAULT_ARGUMENTS) && TREE_PURPOSE (parmtypes))
        {
          output_add_string (scratch_buffer, " = ");
          dump_expr (TREE_PURPOSE (parmtypes), flags | TFF_EXPR_IN_PARENS);
        }
    }

  print_right_paren (scratch_buffer);
}

/* Print an exception specification. T is the exception specification. */

static void
dump_exception_spec (t, flags)
     tree t;
     int flags;
{
  if (t)
    {
      output_add_string (scratch_buffer, " throw (");
      if (TREE_VALUE (t) != NULL_TREE)
        while (1)
          {
            dump_type (TREE_VALUE (t), flags);
            t = TREE_CHAIN (t);
            if (!t)
              break;
            separate_with_comma (scratch_buffer);
          }
      print_right_paren (scratch_buffer);
    }
}

/* Handle the function name for a FUNCTION_DECL node, grokking operators
   and destructors properly.  */

static void
dump_function_name (t, flags)
     tree t;
     int flags;
{
  tree name = DECL_NAME (t);

  /* Don't let the user see __comp_ctor et al.  */
  if (DECL_CONSTRUCTOR_P (t)
      || DECL_DESTRUCTOR_P (t))
    name = constructor_name (DECL_CONTEXT (t));

  if (DECL_DESTRUCTOR_P (t))
    {
      output_add_character (scratch_buffer, '~');
      dump_decl (name, TFF_PLAIN_IDENTIFIER);
    }
  else if (DECL_CONV_FN_P (t))
    {
      /* This cannot use the hack that the operator's return
	 type is stashed off of its name because it may be
	 used for error reporting.  In the case of conflicting
	 declarations, both will have the same name, yet
	 the types will be different, hence the TREE_TYPE field
	 of the first name will be clobbered by the second.  */
      output_add_string (scratch_buffer, "operator ");
      dump_type (TREE_TYPE (TREE_TYPE (t)), flags);
    }
  else if (IDENTIFIER_OPNAME_P (name))
    print_tree_identifier (scratch_buffer, name);
  else
    dump_decl (name, flags);

  if (DECL_LANG_SPECIFIC (t) && DECL_TEMPLATE_INFO (t)
      && !DECL_FRIEND_PSEUDO_TEMPLATE_INSTANTIATION (t)
      && (DECL_TEMPLATE_SPECIALIZATION (t)
	  || TREE_CODE (DECL_TI_TEMPLATE (t)) != TEMPLATE_DECL
	  || DECL_TEMPLATE_SPECIALIZATION (DECL_TI_TEMPLATE (t))
	  || PRIMARY_TEMPLATE_P (DECL_TI_TEMPLATE (t))))
    dump_template_parms (DECL_TEMPLATE_INFO (t), !DECL_USE_TEMPLATE (t), flags);
}

/* Dump the template parameters from the template info INFO under control of
   FLAGS. PRIMARY indicates whether this is a primary template decl, or
   specialization (partial or complete). For partial specializations we show
   the specialized parameter values. For a primary template we show no
   decoration.  */

static void
dump_template_parms (info, primary, flags)
     tree info;
     int primary;
     int flags;
{
  tree args = info ? TI_ARGS (info) : NULL_TREE;

  if (primary && flags & TFF_TEMPLATE_NAME)
    return;
  flags &= ~(TFF_CLASS_KEY_OR_ENUM | TFF_TEMPLATE_NAME);
  print_template_argument_list_start (scratch_buffer);

  /* Be careful only to print things when we have them, so as not
	 to crash producing error messages.  */
  if (args && !primary)
    {
      int len = 0;
      int ix = 0;
      int need_comma = 0;

      if (TREE_CODE (args) == TREE_VEC)
        {
          if (TREE_VEC_LENGTH (args) > 0
	      && TREE_CODE (TREE_VEC_ELT (args, 0)) == TREE_VEC)
	    args = TREE_VEC_ELT (args, TREE_VEC_LENGTH (args) - 1);

          len = TREE_VEC_LENGTH (args);
        }
      else if (TREE_CODE (args) == TREE_LIST)
        len = -1;
      while (ix != len && args)
        {
          tree arg;
          if (len >= 0)
            {
              arg = TREE_VEC_ELT (args, ix);
              ix++;
            }
          else
            {
              arg = TREE_VALUE (args);
              args = TREE_CHAIN (args);
            }
          if (need_comma)
            separate_with_comma (scratch_buffer);
          
          if (!arg)
            print_identifier (scratch_buffer, "<template parameter error>");
          else
            dump_template_argument (arg, flags);
          need_comma = 1;
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
          tree parm = TREE_VALUE (TREE_VEC_ELT (parms, ix));

          if (ix)
            separate_with_comma (scratch_buffer);

          dump_decl (parm, flags & ~TFF_DECL_SPECIFIERS);
        }
    }
  print_template_argument_list_end (scratch_buffer);
}

static void
dump_char (c)
     int c;
{
  switch (c)
    {
    case TARGET_NEWLINE:
      output_add_string (scratch_buffer, "\\n");
      break;
    case TARGET_TAB:
      output_add_string (scratch_buffer, "\\t");
      break;
    case TARGET_VT:
      output_add_string (scratch_buffer, "\\v");
      break;
    case TARGET_BS:
      output_add_string (scratch_buffer, "\\b");
      break;
    case TARGET_CR:
      output_add_string (scratch_buffer, "\\r");
      break;
    case TARGET_FF:
      output_add_string (scratch_buffer, "\\f");
      break;
    case TARGET_BELL:
      output_add_string (scratch_buffer, "\\a");
      break;
    case '\\':
      output_add_string (scratch_buffer, "\\\\");
      break;
    case '\'':
      output_add_string (scratch_buffer, "\\'");
      break;
    case '\"':
      output_add_string (scratch_buffer, "\\\"");
      break;
    default:
      if (ISPRINT (c))
	output_add_character (scratch_buffer, c);
      else
	{
	  sprintf (digit_buffer, "\\%03o", (int) c);
	  output_add_string (scratch_buffer, digit_buffer);
	}
    }
}

/* Print out a list of initializers (subr of dump_expr) */

static void
dump_expr_list (l, flags)
     tree l;
     int flags;
{
  while (l)
    {
      dump_expr (TREE_VALUE (l), flags | TFF_EXPR_IN_PARENS);
      l = TREE_CHAIN (l);
      if (l)
	separate_with_comma (scratch_buffer);
    }
}

/* Print out an expression E under control of FLAGS. */

static void
dump_expr (t, flags)
     tree t;
     int flags;
{
  switch (TREE_CODE (t))
    {
    case VAR_DECL:
    case PARM_DECL:
    case FIELD_DECL:
    case CONST_DECL:
    case FUNCTION_DECL:
    case TEMPLATE_DECL:
    case NAMESPACE_DECL:
    case OVERLOAD:
      dump_decl (t, flags & ~TFF_DECL_SPECIFIERS);
      break;

    case INTEGER_CST:
      {
	tree type = TREE_TYPE (t);
	my_friendly_assert (type != 0, 81);

	/* If it's an enum, output its tag, rather than its value.  */
	if (TREE_CODE (type) == ENUMERAL_TYPE)
	  {
	    tree values = TYPE_VALUES (type);

	    for (; values;
	         values = TREE_CHAIN (values))
	      if (tree_int_cst_equal (TREE_VALUE (values), t))
	        break;

	    if (values)
	      print_tree_identifier (scratch_buffer, TREE_PURPOSE (values));
	    else
	      {
                /* Value must have been cast.  */
                print_left_paren (scratch_buffer);
                dump_type (type, flags);
                print_right_paren (scratch_buffer);
                goto do_int;
	      }
	  }
	else if (type == boolean_type_node)
	  {
	    if (t == boolean_false_node || integer_zerop (t))
	      print_identifier (scratch_buffer, "false");
	    else if (t == boolean_true_node)
	      print_identifier (scratch_buffer, "true");
	  }
	else if (type == char_type_node)
	  {
	    output_add_character (scratch_buffer, '\'');
	    dump_char (tree_low_cst (t, 0));
	    output_add_character (scratch_buffer, '\'');
	  }
	else
	  {
	    do_int:
	    if (! host_integerp (t, 0))
	      {
	        tree val = t;

	        if (tree_int_cst_sgn (val) < 0)
	          {
		    output_add_character (scratch_buffer, '-');
		    val = build_int_2 (-TREE_INT_CST_LOW (val),
				       ~TREE_INT_CST_HIGH (val)
	                               + !TREE_INT_CST_LOW (val));
	          }
	        /* Would "%x%0*x" or "%x%*0x" get zero-padding on all
	           systems?  */
	        {
	          static char format[10]; /* "%x%09999x\0" */
	          if (!format[0])
		    sprintf (format, "%%x%%0%dx", HOST_BITS_PER_INT / 4);
	          sprintf (digit_buffer, format, TREE_INT_CST_HIGH (val),
		           TREE_INT_CST_LOW (val));
	          output_add_string (scratch_buffer, digit_buffer);
	        }
	      }
	    else
	      print_integer (scratch_buffer, TREE_INT_CST_LOW (t));
	  }
      }
      break;

    case REAL_CST:
#ifndef REAL_IS_NOT_DOUBLE
      sprintf (digit_buffer, "%g", TREE_REAL_CST (t));
#else
      {
	const unsigned char *p = (const unsigned char *) &TREE_REAL_CST (t);
	size_t i;
	strcpy (digit_buffer, "0x");
	for (i = 0; i < sizeof TREE_REAL_CST (t); i++)
	  sprintf (digit_buffer + 2 + 2*i, "%02x", *p++);
      }
#endif
      output_add_string (scratch_buffer, digit_buffer);
      break;

    case PTRMEM_CST:
      output_add_character (scratch_buffer, '&');
      dump_type (PTRMEM_CST_CLASS (t), flags);
      print_scope_operator (scratch_buffer);
      print_tree_identifier
        (scratch_buffer, DECL_NAME (PTRMEM_CST_MEMBER (t)));
      break;

    case STRING_CST:
      {
	const char *p = TREE_STRING_POINTER (t);
	int len = TREE_STRING_LENGTH (t) - 1;
	int i;

	output_add_character (scratch_buffer, '\"');
	for (i = 0; i < len; i++)
	  dump_char (p[i]);
	output_add_character (scratch_buffer, '\"');
      }
      break;

    case COMPOUND_EXPR:
      print_left_paren (scratch_buffer);
      dump_expr (TREE_OPERAND (t, 0), flags | TFF_EXPR_IN_PARENS);
      separate_with_comma (scratch_buffer);
      dump_expr (TREE_OPERAND (t, 1), flags | TFF_EXPR_IN_PARENS);
      print_right_paren (scratch_buffer);
      break;

    case COND_EXPR:
      print_left_paren (scratch_buffer);
      dump_expr (TREE_OPERAND (t, 0), flags | TFF_EXPR_IN_PARENS);
      output_add_string (scratch_buffer, " ? ");
      dump_expr (TREE_OPERAND (t, 1), flags | TFF_EXPR_IN_PARENS);
      output_add_string (scratch_buffer, " : ");
      dump_expr (TREE_OPERAND (t, 2), flags | TFF_EXPR_IN_PARENS);
      print_right_paren (scratch_buffer);
      break;

    case SAVE_EXPR:
      if (TREE_HAS_CONSTRUCTOR (t))
	{
	  output_add_string (scratch_buffer, "new ");
	  dump_type (TREE_TYPE (TREE_TYPE (t)), flags);
	}
      else
	{
	  dump_expr (TREE_OPERAND (t, 0), flags | TFF_EXPR_IN_PARENS);
	}
      break;

    case AGGR_INIT_EXPR:
      {
	tree fn = NULL_TREE;

	if (TREE_CODE (TREE_OPERAND (t, 0)) == ADDR_EXPR)
	  fn = TREE_OPERAND (TREE_OPERAND (t, 0), 0);

	if (fn && TREE_CODE (fn) == FUNCTION_DECL)
	  {
	    if (DECL_CONSTRUCTOR_P (fn))
	      print_tree_identifier
                (scratch_buffer, TYPE_IDENTIFIER (TREE_TYPE (t)));
	    else
	      dump_decl (fn, 0);
	  }
	else
	  dump_expr (TREE_OPERAND (t, 0), 0);
      }
      print_left_paren (scratch_buffer);
      if (TREE_OPERAND (t, 1))
	dump_expr_list (TREE_CHAIN (TREE_OPERAND (t, 1)), flags);
      print_right_paren (scratch_buffer);
      break;

    case CALL_EXPR:
      {
	tree fn = TREE_OPERAND (t, 0);
	tree args = TREE_OPERAND (t, 1);

	if (TREE_CODE (fn) == ADDR_EXPR)
	  fn = TREE_OPERAND (fn, 0);

	if (TREE_TYPE (fn) != NULL_TREE && NEXT_CODE (fn) == METHOD_TYPE)
	  {
	    tree ob = TREE_VALUE (args);
	    if (TREE_CODE (ob) == ADDR_EXPR)
	      {
		dump_expr (TREE_OPERAND (ob, 0), flags | TFF_EXPR_IN_PARENS);
		output_add_character (scratch_buffer, '.');
	      }
	    else if (TREE_CODE (ob) != PARM_DECL
		     || strcmp (IDENTIFIER_POINTER (DECL_NAME (ob)), "this"))
	      {
		dump_expr (ob, flags | TFF_EXPR_IN_PARENS);
		output_add_string (scratch_buffer, "->");
	      }
	    args = TREE_CHAIN (args);
	  }
	dump_expr (fn, flags | TFF_EXPR_IN_PARENS);
	print_left_paren (scratch_buffer);
	dump_expr_list (args, flags);
	print_right_paren (scratch_buffer);
      }
      break;

    case NEW_EXPR:
      {
	tree type = TREE_OPERAND (t, 1);
	if (NEW_EXPR_USE_GLOBAL (t))
	  print_scope_operator (scratch_buffer);
	output_add_string (scratch_buffer, "new ");
	if (TREE_OPERAND (t, 0))
	  {
	    print_left_paren (scratch_buffer);
	    dump_expr_list (TREE_OPERAND (t, 0), flags);
	    output_add_string (scratch_buffer, ") ");
	  }
	if (TREE_CODE (type) == ARRAY_REF)
	  type = build_cplus_array_type
	    (TREE_OPERAND (type, 0),
	     build_index_type (fold (build (MINUS_EXPR, integer_type_node,
					    TREE_OPERAND (type, 1),
					    integer_one_node))));
	dump_type (type, flags);
	if (TREE_OPERAND (t, 2))
	  {
	    print_left_paren (scratch_buffer);
	    dump_expr_list (TREE_OPERAND (t, 2), flags);
	    print_right_paren (scratch_buffer);
	  }
      }
      break;

    case TARGET_EXPR:
      /* Note that this only works for G++ target exprs.  If somebody
	 builds a general TARGET_EXPR, there's no way to represent that
	 it initializes anything other that the parameter slot for the
	 default argument.  Note we may have cleared out the first
	 operand in expand_expr, so don't go killing ourselves.  */
      if (TREE_OPERAND (t, 1))
	dump_expr (TREE_OPERAND (t, 1), flags | TFF_EXPR_IN_PARENS);
      break;

    case INIT_EXPR:
    case MODIFY_EXPR:
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
    case BIT_ANDTC_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case EXACT_DIV_EXPR:
      dump_binary_op (operator_name_info[(int) TREE_CODE (t)].name, t, flags);
      break;

    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
      dump_binary_op ("/", t, flags);
      break;

    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
      dump_binary_op ("%", t, flags);
      break;

    case COMPONENT_REF:
      {
	tree ob = TREE_OPERAND (t, 0);
	if (TREE_CODE (ob) == INDIRECT_REF)
	  {
	    ob = TREE_OPERAND (ob, 0);
	    if (TREE_CODE (ob) != PARM_DECL
		|| strcmp (IDENTIFIER_POINTER (DECL_NAME (ob)), "this"))
	      {
		dump_expr (ob, flags | TFF_EXPR_IN_PARENS);
		output_add_string (scratch_buffer, "->");
	      }
	  }
	else
	  {
	    dump_expr (ob, flags | TFF_EXPR_IN_PARENS);
	    output_add_character (scratch_buffer, '.');
	  }
	dump_expr (TREE_OPERAND (t, 1), flags & ~TFF_EXPR_IN_PARENS);
      }
      break;

    case ARRAY_REF:
      dump_expr (TREE_OPERAND (t, 0), flags | TFF_EXPR_IN_PARENS);
      print_left_bracket (scratch_buffer);
      dump_expr (TREE_OPERAND (t, 1), flags | TFF_EXPR_IN_PARENS);
      print_right_bracket (scratch_buffer);
      break;

    case CONVERT_EXPR:
      if (TREE_TYPE (t) && VOID_TYPE_P (TREE_TYPE (t)))
	{
	  print_left_paren (scratch_buffer);
	  dump_type (TREE_TYPE (t), flags);
	  print_right_paren (scratch_buffer);
	  dump_expr (TREE_OPERAND (t, 0), flags);
	}
      else
	dump_unary_op ("+", t, flags);
      break;

    case ADDR_EXPR:
      if (TREE_CODE (TREE_OPERAND (t, 0)) == FUNCTION_DECL
	  || TREE_CODE (TREE_OPERAND (t, 0)) == STRING_CST
	  /* An ADDR_EXPR can have reference type.  In that case, we
	     shouldn't print the `&' doing so indicates to the user
	     that the expression has pointer type.  */
	  || (TREE_TYPE (t)
	      && TREE_CODE (TREE_TYPE (t)) == REFERENCE_TYPE))
	dump_expr (TREE_OPERAND (t, 0), flags | TFF_EXPR_IN_PARENS);
      else
	dump_unary_op ("&", t, flags);
      break;

    case INDIRECT_REF:
      if (TREE_HAS_CONSTRUCTOR (t))
	{
	  t = TREE_OPERAND (t, 0);
	  my_friendly_assert (TREE_CODE (t) == CALL_EXPR, 237);
	  dump_expr (TREE_OPERAND (t, 0), flags | TFF_EXPR_IN_PARENS);
	  print_left_paren (scratch_buffer);
	  dump_expr_list (TREE_CHAIN (TREE_OPERAND (t, 1)), flags);
	  print_right_paren (scratch_buffer);
	}
      else
	{
	  if (TREE_OPERAND (t,0) != NULL_TREE
	      && TREE_TYPE (TREE_OPERAND (t, 0))
	      && NEXT_CODE (TREE_OPERAND (t, 0)) == REFERENCE_TYPE)
	    dump_expr (TREE_OPERAND (t, 0), flags);
	  else
	    dump_unary_op ("*", t, flags);
	}
      break;

    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
      dump_unary_op (operator_name_info [(int)TREE_CODE (t)].name, t, flags);
      break;

    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
      print_left_paren (scratch_buffer);
      dump_expr (TREE_OPERAND (t, 0), flags | TFF_EXPR_IN_PARENS);
      print_identifier
        (scratch_buffer, operator_name_info[(int)TREE_CODE (t)].name);
      print_right_paren (scratch_buffer);
      break;

    case NON_LVALUE_EXPR:
      /* FIXME: This is a KLUDGE workaround for a parsing problem.  There
	 should be another level of INDIRECT_REF so that I don't have to do
	 this.  */
      if (TREE_TYPE (t) != NULL_TREE && NEXT_CODE (t) == POINTER_TYPE)
	{
	  tree next = TREE_TYPE (TREE_TYPE (t));

	  while (TREE_CODE (next) == POINTER_TYPE)
	    next = TREE_TYPE (next);

	  if (TREE_CODE (next) == FUNCTION_TYPE)
	    {
	      if (flags & TFF_EXPR_IN_PARENS)
	        print_left_paren (scratch_buffer);
	      output_add_character (scratch_buffer, '*');
	      dump_expr (TREE_OPERAND (t, 0), flags & ~TFF_EXPR_IN_PARENS);
	      if (flags & TFF_EXPR_IN_PARENS)
	        print_right_paren (scratch_buffer);
	      break;
	    }
	  /* else FALLTHRU */
	}
      dump_expr (TREE_OPERAND (t, 0), flags | TFF_EXPR_IN_PARENS);
      break;

    case NOP_EXPR:
      dump_expr (TREE_OPERAND (t, 0), flags);
      break;

    case EXPR_WITH_FILE_LOCATION:
      dump_expr (EXPR_WFL_NODE (t), flags);
      break;

    case CONSTRUCTOR:
      if (TREE_TYPE (t) && TYPE_PTRMEMFUNC_P (TREE_TYPE (t)))
	{
	  tree idx = build_component_ref (t, pfn_identifier, NULL_TREE, 0);

	  if (integer_zerop (idx))
	    {
	      /* A NULL pointer-to-member constant.  */
	      output_add_string (scratch_buffer, "((");
	      dump_type (TREE_TYPE (t), flags);
	      output_add_string (scratch_buffer, ") 0)");
	      break;
	    }
	  else if (host_integerp (idx, 0))
	    {
	      tree virtuals;
	      unsigned HOST_WIDE_INT n;

	      t = TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (TREE_TYPE (t)));
	      t = TYPE_METHOD_BASETYPE (t);
	      virtuals = TYPE_BINFO_VIRTUALS (TYPE_MAIN_VARIANT (t));

	      n = tree_low_cst (idx, 0);

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
		  dump_expr (BV_FN (virtuals),
	                     flags | TFF_EXPR_IN_PARENS);
		  break;
		}
	    }
	}
      output_add_character (scratch_buffer, '{');
      dump_expr_list (CONSTRUCTOR_ELTS (t), flags);
      output_add_character (scratch_buffer, '}');
      break;

    case OFFSET_REF:
      {
	tree ob = TREE_OPERAND (t, 0);
	if (is_dummy_object (ob))
	  {
	    t = TREE_OPERAND (t, 1);
	    if (TREE_CODE (t) == FUNCTION_DECL)
	      /* A::f */
	      dump_expr (t, flags | TFF_EXPR_IN_PARENS);
	    else if (BASELINK_P (t))
	      dump_expr (OVL_CURRENT (TREE_VALUE (t)), flags | TFF_EXPR_IN_PARENS);
	    else
	      dump_decl (t, flags);
	  }
	else
	  {
	    if (TREE_CODE (ob) == INDIRECT_REF)
	      {
		dump_expr (TREE_OPERAND (ob, 0), flags | TFF_EXPR_IN_PARENS);
		output_add_string (scratch_buffer, "->*");
	      }
	    else
	      {
		dump_expr (ob, flags | TFF_EXPR_IN_PARENS);
		output_add_string (scratch_buffer, ".*");
	      }
	    dump_expr (TREE_OPERAND (t, 1), flags | TFF_EXPR_IN_PARENS);
	  }
	break;
      }

    case TEMPLATE_PARM_INDEX:
      dump_decl (TEMPLATE_PARM_DECL (t), flags & ~TFF_DECL_SPECIFIERS);
      break;

    case IDENTIFIER_NODE:
      print_tree_identifier (scratch_buffer, t);
      break;

    case SCOPE_REF:
      dump_type (TREE_OPERAND (t, 0), flags);
      print_scope_operator (scratch_buffer);
      dump_expr (TREE_OPERAND (t, 1), flags | TFF_EXPR_IN_PARENS);
      break;

    case CAST_EXPR:
      if (TREE_OPERAND (t, 0) == NULL_TREE
	  || TREE_CHAIN (TREE_OPERAND (t, 0)))
	{
	  dump_type (TREE_TYPE (t), flags);
	  print_left_paren (scratch_buffer);
	  dump_expr_list (TREE_OPERAND (t, 0), flags);
	  print_right_paren (scratch_buffer);
	}
      else
	{
	  print_left_paren (scratch_buffer);
	  dump_type (TREE_TYPE (t), flags);
          output_add_string (scratch_buffer, ")(");
	  dump_expr_list (TREE_OPERAND (t, 0), flags);
	  print_right_paren (scratch_buffer);
	}
      break;

    case STATIC_CAST_EXPR:
      output_add_string (scratch_buffer, "static_cast<");
      goto cast;
    case REINTERPRET_CAST_EXPR:
      output_add_string (scratch_buffer, "reinterpret_cast<");
      goto cast;
    case CONST_CAST_EXPR:
      output_add_string (scratch_buffer, "const_cast<");
      goto cast;
    case DYNAMIC_CAST_EXPR:
      output_add_string (scratch_buffer, "dynamic_cast<");
    cast:
      dump_type (TREE_TYPE (t), flags);
      output_add_string (scratch_buffer, ">(");
      dump_expr (TREE_OPERAND (t, 0), flags);
      print_right_paren (scratch_buffer);
      break;

    case LOOKUP_EXPR:
      print_tree_identifier (scratch_buffer, TREE_OPERAND (t, 0));
      break;

    case ARROW_EXPR:
      dump_expr (TREE_OPERAND (t, 0), flags);
      output_add_string (scratch_buffer, "->");
      break;

    case SIZEOF_EXPR:
    case ALIGNOF_EXPR:
      if (TREE_CODE (t) == SIZEOF_EXPR)
	output_add_string (scratch_buffer, "sizeof (");
      else
	{
	  my_friendly_assert (TREE_CODE (t) == ALIGNOF_EXPR, 0);
	  output_add_string (scratch_buffer, "__alignof__ (");
	}
      if (TYPE_P (TREE_OPERAND (t, 0)))
	dump_type (TREE_OPERAND (t, 0), flags);
      else
	dump_unary_op ("*", t, flags | TFF_EXPR_IN_PARENS);
      print_right_paren (scratch_buffer);
      break;

    case DEFAULT_ARG:
      print_identifier (scratch_buffer, "<unparsed>");
      break;

    case TRY_CATCH_EXPR:
    case WITH_CLEANUP_EXPR:
    case CLEANUP_POINT_EXPR:
      dump_expr (TREE_OPERAND (t, 0), flags);
      break;

    case PSEUDO_DTOR_EXPR:
      dump_expr (TREE_OPERAND (t, 2), flags);
      output_add_character (scratch_buffer, '.');
      dump_type (TREE_OPERAND (t, 0), flags);
      output_add_string (scratch_buffer, "::~");
      dump_type (TREE_OPERAND (t, 1), flags);
      break;

    case TEMPLATE_ID_EXPR:
      dump_decl (t, flags);
      break;

    case STMT_EXPR:
      /* We don't yet have a way of dumping statements in a
	 human-readable format.  */
      output_add_string (scratch_buffer, "({...})");
      break;

    case BIND_EXPR:
      output_add_character (scratch_buffer, '{');
      dump_expr (TREE_OPERAND (t, 1), flags & ~TFF_EXPR_IN_PARENS);
      output_add_character (scratch_buffer, '}');
      break;

    case LOOP_EXPR:
      output_add_string (scratch_buffer, "while (1) { ");
      dump_expr (TREE_OPERAND (t, 0), flags & ~TFF_EXPR_IN_PARENS);
      output_add_character (scratch_buffer, '}');
      break;

    case EXIT_EXPR:
      output_add_string (scratch_buffer, "if (");
      dump_expr (TREE_OPERAND (t, 0), flags & ~TFF_EXPR_IN_PARENS);
      output_add_string (scratch_buffer, ") break; ");
      break;

    case TREE_LIST:
      if (TREE_VALUE (t) && TREE_CODE (TREE_VALUE (t)) == FUNCTION_DECL)
	{
	  print_tree_identifier (scratch_buffer, DECL_NAME (TREE_VALUE (t)));
	  break;
	}
      /* else fall through */

      /*  This list is incomplete, but should suffice for now.
	  It is very important that `sorry' does not call
	  `report_error_function'.  That could cause an infinite loop.  */
    default:
      sorry_for_unsupported_tree (t);
      /* fall through to ERROR_MARK...  */
    case ERROR_MARK:
      print_identifier (scratch_buffer, "<expression error>");
      break;
    }
}

static void
dump_binary_op (opstring, t, flags)
     const char *opstring;
     tree t;
     int flags;
{
  print_left_paren (scratch_buffer);
  dump_expr (TREE_OPERAND (t, 0), flags | TFF_EXPR_IN_PARENS);
  output_add_space (scratch_buffer);
  if (opstring)
    print_identifier (scratch_buffer, opstring);
  else
    print_identifier (scratch_buffer, "<unknown operator>");
  output_add_space (scratch_buffer);
  dump_expr (TREE_OPERAND (t, 1), flags | TFF_EXPR_IN_PARENS);
  print_right_paren (scratch_buffer);
}

static void
dump_unary_op (opstring, t, flags)
     const char *opstring;
     tree t;
     int flags;
{
  if (flags & TFF_EXPR_IN_PARENS)
    print_left_paren (scratch_buffer);
  print_identifier (scratch_buffer, opstring);
  dump_expr (TREE_OPERAND (t, 0), flags & ~TFF_EXPR_IN_PARENS);
  if (flags & TFF_EXPR_IN_PARENS)
    print_right_paren (scratch_buffer);
}

/* Exported interface to stringifying types, exprs and decls under TFF_*
   control.  */

const char *
type_as_string (typ, flags)
     tree typ;
     int flags;
{
  reinit_global_formatting_buffer ();

  dump_type (typ, flags);

  return output_finalize_message (scratch_buffer);
}

const char *
expr_as_string (decl, flags)
     tree decl;
     int flags;
{
  reinit_global_formatting_buffer ();

  dump_expr (decl, flags);

  return output_finalize_message (scratch_buffer);
}

const char *
decl_as_string (decl, flags)
     tree decl;
     int flags;
{
  reinit_global_formatting_buffer ();

  dump_decl (decl, flags);

  return output_finalize_message (scratch_buffer);
}

const char *
context_as_string (context, flags)
     tree context;
     int flags;
{
  reinit_global_formatting_buffer ();

  dump_scope (context, flags);

  return output_finalize_message (scratch_buffer);
}

/* Generate the three forms of printable names for lang_printable_name.  */

const char *
lang_decl_name (decl, v)
     tree decl;
     int v;
{
  if (v >= 2)
    return decl_as_string (decl, TFF_DECL_SPECIFIERS);

  reinit_global_formatting_buffer ();

  if (v == 1 && DECL_CLASS_SCOPE_P (decl))
    {
      dump_type (CP_DECL_CONTEXT (decl), TFF_PLAIN_IDENTIFIER);
      print_scope_operator (scratch_buffer);
    }

  if (TREE_CODE (decl) == FUNCTION_DECL)
    dump_function_name (decl, TFF_PLAIN_IDENTIFIER);
  else
    dump_decl (DECL_NAME (decl), TFF_PLAIN_IDENTIFIER);

  return output_finalize_message (scratch_buffer);
}

const char *
cp_file_of (t)
     tree t;
{
  if (TREE_CODE (t) == PARM_DECL && DECL_CONTEXT (t))
    return DECL_SOURCE_FILE (DECL_CONTEXT (t));
  else if (TYPE_P (t))
    return DECL_SOURCE_FILE (TYPE_MAIN_DECL (t));
  else if (TREE_CODE (t) == OVERLOAD)
    return DECL_SOURCE_FILE (OVL_FUNCTION (t));
  else
    return DECL_SOURCE_FILE (t);
}

int
cp_line_of (t)
     tree t;
{
  int line = 0;
  if (TREE_CODE (t) == PARM_DECL && DECL_CONTEXT (t))
    line = DECL_SOURCE_LINE (DECL_CONTEXT (t));
  if (TREE_CODE (t) == TYPE_DECL && DECL_ARTIFICIAL (t)
      && TYPE_MAIN_DECL (TREE_TYPE (t)))
    t = TREE_TYPE (t);

  if (TYPE_P (t))
    line = DECL_SOURCE_LINE (TYPE_MAIN_DECL (t));
  else if (TREE_CODE (t) == OVERLOAD)
    line = DECL_SOURCE_LINE (OVL_FUNCTION (t));
  else
    line = DECL_SOURCE_LINE (t);

  if (line == 0)
    return lineno;

  return line;
}

/* Now the interfaces from error et al to dump_type et al. Each takes an
   on/off VERBOSE flag and supply the appropriate TFF_ flags to a dump_
   function.  */

static const char *
decl_to_string (decl, verbose)
     tree decl;
     int verbose;
{
  int flags = 0;

  if (TREE_CODE (decl) == TYPE_DECL || TREE_CODE (decl) == RECORD_TYPE
      || TREE_CODE (decl) == UNION_TYPE || TREE_CODE (decl) == ENUMERAL_TYPE)
    flags = TFF_CLASS_KEY_OR_ENUM;
  if (verbose)
    flags |= TFF_DECL_SPECIFIERS | TFF_FUNCTION_DEFAULT_ARGUMENTS;
  else if (TREE_CODE (decl) == FUNCTION_DECL)
    flags |= TFF_DECL_SPECIFIERS | TFF_RETURN_TYPE;
  flags |= TFF_TEMPLATE_HEADER;

  reinit_global_formatting_buffer ();

  dump_decl (decl, flags);

  return output_finalize_message (scratch_buffer);
}

static const char *
expr_to_string (decl, verbose)
     tree decl;
     int verbose ATTRIBUTE_UNUSED;
{
  reinit_global_formatting_buffer ();

  dump_expr (decl, 0);

  return output_finalize_message (scratch_buffer);
}

static const char *
fndecl_to_string (fndecl, verbose)
     tree fndecl;
     int verbose;
{
  int flags;

  flags = TFF_EXCEPTION_SPECIFICATION | TFF_DECL_SPECIFIERS;
  if (verbose)
    flags |= TFF_FUNCTION_DEFAULT_ARGUMENTS;
  reinit_global_formatting_buffer ();

  dump_decl (fndecl, flags);

  return output_finalize_message (scratch_buffer);
}


static const char *
code_to_string (c, v)
     enum tree_code c;
     int v ATTRIBUTE_UNUSED;
{
  return tree_code_name [c];
}

const char *
language_to_string (c, v)
     enum languages c;
     int v ATTRIBUTE_UNUSED;
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
      abort ();
      return 0;
    }
}

/* Return the proper printed version of a parameter to a C++ function.  */

static const char *
parm_to_string (p, v)
     int p;
     int v ATTRIBUTE_UNUSED;
{
  if (p < 0)
    return "`this'";

  sprintf (digit_buffer, "%d", p+1);
  return digit_buffer;
}

static const char *
op_to_string (p, v)
     enum tree_code p;
     int v ATTRIBUTE_UNUSED;
{
  tree id;

  id = operator_name_info[(int) p].identifier;
  return id ? IDENTIFIER_POINTER (id) : "{unknown}";
}

static const char *
type_to_string (typ, verbose)
     tree typ;
     int verbose;
{
  int flags;

  flags = 0;
  if (verbose)
    flags |= TFF_CLASS_KEY_OR_ENUM;
  flags |= TFF_TEMPLATE_HEADER;

  reinit_global_formatting_buffer ();

  dump_type (typ, flags);

  return output_finalize_message (scratch_buffer);
}

static const char *
assop_to_string (p, v)
     enum tree_code p;
     int v ATTRIBUTE_UNUSED;
{
  tree id;

  id = assignment_operator_name_info[(int) p].identifier;
  return id ? IDENTIFIER_POINTER (id) : "{unknown}";
}

static const char *
args_to_string (p, verbose)
     tree p;
     int verbose;
{
  int flags = 0;
  if (verbose)
    flags |= TFF_CLASS_KEY_OR_ENUM;

  if (p == NULL_TREE)
    return "";

  if (TYPE_P (TREE_VALUE (p)))
    return type_as_string (p, flags);

  reinit_global_formatting_buffer ();
  for (; p; p = TREE_CHAIN (p))
    {
      if (TREE_VALUE (p) == null_node)
	print_identifier (scratch_buffer, "NULL");
      else
	dump_type (error_type (TREE_VALUE (p)), flags);
      if (TREE_CHAIN (p))
	separate_with_comma (scratch_buffer);
    }
  return output_finalize_message (scratch_buffer);
}

static const char *
cv_to_string (p, v)
     tree p;
     int v;
{
  reinit_global_formatting_buffer ();

  dump_qualifiers (p, v ? before : none);

  return output_finalize_message (scratch_buffer);
}

static void
lang_print_error_function (context, file)
     diagnostic_context *context;
     const char *file;
{
  output_state os;

  default_print_error_function (context, file);
  os = output_buffer_state (context);
  output_set_prefix ((output_buffer *)context, file);
  maybe_print_instantiation_context ((output_buffer *)context);
  output_buffer_state (context) = os;
}

static void
cp_diagnostic_starter (buffer, dc)
     output_buffer *buffer;
     diagnostic_context *dc;
{
  report_problematic_module (buffer);
  cp_print_error_function (buffer, dc);
  maybe_print_instantiation_context (buffer);
  output_set_prefix (buffer,
                     context_as_prefix (diagnostic_file_location (dc),
                                        diagnostic_line_location (dc),
                                        diagnostic_is_warning (dc)));
}

static void
cp_diagnostic_finalizer (buffer, dc)
     output_buffer *buffer;
     diagnostic_context *dc __attribute__ ((__unused__));
{
  output_destroy_prefix (buffer);
}

/* Print current function onto BUFFER, in the process of reporting
   a diagnostic message.  Called from cp_diagnostic_starter.  */
static void
cp_print_error_function (buffer, dc)
     output_buffer *buffer;
     diagnostic_context *dc;
{
  if (error_function_changed ())
    {
      char *prefix = diagnostic_file_location (dc)
        ? file_name_as_prefix (diagnostic_file_location (dc))
        : NULL;
      output_state os;

      os = output_buffer_state (buffer);
      output_set_prefix (buffer, prefix);

      if (current_function_decl == NULL)
        output_add_string (buffer, "At global scope:");
      else
        output_printf
          (buffer, "In %s `%s':", function_category (current_function_decl),
           (*decl_printable_name) (current_function_decl, 2));
      output_add_newline (buffer);

      record_last_error_function ();
      output_destroy_prefix (buffer);
      output_buffer_state (buffer) = os;
    }
}

/* Returns a description of FUNCTION using standard terminology.  */
static const char *
function_category (fn)
     tree fn;
{
  if (DECL_FUNCTION_MEMBER_P (fn))
    {
      if (DECL_STATIC_FUNCTION_P (fn))
        return "static member function";
      else if (DECL_COPY_CONSTRUCTOR_P (fn))
        return "copy constructor";
      else if (DECL_CONSTRUCTOR_P (fn))
        return "constructor";
      else if (DECL_DESTRUCTOR_P (fn))
        return "destructor";
      else
        return "member function";
    }
  else
    return "function";
}

/* Report the full context of a current template instantiation,
   onto BUFFER.  */
static void
print_instantiation_full_context (buffer)
     output_buffer *buffer;
{
  tree p = current_instantiation ();
  int line = lineno;
  const char *file = input_filename;

  if (p)
    {
      if (current_function_decl != TINST_DECL (p)
	  && current_function_decl != NULL_TREE)
	/* We can get here during the processing of some synthesized
	   method.  Then, TINST_DECL (p) will be the function that's causing
	   the synthesis.  */
	;
      else
	{
	  if (current_function_decl == TINST_DECL (p))
	    /* Avoid redundancy with the the "In function" line.  */;
	  else
	    output_verbatim (buffer, "%s: In instantiation of `%s':\n", file,
                             decl_as_string (TINST_DECL (p),
                                             TFF_DECL_SPECIFIERS | TFF_RETURN_TYPE));

	  line = TINST_LINE (p);
	  file = TINST_FILE (p);
	  p = TREE_CHAIN (p);
	}
    }

  print_instantiation_partial_context (buffer, p, file, line);
}

/* Same as above but less verbose.  */
static void
print_instantiation_partial_context (buffer, t, file, line)
     output_buffer *buffer;
     tree t;
     const char *file;
     int line;
{
  for (; t; t = TREE_CHAIN (t))
    {
      output_verbatim
        (buffer, "%s:%d:   instantiated from `%s'\n", file, line,
         decl_as_string (TINST_DECL (t), TFF_DECL_SPECIFIERS | TFF_RETURN_TYPE));
      line = TINST_LINE (t);
      file = TINST_FILE (t);
    }
  output_verbatim (buffer, "%s:%d:   instantiated from here\n", file, line);
}

/* Called from cp_thing to print the template context for an error.  */
static void
maybe_print_instantiation_context (buffer)
     output_buffer *buffer;
{
  if (!problematic_instantiation_changed () || current_instantiation () == 0)
    return;

  record_last_problematic_instantiation ();
  print_instantiation_full_context (buffer);
}

/* Report the bare minimum context of a template instantiation.  */
void
print_instantiation_context ()
{
  print_instantiation_partial_context
    (diagnostic_buffer, current_instantiation (), input_filename, lineno);
  flush_diagnostic_buffer ();
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
   %T   type.
   %V   cv-qualifier.  */
static int
cp_printer (buffer)
     output_buffer *buffer;
{
  int verbose = 0;
  const char *result;
#define next_tree    va_arg (output_buffer_format_args (buffer), tree)
#define next_tcode   va_arg (output_buffer_format_args (buffer), enum tree_code)
#define next_lang    va_arg (output_buffer_format_args (buffer), enum languages)
#define next_int     va_arg (output_buffer_format_args (buffer), int)

  if (*output_buffer_text_cursor (buffer) == '+')
    ++output_buffer_text_cursor (buffer);
  if (*output_buffer_text_cursor (buffer) == '#')
    {
      verbose = 1;
      ++output_buffer_text_cursor (buffer);
    }

  switch (*output_buffer_text_cursor (buffer))
    {
    case 'A': result = args_to_string (next_tree, verbose);	break;
    case 'C': result = code_to_string (next_tcode, verbose);	break;
    case 'D': result = decl_to_string (next_tree, verbose);	break;
    case 'E': result = expr_to_string (next_tree, verbose);	break;
    case 'F': result = fndecl_to_string (next_tree, verbose);	break;
    case 'L': result = language_to_string (next_lang, verbose); break;
    case 'O': result = op_to_string (next_tcode, verbose);	break;
    case 'P': result = parm_to_string (next_int, verbose);	break;
    case 'Q': result = assop_to_string (next_tcode, verbose);	break;
    case 'T': result = type_to_string (next_tree, verbose);	break;
    case 'V': result = cv_to_string (next_tree, verbose);	break;
 
    default:
      return 0;
    }

  output_add_string (buffer, result);
  return 1;
#undef next_tree
#undef next_tcode
#undef next_lang
#undef next_int
}

static void
print_integer (buffer, i)
     output_buffer *buffer;
     HOST_WIDE_INT i;
{
  sprintf (digit_buffer, HOST_WIDE_INT_PRINT_DEC, (HOST_WIDE_INT) i);
  output_add_string (buffer, digit_buffer);
}

static void
print_non_consecutive_character (buffer, c)
     output_buffer *buffer;
     int c;
{
  const char *p = output_last_position (buffer);

  if (p != NULL && *p == c)
    output_add_space (buffer);
  output_add_character (buffer, c);
}

/* These are temporary wrapper functions which handle the historic
   behavior of cp_*_at.  */

static tree
locate_error (msgid, ap)
     const char *msgid;
     va_list ap;
{
  tree here = 0, t;
  int plus = 0;
  const char *f;

  for (f = msgid; *f; f++)
    {
      plus = 0;
      if (*f == '%')
	{
	  f++;
	  if (*f == '+')
	    f++, plus = 1;
	  if (*f == '#')
	    f++;

	  switch (*f)
	    {
	      /* Just ignore these possibilities.  */
	    case '%':						break;
	    case 'd':	(void) va_arg (ap, int);		break;
	    case 's':	(void) va_arg (ap, char *);		break;
	    case 'L':	(void) va_arg (ap, enum languages);	break;
	    case 'C':
	    case 'O':
	    case 'Q':	(void) va_arg (ap, enum tree_code);	break;

	      /* These take a tree, which may be where the error is
		 located.  */
	    case 'A':
	    case 'D':
	    case 'E':
	    case 'F':
	    case 'P':
	    case 'T':
	    case 'V':
	      t = va_arg (ap, tree);
	      if (!here || plus)
		here = t;
	      break;

	    default:
	      errorcount = 0;  /* damn ICE suppression */
	      internal_error ("unexpected letter `%c' in locate_error\n", *f);
	    }
	}
    }

  if (here == 0)
    here = va_arg (ap, tree);

  return here;
}


void
cp_error_at VPARAMS ((const char *msgid, ...))
{
  tree here;
  diagnostic_context dc;

  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, const char *, msgid);
  here = locate_error (msgid, ap);
  VA_CLOSE (ap);

  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, const char *, msgid);

  set_diagnostic_context (&dc, msgid, &ap,
			  cp_file_of (here),
			  cp_line_of (here), /* warning = */ 0);
  report_diagnostic (&dc);
  VA_CLOSE (ap);
}

void
cp_warning_at VPARAMS ((const char *msgid, ...))
{
  tree here;
  diagnostic_context dc;

  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, const char *, msgid);
  here = locate_error (msgid, ap);
  VA_CLOSE (ap);

  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, const char *, msgid);

  set_diagnostic_context (&dc, msgid, &ap,
			  cp_file_of (here),
			  cp_line_of (here), /* warning = */ 1);
  report_diagnostic (&dc);
  VA_CLOSE (ap);
}

void
cp_pedwarn_at VPARAMS ((const char *msgid, ...))
{
  tree here;
  diagnostic_context dc;

  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, const char *, msgid);
  here = locate_error (msgid, ap);
  VA_CLOSE (ap);

  VA_OPEN (ap, msgid);
  VA_FIXEDARG (ap, const char *, msgid);

  set_diagnostic_context (&dc, msgid, &ap,
			  cp_file_of (here),
			  cp_line_of (here),
			  /* warning = */ !flag_pedantic_errors);
  report_diagnostic (&dc);
  VA_CLOSE (ap);
}
