/* Call-backs for C++ error reporting.
   This code is non-reentrant.
   Copyright (C) 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000
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
#include "obstack.h"
#include "toplev.h"

typedef const char *cp_printer ();

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

/* Obstack where we build text strings for overloading, etc.  */
static struct obstack scratch_obstack;
static char *scratch_firstobj;

# define OB_INIT() (scratch_firstobj ? (obstack_free (&scratch_obstack, scratch_firstobj), 0) : 0)
# define OB_PUTC(C) (obstack_1grow (&scratch_obstack, (C)))
# define OB_PUTC2(C1,C2)	\
  (obstack_1grow (&scratch_obstack, (C1)), obstack_1grow (&scratch_obstack, (C2)))
# define OB_PUTS(S) (obstack_grow (&scratch_obstack, (S), sizeof (S) - 1))
# define OB_PUTID(ID)  \
  (obstack_grow (&scratch_obstack, IDENTIFIER_POINTER (ID),	\
		 IDENTIFIER_LENGTH (ID)))
# define OB_PUTCP(S) (obstack_grow (&scratch_obstack, (S), strlen (S)))
# define OB_FINISH() (obstack_1grow (&scratch_obstack, '\0'))
# define OB_PUTI(CST) do { sprintf (digit_buffer, HOST_WIDE_INT_PRINT_DEC, (HOST_WIDE_INT)(CST)); \
			   OB_PUTCP (digit_buffer); } while (0)

# define OB_END_TEMPLATE_ID() 						    \
  (((obstack_next_free (&scratch_obstack) != obstack_base (&scratch_obstack) \
    && obstack_next_free (&scratch_obstack)[-1] == '>')			    \
   ? OB_PUTC (' ') : (void)0), OB_PUTC ('>'))

# define NEXT_CODE(t) (TREE_CODE (TREE_TYPE (t)))

enum pad { none, before, after };

static const char *args_to_string		PARAMS ((tree, int));
static const char *assop_to_string		PARAMS ((enum tree_code, int));
static const char *code_to_string		PARAMS ((enum tree_code, int));
static const char *cv_to_string			PARAMS ((tree, int));
static const char *decl_to_string		PARAMS ((tree, int));
static const char *expr_to_string		PARAMS ((tree, int));
static const char *fndecl_to_string		PARAMS ((tree, int));
static const char *language_to_string		PARAMS ((enum languages, int));
static const char *op_to_string			PARAMS ((enum tree_code, int));
static const char *parm_to_string		PARAMS ((int, int));
static const char *type_to_string		PARAMS ((tree, int));

static void dump_type PARAMS ((tree, enum tree_string_flags));
static void dump_simple_decl PARAMS ((tree, tree, enum tree_string_flags));
static void dump_decl PARAMS ((tree, enum tree_string_flags));
static void dump_template_decl PARAMS ((tree, enum tree_string_flags));
static void dump_function_decl PARAMS ((tree, enum tree_string_flags));
static void dump_expr PARAMS ((tree, enum tree_string_flags));
static void dump_unary_op PARAMS ((const char *, tree, enum tree_string_flags));
static void dump_binary_op PARAMS ((const char *, tree, enum tree_string_flags));
static void dump_aggr_type PARAMS ((tree, enum tree_string_flags));
static enum pad dump_type_prefix PARAMS ((tree, enum tree_string_flags));
static void dump_type_suffix PARAMS ((tree, enum tree_string_flags));
static void dump_function_name PARAMS ((tree, enum tree_string_flags));
static void dump_expr_list PARAMS ((tree, enum tree_string_flags));
static void dump_global_iord PARAMS ((tree));
static enum pad dump_qualifiers PARAMS ((tree, enum pad));
static void dump_char PARAMS ((int));
static void dump_parameters PARAMS ((tree, enum tree_string_flags));
static void dump_exception_spec PARAMS ((tree, enum tree_string_flags));
static const char *aggr_variety PARAMS ((tree));
static tree ident_fndecl PARAMS ((tree));
static void dump_template_argument PARAMS ((tree, enum tree_string_flags));
static void dump_template_argument_list PARAMS ((tree, enum tree_string_flags));
static void dump_template_parameter PARAMS ((tree, enum tree_string_flags));
static void dump_template_bindings PARAMS ((tree, tree));
static void dump_scope PARAMS ((tree, enum tree_string_flags));
static void dump_template_parms PARAMS ((tree, int, enum tree_string_flags));

#define A args_to_string
#define C code_to_string
#define D decl_to_string
#define E expr_to_string
#define F fndecl_to_string
#define L language_to_string
#define O op_to_string
#define P parm_to_string
#define Q assop_to_string
#define T type_to_string
#define V cv_to_string

#define o (cp_printer *) 0
cp_printer * cp_printers[256] =
{
/*0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F */
  o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, /* 0x00 */
  o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, /* 0x10 */
  o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, /* 0x20 */
  o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, /* 0x30 */
  o, A, o, C, D, E, F, o, o, o, o, o, L, o, o, O, /* 0x40 */
  P, Q, o, o, T, o, V, o, o, o, o, o, o, o, o, o, /* 0x50 */
  o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, /* 0x60 */
  o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, o, /* 0x70 */
};
#undef C
#undef D
#undef E
#undef F
#undef L
#undef O
#undef P
#undef Q
#undef T
#undef V
#undef o

void
init_error ()
{
  gcc_obstack_init (&scratch_obstack);
  scratch_firstobj = (char *)obstack_alloc (&scratch_obstack, 0);
}

/* Dump a scope, if deemed necessary.  */

static void
dump_scope (scope, flags)
     tree scope;
     enum tree_string_flags flags;
{
  if (scope == NULL_TREE)
    return;
  
  if (TREE_CODE (scope) == NAMESPACE_DECL)
    {
      if (scope != global_namespace)
        {
          dump_decl (scope, (flags & (TS_PEDANTIC_NAME | TS_FUNC_SCOPE | TS_CHASE_TYPEDEFS))
                             | TS_FUNC_NORETURN | TS_DECL_TYPE);
          OB_PUTS ("::");
        }
      else if (flags & TS_PEDANTIC_NAME)
        OB_PUTS ("::");
    }
  else if (AGGREGATE_TYPE_P (scope))
    {
      dump_type (scope, (flags & (TS_PEDANTIC_NAME | TS_FUNC_SCOPE | TS_CHASE_TYPEDEFS))
                           | TS_FUNC_NORETURN | TS_DECL_TYPE);
      OB_PUTS ("::");
    }
  else if ((flags & (TS_PEDANTIC_NAME | TS_FUNC_SCOPE))
            && TREE_CODE (scope) == FUNCTION_DECL)
    {
      dump_function_decl (scope, (flags & (TS_PEDANTIC_NAME | TS_FUNC_SCOPE | TS_CHASE_TYPEDEFS))
                           | TS_FUNC_NORETURN | TS_DECL_TYPE);
      OB_PUTS ("::");
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
              OB_PUTC (' ');
            p = before;
            OB_PUTCP (names[ix]);
          }
      if (do_after)
        OB_PUTC (' ');
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
     enum tree_string_flags flags;
{
  if (TREE_CODE_CLASS (TREE_CODE (arg)) == 't'
      || TREE_CODE (arg) == TEMPLATE_DECL)
    dump_type (arg, flags & ~TS_AGGR_TAGS);
  else
    dump_expr (arg, (flags | TS_EXPR_PARENS) & ~TS_AGGR_TAGS);
}

/* Dump a template-argument-list ARGS (always a TREE_VEC) under control
   of FLAGS.  */

static void
dump_template_argument_list (args, flags)
     tree args;
     enum tree_string_flags flags;
{
  int n = TREE_VEC_LENGTH (args);
  int need_comma = 0;
  int i;

  for (i = 0; i< n; ++i)
    {
      if (need_comma)
        OB_PUTS (", ");
      dump_template_argument (TREE_VEC_ELT (args, i), flags);
      need_comma = 1;
    }
}

/* Dump a template parameter PARM (a TREE_LIST) under control of FLAGS.  */

static void
dump_template_parameter (parm, flags)
     tree parm;
     enum tree_string_flags flags;
{
  tree p = TREE_VALUE (parm);
  tree a = TREE_PURPOSE (parm);

  if (TREE_CODE (p) == TYPE_DECL)
    {
      if (flags & TS_DECL_TYPE)
        {
          OB_PUTS ("class");
          if (DECL_NAME (p))
            {
              OB_PUTC (' ');
              OB_PUTID (DECL_NAME (p));
            }
        }
      else if (DECL_NAME (p))
        OB_PUTID (DECL_NAME (p));
      else
        OB_PUTS ("{template default argument error}");
    }
  else
    dump_decl (p, flags | TS_DECL_TYPE);

  if ((flags & TS_PARM_DEFAULTS) && a != NULL_TREE)
    {
      OB_PUTS (" = ");
      if (TREE_CODE (a) == TYPE_DECL || TREE_CODE (a) == TEMPLATE_DECL)
        dump_type (a, flags & ~TS_CHASE_TYPEDEFS);
      else
        dump_expr (a, flags | TS_EXPR_PARENS);
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
	  tree arg = TMPL_ARG (args, lvl, arg_idx);

	  if (need_comma)
	    OB_PUTS (", ");
	  dump_template_parameter (TREE_VEC_ELT (p, i), TS_PLAIN);
	  OB_PUTS (" = ");
	  if (arg)
	    dump_template_argument (arg, TS_PLAIN);
	  else
	    OB_PUTS ("{missing}");
          
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
     enum tree_string_flags flags;
{
  if (t == NULL_TREE)
    return;
  
  if (TYPE_PTRMEMFUNC_P (t))
    goto offset_type;

  switch (TREE_CODE (t))
    {
    case UNKNOWN_TYPE:
      OB_PUTS ("{unknown type}");
      break;

    case TREE_LIST:
      /* A list of function parms.  */
      dump_parameters (t, flags);
      break;

    case IDENTIFIER_NODE:
      OB_PUTID (t);
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
      if (flags & TS_CHASE_TYPEDEFS)
        {
          dump_type (DECL_ORIGINAL_TYPE (t)
                     ? DECL_ORIGINAL_TYPE (t) : TREE_TYPE (t), flags);
          break;
        }
      /* else fallthrough */
    
    case TEMPLATE_DECL:
    case NAMESPACE_DECL:
      dump_decl (t, flags & ~TS_DECL_TYPE);
      break;
    
    case COMPLEX_TYPE:
      OB_PUTS ("complex ");
      dump_type (TREE_TYPE (t), flags);
      break;

    case INTEGER_TYPE:
      if (!TREE_UNSIGNED (TYPE_MAIN_VARIANT (t)) && TREE_UNSIGNED (t))
	OB_PUTS ("unsigned ");
      else if (TREE_UNSIGNED (TYPE_MAIN_VARIANT (t)) && !TREE_UNSIGNED (t))
	OB_PUTS ("signed ");

      /* fall through.  */
    case REAL_TYPE:
    case VOID_TYPE:
    case BOOLEAN_TYPE:
      {
	tree type;
	dump_qualifiers (t, after);
	type = flags & TS_CHASE_TYPEDEFS ? TYPE_MAIN_VARIANT (t) : t;
	if (TYPE_NAME (type) && TYPE_IDENTIFIER (type))
	  OB_PUTID (TYPE_IDENTIFIER (type));
	else
	  /* Types like intQI_type_node and friends have no names.
	     These don't come up in user error messages, but it's nice
	     to be able to print them from the debugger.  */
	  OB_PUTS ("{anonymous}");
      }
      break;

    case TEMPLATE_TEMPLATE_PARM:
      if (!TEMPLATE_TEMPLATE_PARM_TEMPLATE_INFO (t))
	{
	  /* For parameters inside template signature. */
	  if (TYPE_IDENTIFIER (t))
	    OB_PUTID (TYPE_IDENTIFIER (t));
	  else
	    OB_PUTS ("{anonymous template template parameter}");
	}
      else
	{
	  tree args = TYPE_TI_ARGS (t);
	  OB_PUTID (TYPE_IDENTIFIER (t));
	  OB_PUTC ('<');
          dump_template_argument_list (args, flags);
	  OB_END_TEMPLATE_ID ();
	}
      break;

    case TEMPLATE_TYPE_PARM:
      dump_qualifiers (t, after);
      if (TYPE_IDENTIFIER (t))
	OB_PUTID (TYPE_IDENTIFIER (t));
      else
	OB_PUTS ("{anonymous template type parameter}");
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
      OB_PUTS ("typename ");
      dump_type (TYPE_CONTEXT (t), flags);
      OB_PUTS ("::");
      dump_decl (TYPENAME_TYPE_FULLNAME (t), flags);
      break;

    case TYPEOF_TYPE:
      OB_PUTS ("__typeof (");
      dump_expr (TYPE_FIELDS (t), flags & ~TS_EXPR_PARENS);
      OB_PUTC (')');
      break;

    default:
      sorry ("`%s' not supported by dump_type",
	     tree_code_name[(int) TREE_CODE (t)]);
      /* Fall through to error. */

    case ERROR_MARK:
      OB_PUTS ("{type error}");
      break;
    }
}

/* Return the name of the supplied aggregate, or enumeral type.  */

static const char *
aggr_variety (t)
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
     enum tree_string_flags flags;
{
  tree name;
  const char *variety = aggr_variety (t);
  int typdef = 0;
  int tmplate = 0;

  dump_qualifiers (t, after);

  if (flags & TS_AGGR_TAGS)
    {
      OB_PUTCP (variety);
      OB_PUTC (' ');
    }
  
  if (flags & TS_CHASE_TYPEDEFS)
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
      dump_scope (CP_DECL_CONTEXT (name), flags | TS_FUNC_SCOPE);
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
      OB_PUTS ("{anonymous");
      if (!(flags & TS_AGGR_TAGS))
	{
	  OB_PUTC (' ');
	  OB_PUTCP (variety);
	}
      OB_PUTC ('}');
    }
  else
    OB_PUTID (name);
  if (tmplate)
    dump_template_parms (TYPE_TEMPLATE_INFO (t),
                         !CLASSTYPE_USE_TEMPLATE (t),
                         flags & ~TS_TEMPLATE_PREFIX);
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
     enum tree_string_flags flags;
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
	    if (padding != none)
	      OB_PUTC (' ');
	    if (TREE_CODE (sub) == ARRAY_TYPE)
	      OB_PUTC ('(');
            OB_PUTC ("&*"[TREE_CODE (t) == POINTER_TYPE]);
	    padding = dump_qualifiers (t, none);
	  }
      }
      break;

    case OFFSET_TYPE:
    offset_type:
      padding = dump_type_prefix (TREE_TYPE (t), flags);
      if (TREE_CODE (t) == OFFSET_TYPE)	/* pmfs deal with this in d_t_p */
	{
	  if (padding != none)
	    OB_PUTC (' ');
	  dump_type (TYPE_OFFSET_BASETYPE (t), flags);
	  OB_PUTS ("::");
	}
      OB_PUTC ('*');
      padding = dump_qualifiers (t, none);
      break;

      /* Can only be reached through function pointer -- this would not be
         correct if FUNCTION_DECLs used it.  */
    case FUNCTION_TYPE:
      padding = dump_type_prefix (TREE_TYPE (t), flags);
      if (padding != none)
        OB_PUTC (' ');
      OB_PUTC ('(');
      padding = none;
      break;

    case METHOD_TYPE:
      padding = dump_type_prefix (TREE_TYPE (t), flags);
      if (padding != none)
        OB_PUTC (' ');
      OB_PUTC ('(');
      padding = none;
      dump_aggr_type (TYPE_METHOD_BASETYPE (t), flags);
      OB_PUTS ("::");
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
    case TREE_LIST:
    case TYPE_DECL:
    case TREE_VEC:
    case UNION_TYPE:
    case UNKNOWN_TYPE:
    case VOID_TYPE:
    case TYPENAME_TYPE:
    case COMPLEX_TYPE:
      dump_type (t, flags);
      padding = before;
      break;
      
    default:
      sorry ("`%s' not supported by dump_type_prefix",
	     tree_code_name[(int) TREE_CODE (t)]);

    case ERROR_MARK:
      OB_PUTS ("{typeprefixerror}");
      break;
    }
  return padding;
}

/* Dump the suffix of type T, under control of FLAGS.  This is the part
   which appears after the identifier (or function parms).  */

static void
dump_type_suffix (t, flags)
     tree t;
     enum tree_string_flags flags;
{
  if (TYPE_PTRMEMFUNC_P (t))
    t = TYPE_PTRMEMFUNC_FN_TYPE (t);

  switch (TREE_CODE (t))
    {
    case POINTER_TYPE:
    case REFERENCE_TYPE:
    case OFFSET_TYPE:
      if (TREE_CODE (TREE_TYPE (t)) == ARRAY_TYPE)
	OB_PUTC (')');
      dump_type_suffix (TREE_TYPE (t), flags);
      break;

      /* Can only be reached through function pointer */
    case FUNCTION_TYPE:
    case METHOD_TYPE:
      {
	tree arg;
	OB_PUTC (')');
	arg = TYPE_ARG_TYPES (t);
	if (TREE_CODE (t) == METHOD_TYPE)
	  arg = TREE_CHAIN (arg);

	/* Function pointers don't have default args.  Not in standard C++,
	   anyway; they may in g++, but we'll just pretend otherwise.  */
	dump_parameters (arg, flags & ~TS_PARM_DEFAULTS);

	if (TREE_CODE (t) == METHOD_TYPE)
	  dump_qualifiers
	    (TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (t))), before);
	dump_type_suffix (TREE_TYPE (t), flags);
	dump_exception_spec (TYPE_RAISES_EXCEPTIONS (t), flags);
	break;
      }

    case ARRAY_TYPE:
      OB_PUTC ('[');
      if (TYPE_DOMAIN (t))
	{
	  if (host_integerp (TYPE_MAX_VALUE (TYPE_DOMAIN (t)), 0))
	    OB_PUTI (tree_low_cst (TYPE_MAX_VALUE (TYPE_DOMAIN (t)), 0) + 1);
	  else if (TREE_CODE (TYPE_MAX_VALUE (TYPE_DOMAIN (t))) == MINUS_EXPR)
	    dump_expr (TREE_OPERAND (TYPE_MAX_VALUE (TYPE_DOMAIN (t)), 0),
	               flags & ~TS_EXPR_PARENS);
	  else
	    dump_expr (fold (build_binary_op
			     (PLUS_EXPR, TYPE_MAX_VALUE (TYPE_DOMAIN (t)),
			      integer_one_node)),
	               flags & ~TS_EXPR_PARENS);
	}
      OB_PUTC (']');
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
    case TREE_LIST:
    case TYPE_DECL:
    case TREE_VEC:
    case UNION_TYPE:
    case UNKNOWN_TYPE:
    case VOID_TYPE:
    case TYPENAME_TYPE:
    case COMPLEX_TYPE:
      break;

    default:
      sorry ("`%s' not supported by dump_type_suffix",
	     tree_code_name[(int) TREE_CODE (t)]);

    case ERROR_MARK:
      /* Don't mark it here, we should have already done in
         dump_type_prefix.  */
      break;
    }
}

/* Return a function declaration which corresponds to the IDENTIFIER_NODE
   argument.  */

static tree
ident_fndecl (t)
     tree t;
{
  tree n = lookup_name (t, 0);

  if (n == NULL_TREE)
    return NULL_TREE;

  if (TREE_CODE (n) == FUNCTION_DECL)
    return n;
  else if (TREE_CODE (n) == TREE_LIST
	   && TREE_CODE (TREE_VALUE (n)) == FUNCTION_DECL)
    return TREE_VALUE (n);

  my_friendly_abort (66);
  return NULL_TREE;
}

#ifndef NO_DOLLAR_IN_LABEL
#  define GLOBAL_THING "_GLOBAL_$"
#else
#  ifndef NO_DOT_IN_LABEL
#    define GLOBAL_THING "_GLOBAL_."
#  else
#    define GLOBAL_THING "_GLOBAL__"
#  endif
#endif

#define GLOBAL_IORD_P(NODE) \
  ! strncmp (IDENTIFIER_POINTER(NODE), GLOBAL_THING, sizeof (GLOBAL_THING) - 1)

static void
dump_global_iord (t)
     tree t;
{
  const char *name = IDENTIFIER_POINTER (t);

  OB_PUTS ("(static ");
  if (name [sizeof (GLOBAL_THING) - 1] == 'I')
    OB_PUTS ("initializers");
  else if (name [sizeof (GLOBAL_THING) - 1] == 'D')
    OB_PUTS ("destructors");
  else
    my_friendly_abort (352);
  
  OB_PUTS (" for ");
  OB_PUTCP (input_filename);
  OB_PUTC (')');
}

static void
dump_simple_decl (t, type, flags)
     tree t;
     tree type;
     enum tree_string_flags flags;
{
  if (flags & TS_DECL_TYPE)
    {
      if (dump_type_prefix (type, flags) != none)
        OB_PUTC (' ');
    }
  if (!DECL_INITIAL (t) || TREE_CODE (DECL_INITIAL (t)) != TEMPLATE_PARM_INDEX)
    dump_scope (CP_DECL_CONTEXT (t), flags);
  if (DECL_NAME (t))
    dump_decl (DECL_NAME (t), flags);
  else
    OB_PUTS ("{anonymous}");
  if (flags & TS_DECL_TYPE)
    dump_type_suffix (type, flags);
}

/* Dump a human readable string for the decl T under control of FLAGS.  */

static void
dump_decl (t, flags)
     tree t;
     enum tree_string_flags flags;
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
	    if ((flags & TS_DECL_TYPE)
	        && TREE_CODE (TREE_TYPE (t)) == TEMPLATE_TYPE_PARM)
	      /* Say `class T' not just `T'. */
	      OB_PUTS ("class ");

	    dump_type (TREE_TYPE (t), flags);
	    break;
	  }
      }
      if (flags & TS_DECORATE)
	OB_PUTS ("typedef ");
      dump_simple_decl (t, DECL_ORIGINAL_TYPE (t) 
			? DECL_ORIGINAL_TYPE (t) : TREE_TYPE (t),
	                flags);
      break;
      
    case VAR_DECL:
      if (DECL_NAME (t) && VTABLE_NAME_P (DECL_NAME (t)))
	{
	  OB_PUTS ("vtable for ");
	  if (TYPE_P (DECL_CONTEXT (t)))
	    dump_type (DECL_CONTEXT (t), flags);
	  else
	    /* This case can arise with -fno-vtable-thunks.  See
	       expand_upcast_fixups.  It's not clear what to print
	       here.  */
	    OB_PUTS ("{unknown type}");
	  break;
	}
      /* else fall through */
    case FIELD_DECL:
    case PARM_DECL:
      dump_simple_decl (t, TREE_TYPE (t), flags);
      break;

    case RESULT_DECL:
      OB_PUTS ("{return} ");
      dump_simple_decl (t, TREE_TYPE (t), flags);
      break;

    case NAMESPACE_DECL:
      dump_scope (CP_DECL_CONTEXT (t), flags);
      if (DECL_NAME (t) == anonymous_namespace_name)
	OB_PUTS ("{unnamed}");
      else
	OB_PUTID (DECL_NAME (t));
      break;

    case SCOPE_REF:
      dump_decl (TREE_OPERAND (t, 0), flags & ~TS_DECL_TYPE);
      OB_PUTS ("::");
      dump_decl (TREE_OPERAND (t, 1), flags);
      break;      

    case ARRAY_REF:
      dump_decl (TREE_OPERAND (t, 0), flags);
      OB_PUTC ('[');
      dump_decl (TREE_OPERAND (t, 1), flags);
      OB_PUTC (']');
      break;

      /* So that we can do dump_decl on an aggr type.  */
    case RECORD_TYPE:
    case UNION_TYPE:
    case ENUMERAL_TYPE:
      dump_type (t, flags);
      break;

    case TYPE_EXPR:
      my_friendly_abort (69);
      break;

      /* These special cases are duplicated here so that other functions
	 can feed identifiers to cp_error and get them demangled properly.  */
    case IDENTIFIER_NODE:
      { tree f;
	if (DESTRUCTOR_NAME_P (t)
	    && (f = ident_fndecl (t))
	    && DECL_LANGUAGE (f) == lang_cplusplus)
	  {
	    OB_PUTC ('~');
	    dump_decl (DECL_NAME (f), flags);
	  }
	else if (IDENTIFIER_TYPENAME_P (t))
	  {
	    OB_PUTS ("operator ");
	    /* Not exactly IDENTIFIER_TYPE_VALUE.  */
	    dump_type (TREE_TYPE (t), flags);
	    break;
	  }
	else if (IDENTIFIER_OPNAME_P (t))
	  {
	    const char *name_string = operator_name_string (t);
	    OB_PUTS ("operator");
	    if (ISALPHA (name_string[0]))
	      OB_PUTC (' ');
	    OB_PUTCP (name_string);
	  }
	else
	  OB_PUTID (t);
      }
      break;

    case OVERLOAD:
      t = OVL_CURRENT (t);
      /* Fall through.  */

    case FUNCTION_DECL:
      if (GLOBAL_IORD_P (DECL_ASSEMBLER_NAME (t)))
	dump_global_iord (DECL_ASSEMBLER_NAME (t));
      else if (! DECL_LANG_SPECIFIC (t))
	OB_PUTS ("{internal}");
      else if (flags & TS_PEDANTIC_NAME)
        dump_function_decl (t, flags | TS_FUNC_NORETURN | TS_DECL_TYPE);
      else
        dump_function_decl (t, flags);
      break;

    case TEMPLATE_DECL:
      if (flags & TS_PEDANTIC_NAME)
        dump_template_decl (t, flags | TS_FUNC_NORETURN | TS_DECL_TYPE);
      else
        dump_template_decl (t, flags);
      break;

    case TEMPLATE_ID_EXPR:
      {
	tree args;
	tree name = TREE_OPERAND (t, 0);
	if (is_overloaded_fn (name))
	  name = DECL_NAME (get_first_fn (name));
	dump_decl (name, flags);
	OB_PUTC ('<');
	for (args = TREE_OPERAND (t, 1); args; args = TREE_CHAIN (args))
	  {
	    dump_template_argument (TREE_VALUE (args), flags);
	    if (TREE_CHAIN (args))
	      OB_PUTS (", ");
	  }
	OB_END_TEMPLATE_ID ();
      }
      break;

    case LOOKUP_EXPR:
      dump_decl (TREE_OPERAND (t, 0), flags);
      break;

    case LABEL_DECL:
      OB_PUTID (DECL_NAME (t));
      break;

    case CONST_DECL:
      if ((TREE_TYPE (t) != NULL_TREE && NEXT_CODE (t) == ENUMERAL_TYPE)
	  || (DECL_INITIAL (t) &&
	      TREE_CODE (DECL_INITIAL (t)) == TEMPLATE_PARM_INDEX))
	dump_simple_decl (t, TREE_TYPE (t), flags);
      else if (DECL_NAME (t))
	dump_decl (DECL_NAME (t), flags);
      else if (DECL_INITIAL (t))
	dump_expr (DECL_INITIAL (t), flags | TS_EXPR_PARENS);
      else
	OB_PUTS ("enumerator");
      break;

    case USING_DECL:
      OB_PUTS ("using ");
      dump_type (DECL_INITIAL (t), flags);
      OB_PUTS ("::");
      OB_PUTID (DECL_NAME (t));
      break;

    default:
      sorry ("`%s' not supported by dump_decl",
	     tree_code_name[(int) TREE_CODE (t)]);
      /* Fallthrough to error.  */

    case ERROR_MARK:
      OB_PUTS ("{declaration error}");
      break;
    }
}

/* Dump a template declaration T under control of FLAGS. This means the
   'template <...> leaders plus the 'class X' or 'void fn(...)' part.  */

static void
dump_template_decl (t, flags)
     tree t;
     enum tree_string_flags flags;
{
  tree orig_parms = DECL_TEMPLATE_PARMS (t);
  tree parms;
  int i; 
  
  if (flags & TS_TEMPLATE_PREFIX)
    {
      for (parms = orig_parms = nreverse (orig_parms); 
           parms;
           parms = TREE_CHAIN (parms))
        {
	  tree inner_parms = INNERMOST_TEMPLATE_PARMS (parms);
          int len = TREE_VEC_LENGTH (inner_parms);
          
          OB_PUTS ("template <");
          for (i = 0; i < len; i++)
            {
              if (i)
                OB_PUTS (", ");
              dump_template_parameter (TREE_VEC_ELT (inner_parms, i), flags);
            }
          OB_END_TEMPLATE_ID ();
          OB_PUTC (' ');
        }
      nreverse(orig_parms);
      /* If we've shown the template<args> prefix, we'd better show the
	 decl's type too.  */
      flags |= TS_DECL_TYPE;
    }
  if (TREE_CODE (DECL_TEMPLATE_RESULT (t)) == TYPE_DECL)
    dump_type (TREE_TYPE (t),
               ((flags & ~TS_AGGR_TAGS) | TS_TEMPLATE_PLAIN
                | (flags & TS_DECL_TYPE ? TS_AGGR_TAGS : 0)));
  else if (TREE_CODE (DECL_TEMPLATE_RESULT (t)) == VAR_DECL)
    dump_decl (DECL_TEMPLATE_RESULT (t), flags | TS_TEMPLATE_PLAIN);
  else if (TREE_TYPE (t) == NULL_TREE)
    my_friendly_abort (353);
  else
    switch (NEXT_CODE (t))
    {
      case METHOD_TYPE:
      case FUNCTION_TYPE:
        dump_function_decl (t, flags | TS_TEMPLATE_PLAIN);
        break;
      default:
        /* This case can occur with some illegal code.  */
        dump_type (TREE_TYPE (t),
                   (flags & ~TS_AGGR_TAGS) | TS_TEMPLATE_PLAIN
                   | (flags & TS_DECL_TYPE ? TS_AGGR_TAGS : 0));
    }
}

/* Pretty print a function decl. There are several ways we want to print a
   function declaration. The TS_FUNC bits in FLAGS tells us how to behave.
   As cp_error can only apply the '#' flag once to give 0 and 1 for V, there
   is %D which doesn't print the throw specs, and %F which does. */

static void
dump_function_decl (t, flags)
     tree t;
     enum tree_string_flags flags;
{
  tree fntype;
  tree parmtypes;
  tree cname = NULL_TREE;
  tree template_args = NULL_TREE;
  tree template_parms = NULL_TREE;
  int show_return = !(flags & TS_FUNC_NORETURN) && (flags & TS_DECL_TYPE);

  if (TREE_CODE (t) == TEMPLATE_DECL)
    t = DECL_TEMPLATE_RESULT (t);

  /* Pretty print template instantiations only.  */
  if (DECL_USE_TEMPLATE (t) && DECL_TEMPLATE_INFO (t))
    {
      template_args = DECL_TI_ARGS (t);
      t = most_general_template (t);
      if (TREE_CODE (t) == TEMPLATE_DECL)
	template_parms = DECL_TEMPLATE_PARMS (t);
    }

  fntype = TREE_TYPE (t);
  parmtypes = TYPE_ARG_TYPES (fntype);

  if (DECL_CLASS_SCOPE_P (t))
    cname = DECL_CONTEXT (t);
  /* this is for partially instantiated template methods */
  else if (TREE_CODE (fntype) == METHOD_TYPE)
    cname = TREE_TYPE (TREE_VALUE (parmtypes));

  if (!(flags & TS_DECORATE))
    /* OK */;
  else if (DECL_STATIC_FUNCTION_P (t))
    OB_PUTS ("static ");
  else if (TYPE_POLYMORPHIC_P (t))
    OB_PUTS ("virtual ");
  
  /* Print the return type?  */
  if (show_return)
    show_return = !DECL_CONV_FN_P (t)  && !DECL_CONSTRUCTOR_P (t)
                  && !DECL_DESTRUCTOR_P (t);
  if (show_return)
    {
      if (dump_type_prefix (TREE_TYPE (fntype), flags) != none)
        OB_PUTC (' ');
    }

  /* Print the function name.  */
  if (cname)
    {
      dump_type (cname, flags);
      OB_PUTS ("::");
    }
  else
    dump_scope (CP_DECL_CONTEXT (t), flags);

  dump_function_name (t, flags);
  
  if (!(flags & TS_DECL_TYPE))
    return;
  if (TREE_CODE (fntype) == METHOD_TYPE && parmtypes)
    /* Skip "this" parameter.  */
    parmtypes = TREE_CHAIN (parmtypes);
    
  if (DECL_DESTRUCTOR_P (t) || DECL_CONSTRUCTOR_FOR_VBASE_P (t))
    /* Skip past "in_charge" identifier.  */
    parmtypes = TREE_CHAIN (parmtypes);
  
  dump_parameters (parmtypes, flags);
  
  if (show_return)
    dump_type_suffix (TREE_TYPE (fntype), flags);

  if (TREE_CODE (fntype) == METHOD_TYPE)
    dump_qualifiers (TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (fntype))),
		     before);
  
  if (flags & TS_FUNC_THROW)
    dump_exception_spec (TYPE_RAISES_EXCEPTIONS (fntype), flags);

  /* If T is a template instantiation, dump the parameter binding.  */
  if (template_parms != NULL_TREE && template_args != NULL_TREE)
    {
      OB_PUTS (" [with ");
      dump_template_bindings (template_parms, template_args);
      OB_PUTC (']');
    }
}

/* Print a parameter list. If this is for a member function, the
   member object ptr (and any other hidden args) should have
   already been removed. */

static void
dump_parameters (parmtypes, flags)
     tree parmtypes;
     enum tree_string_flags flags;
{
  int first;
  OB_PUTS (" (");

  for (first = 1; parmtypes != void_list_node;
       parmtypes = TREE_CHAIN (parmtypes))
    {
      if (!first)
        OB_PUTS (", ");
      first = 0;
      if (!parmtypes)
        {
          OB_PUTS ("...");
          break;
        }
      dump_type (TREE_VALUE (parmtypes), flags);
      
      if ((flags & TS_PARM_DEFAULTS) && TREE_PURPOSE (parmtypes))
        {
          OB_PUTS (" = ");
          dump_expr (TREE_PURPOSE (parmtypes), flags | TS_EXPR_PARENS);
        }
    }

  OB_PUTC (')');
}

/* Print an exception specification. T is the exception specification. */

static void
dump_exception_spec (t, flags)
     tree t;
     enum tree_string_flags flags;
{
  if (t)
    {
      OB_PUTS (" throw (");
      if (TREE_VALUE (t) != NULL_TREE)
        while (1)
          {
            dump_type (TREE_VALUE (t), flags);
            t = TREE_CHAIN (t);
            if (!t)
              break;
            OB_PUTS (", ");
          }
      OB_PUTC (')');
    }
}

/* Handle the function name for a FUNCTION_DECL node, grokking operators
   and destructors properly.  */

static void
dump_function_name (t, flags)
     tree t;
     enum tree_string_flags flags;
{
  tree name = DECL_NAME (t);

  if (DECL_DESTRUCTOR_P (t))
    {
      OB_PUTC ('~');
      dump_decl (name, TS_PLAIN);
    }
  else if (DECL_CONV_FN_P (t))
    {
      /* This cannot use the hack that the operator's return
	 type is stashed off of its name because it may be
	 used for error reporting.  In the case of conflicting
	 declarations, both will have the same name, yet
	 the types will be different, hence the TREE_TYPE field
	 of the first name will be clobbered by the second.  */
      OB_PUTS ("operator ");
      dump_type (TREE_TYPE (TREE_TYPE (t)), flags);
    }
  else if (IDENTIFIER_OPNAME_P (name))
    {
      const char *name_string = operator_name_string (name);
      OB_PUTS ("operator");
      if (ISALPHA (name_string[0]))
	OB_PUTC (' ');
      OB_PUTCP (name_string);
    }
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
     enum tree_string_flags flags;
{
  tree args = info ? TI_ARGS (info) : NULL_TREE;
  
  if (primary && flags & TS_TEMPLATE_PLAIN)
    return;
  flags &= ~(TS_AGGR_TAGS | TS_TEMPLATE_PLAIN);
  OB_PUTC ('<');

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
            OB_PUTS (", ");
              
          if (!arg)
            OB_PUTS ("{template parameter error}");
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
            OB_PUTS (", ");
          
          dump_decl (parm, flags & ~TS_DECL_TYPE);
        }
    }
  OB_END_TEMPLATE_ID ();
}

static void
dump_char (c)
     int c;
{
  switch (c)
    {
    case TARGET_NEWLINE:
      OB_PUTS ("\\n");
      break;
    case TARGET_TAB:
      OB_PUTS ("\\t");
      break;
    case TARGET_VT:
      OB_PUTS ("\\v");
      break;
    case TARGET_BS:
      OB_PUTS ("\\b");
      break;
    case TARGET_CR:
      OB_PUTS ("\\r");
      break;
    case TARGET_FF:
      OB_PUTS ("\\f");
      break;
    case TARGET_BELL:
      OB_PUTS ("\\a");
      break;
    case '\\':
      OB_PUTS ("\\\\");
      break;
    case '\'':
      OB_PUTS ("\\'");
      break;
    case '\"':
      OB_PUTS ("\\\"");
      break;
    default:
      if (ISPRINT (c))
	OB_PUTC (c);
      else
	{
	  sprintf (digit_buffer, "\\%03o", (int) c);
	  OB_PUTCP (digit_buffer);
	}
    }
}

/* Print out a list of initializers (subr of dump_expr) */

static void
dump_expr_list (l, flags)
     tree l;
     enum tree_string_flags flags;
{
  while (l)
    {
      dump_expr (TREE_VALUE (l), flags | TS_EXPR_PARENS);
      l = TREE_CHAIN (l);
      if (l)
	OB_PUTS (", ");
    }
}

/* Print out an expression E under control of FLAGS. */

static void
dump_expr (t, flags)
     tree t;
     enum tree_string_flags flags;
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
      dump_decl (t, flags & ~TS_DECL_TYPE);
      break;

    case INTEGER_CST:
      {
	tree type = TREE_TYPE (t);
	my_friendly_assert (type != 0, 81);

	/* If it's an enum, output its tag, rather than its value.  */
	if (TREE_CODE (type) == ENUMERAL_TYPE)
	  {
	    const char *p = enum_name_string (t, type);
	    OB_PUTCP (p);
	  }
	else if (type == boolean_type_node)
	  {
	    if (t == boolean_false_node || integer_zerop (t))
	      OB_PUTS ("false");
	    else if (t == boolean_true_node)
	      OB_PUTS ("true");
	  }
	else if (type == char_type_node)
	  {
	    OB_PUTC ('\'');
	    dump_char (tree_low_cst (t, 0));
	    OB_PUTC ('\'');
	  }
	else if ((unsigned HOST_WIDE_INT) TREE_INT_CST_HIGH (t)
		 != (TREE_INT_CST_LOW (t) >> (HOST_BITS_PER_WIDE_INT - 1)))
	  {
	    tree val = t;

	    if (tree_int_cst_sgn (val) < 0)
	      {
		OB_PUTC ('-');
		val = build_int_2 (~TREE_INT_CST_LOW (val),
				   -TREE_INT_CST_HIGH (val));
	      }
	    /* Would "%x%0*x" or "%x%*0x" get zero-padding on all
	       systems?  */
	    {
	      static char format[10]; /* "%x%09999x\0" */
	      if (!format[0])
		sprintf (format, "%%x%%0%dx", HOST_BITS_PER_INT / 4);
	      sprintf (digit_buffer, format, TREE_INT_CST_HIGH (val),
		       TREE_INT_CST_LOW (val));
	      OB_PUTCP (digit_buffer);
	    }
	  }
	else
	  OB_PUTI (TREE_INT_CST_LOW (t));
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
      OB_PUTCP (digit_buffer);
      break;

    case PTRMEM_CST:
      OB_PUTC ('&');
      dump_type (PTRMEM_CST_CLASS (t), flags);
      OB_PUTS ("::");
      OB_PUTID (DECL_NAME (PTRMEM_CST_MEMBER (t)));
      break;

    case STRING_CST:
      {
	const char *p = TREE_STRING_POINTER (t);
	int len = TREE_STRING_LENGTH (t) - 1;
	int i;

	OB_PUTC ('\"');
	for (i = 0; i < len; i++)
	  dump_char (p[i]);
	OB_PUTC ('\"');
      }
      break;

    case COMPOUND_EXPR:
      OB_PUTC ('(');
      dump_expr (TREE_OPERAND (t, 0), flags | TS_EXPR_PARENS);
      OB_PUTS (", ");
      dump_expr (TREE_OPERAND (t, 1), flags | TS_EXPR_PARENS);
      OB_PUTC (')');
      break;

    case COND_EXPR:
      OB_PUTC ('(');
      dump_expr (TREE_OPERAND (t, 0), flags | TS_EXPR_PARENS);
      OB_PUTS (" ? ");
      dump_expr (TREE_OPERAND (t, 1), flags | TS_EXPR_PARENS);
      OB_PUTS (" : ");
      dump_expr (TREE_OPERAND (t, 2), flags | TS_EXPR_PARENS);
      OB_PUTC (')');
      break;

    case SAVE_EXPR:
      if (TREE_HAS_CONSTRUCTOR (t))
	{
	  OB_PUTS ("new ");
	  dump_type (TREE_TYPE (TREE_TYPE (t)), flags);
	}
      else
	{
	  dump_expr (TREE_OPERAND (t, 0), flags | TS_EXPR_PARENS);
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
	      OB_PUTID (TYPE_IDENTIFIER (TREE_TYPE (t)));
	    else
	      dump_decl (fn, 0);
	  }
	else
	  dump_expr (TREE_OPERAND (t, 0), 0);
      }
      OB_PUTC ('(');
      if (TREE_OPERAND (t, 1))
	dump_expr_list (TREE_CHAIN (TREE_OPERAND (t, 1)), flags);
      OB_PUTC (')');
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
		dump_expr (TREE_OPERAND (ob, 0), flags | TS_EXPR_PARENS);
		OB_PUTC ('.');
	      }
	    else if (TREE_CODE (ob) != PARM_DECL
		     || strcmp (IDENTIFIER_POINTER (DECL_NAME (ob)), "this"))
	      {
		dump_expr (ob, flags | TS_EXPR_PARENS);
		OB_PUTS ("->");
	      }
	    args = TREE_CHAIN (args);
	  }
	dump_expr (fn, flags | TS_EXPR_PARENS);
	OB_PUTC ('(');
	dump_expr_list (args, flags);
	OB_PUTC (')');
      }
      break;

    case NEW_EXPR:
      {
	tree type = TREE_OPERAND (t, 1);
	if (NEW_EXPR_USE_GLOBAL (t))
	  OB_PUTS ("::");
	OB_PUTS ("new ");
	if (TREE_OPERAND (t, 0))
	  {
	    OB_PUTC ('(');
	    dump_expr_list (TREE_OPERAND (t, 0), flags);
	    OB_PUTS (") ");
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
	    OB_PUTC ('(');
	    dump_expr_list (TREE_OPERAND (t, 2), flags);
	    OB_PUTC (')');
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
	dump_expr (TREE_OPERAND (t, 1), flags | TS_EXPR_PARENS);
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
      dump_binary_op (opname_tab[(int) TREE_CODE (t)], t, flags);
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
		dump_expr (ob, flags | TS_EXPR_PARENS);
		OB_PUTS ("->");
	      }
	  }
	else
	  {
	    dump_expr (ob, flags | TS_EXPR_PARENS);
	    OB_PUTC ('.');
	  }
	dump_expr (TREE_OPERAND (t, 1), flags & ~TS_EXPR_PARENS);
      }
      break;

    case ARRAY_REF:
      dump_expr (TREE_OPERAND (t, 0), flags | TS_EXPR_PARENS);
      OB_PUTC ('[');
      dump_expr (TREE_OPERAND (t, 1), flags | TS_EXPR_PARENS);
      OB_PUTC (']');
      break;

    case CONVERT_EXPR:
      if (same_type_p (TREE_TYPE (t), void_type_node))
	{
	  OB_PUTS ("(void)");
	  dump_expr (TREE_OPERAND (t, 0), flags);
	}
      else
	dump_unary_op ("+", t, flags);
      break;

    case ADDR_EXPR:
      if (TREE_CODE (TREE_OPERAND (t, 0)) == FUNCTION_DECL
	  || TREE_CODE (TREE_OPERAND (t, 0)) == STRING_CST)
	dump_expr (TREE_OPERAND (t, 0), flags | TS_EXPR_PARENS);
      else
	dump_unary_op ("&", t, flags);
      break;

    case INDIRECT_REF:
      if (TREE_HAS_CONSTRUCTOR (t))
	{
	  t = TREE_OPERAND (t, 0);
	  my_friendly_assert (TREE_CODE (t) == CALL_EXPR, 237);
	  dump_expr (TREE_OPERAND (t, 0), flags | TS_EXPR_PARENS);
	  OB_PUTC ('(');
	  dump_expr_list (TREE_CHAIN (TREE_OPERAND (t, 1)), flags);
	  OB_PUTC (')');
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
      dump_unary_op (opname_tab [(int)TREE_CODE (t)], t, flags);
      break;

    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
      OB_PUTC ('(');
      dump_expr (TREE_OPERAND (t, 0), flags | TS_EXPR_PARENS);
      OB_PUTCP (opname_tab[(int)TREE_CODE (t)]);
      OB_PUTC (')');
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
	      if (flags & TS_EXPR_PARENS)
	        OB_PUTC ('(');
	      OB_PUTC ('*');
	      dump_expr (TREE_OPERAND (t, 0), flags & ~TS_EXPR_PARENS);
	      if (flags & TS_EXPR_PARENS)
	        OB_PUTC (')');
	      break;
	    }
	  /* else FALLTHRU */
	}
      dump_expr (TREE_OPERAND (t, 0), flags | TS_EXPR_PARENS);
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
	  tree idx = build_component_ref (t, index_identifier, NULL_TREE, 0);

	  if (integer_all_onesp (idx))
	    {
	      tree pfn = PFN_FROM_PTRMEMFUNC (t);
	      dump_unary_op ("&", pfn, flags | TS_EXPR_PARENS);
	      break;
	    }
	  else if (TREE_CODE (idx) == INTEGER_CST
		   && tree_int_cst_equal (idx, integer_zero_node))
	    {
	      /* A NULL pointer-to-member constant.  */
	      OB_PUTS ("((");
	      dump_type (TREE_TYPE (t), flags);
	      OB_PUTS (") 0)");
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
		  dump_expr (TREE_VALUE (virtuals),
	                     flags | TS_EXPR_PARENS);
		  break;
		}
	    }
	}
      OB_PUTC ('{');
      dump_expr_list (CONSTRUCTOR_ELTS (t), flags);
      OB_PUTC ('}');
      break;

    case OFFSET_REF:
      {
	tree ob = TREE_OPERAND (t, 0);
	if (is_dummy_object (ob))
	  {
	    t = TREE_OPERAND (t, 1);
	    if (TREE_CODE (t) == FUNCTION_DECL)
	      /* A::f */
	      dump_expr (t, flags | TS_EXPR_PARENS);
	    else if (BASELINK_P (t))
	      dump_expr (OVL_CURRENT (TREE_VALUE (t)), flags | TS_EXPR_PARENS);
	    else
	      dump_decl (t, flags);
	  }
	else
	  {
	    if (TREE_CODE (ob) == INDIRECT_REF)
	      {
		dump_expr (TREE_OPERAND (ob, 0), flags | TS_EXPR_PARENS);
		OB_PUTS (" ->* ");
	      }
	    else
	      {
		dump_expr (ob, flags | TS_EXPR_PARENS);
		OB_PUTS (" .* ");
	      }
	    dump_expr (TREE_OPERAND (t, 1), flags | TS_EXPR_PARENS);
	  }
	break;
      }

    case TEMPLATE_PARM_INDEX:
      dump_decl (TEMPLATE_PARM_DECL (t), flags & ~TS_DECL_TYPE);
      break;

    case IDENTIFIER_NODE:
      OB_PUTID (t);
      break;

    case SCOPE_REF:
      dump_type (TREE_OPERAND (t, 0), flags);
      OB_PUTS ("::");
      dump_expr (TREE_OPERAND (t, 1), flags | TS_EXPR_PARENS);
      break;

    case CAST_EXPR:
      if (TREE_OPERAND (t, 0) == NULL_TREE
	  || TREE_CHAIN (TREE_OPERAND (t, 0)))
	{
	  dump_type (TREE_TYPE (t), flags);
	  OB_PUTC ('(');
	  dump_expr_list (TREE_OPERAND (t, 0), flags);
	  OB_PUTC (')');
	}
      else
	{
	  OB_PUTC ('(');
	  dump_type (TREE_TYPE (t), flags);
	  OB_PUTC (')');
	  OB_PUTC ('(');
	  dump_expr_list (TREE_OPERAND (t, 0), flags);
	  OB_PUTC (')');
	}
      break;

    case LOOKUP_EXPR:
      OB_PUTID (TREE_OPERAND (t, 0));
      break;

    case ARROW_EXPR:
      dump_expr (TREE_OPERAND (t, 0), flags);
      OB_PUTS ("->");
      break;

    case SIZEOF_EXPR:
    case ALIGNOF_EXPR:
      if (TREE_CODE (t) == SIZEOF_EXPR)
	OB_PUTS ("sizeof (");
      else 
	{
	  my_friendly_assert (TREE_CODE (t) == ALIGNOF_EXPR, 0);
	  OB_PUTS ("__alignof__ (");
	}
      if (TREE_CODE_CLASS (TREE_CODE (TREE_OPERAND (t, 0))) == 't')
	dump_type (TREE_OPERAND (t, 0), flags);
      else
	dump_unary_op ("*", t, flags | TS_EXPR_PARENS);
      OB_PUTC (')');
      break;

    case DEFAULT_ARG:
      OB_PUTS ("{unparsed}");
      break;

    case TRY_CATCH_EXPR:
    case WITH_CLEANUP_EXPR:
    case CLEANUP_POINT_EXPR:
      dump_expr (TREE_OPERAND (t, 0), flags);
      break;

    case PSEUDO_DTOR_EXPR:
      dump_expr (TREE_OPERAND (t, 2), flags);
      OB_PUTS (".");
      dump_type (TREE_OPERAND (t, 0), flags);
      OB_PUTS ("::~");
      dump_type (TREE_OPERAND (t, 1), flags);
      break;

    case TEMPLATE_ID_EXPR:
      dump_decl (t, flags);
      break;

    case STMT_EXPR:
      /* We don't yet have a way of dumping statements in a
	 human-readable format.  */
      OB_PUTS ("{ ... }");
      break;

    case BIND_EXPR:
      OB_PUTS ("{ ");
      dump_expr (TREE_OPERAND (t, 1), flags & ~TS_EXPR_PARENS);
      OB_PUTS ("} ");
      break;
      
    case LOOP_EXPR:
      OB_PUTS ("while (1) { ");
      dump_expr (TREE_OPERAND (t, 0), flags & ~TS_EXPR_PARENS);
      OB_PUTS ("} ");
      break;

    case EXIT_EXPR:
      OB_PUTS ("if (");
      dump_expr (TREE_OPERAND (t, 0), flags & ~TS_EXPR_PARENS);
      OB_PUTS (") break; ");
      break;

    case TREE_LIST:
      if (TREE_VALUE (t) && TREE_CODE (TREE_VALUE (t)) == FUNCTION_DECL)
	{
	  OB_PUTID (DECL_NAME (TREE_VALUE (t)));
	  break;
	}
      /* else fall through */	

      /*  This list is incomplete, but should suffice for now.
	  It is very important that `sorry' does not call
	  `report_error_function'.  That could cause an infinite loop.  */
    default:
      sorry ("`%s' not supported by dump_expr",
	     tree_code_name[(int) TREE_CODE (t)]);

      /* fall through to ERROR_MARK...  */
    case ERROR_MARK:
      OB_PUTCP ("{expression error}");
      break;
    }
}

static void
dump_binary_op (opstring, t, flags)
     const char *opstring;
     tree t;
     enum tree_string_flags flags;
{
  OB_PUTC ('(');
  dump_expr (TREE_OPERAND (t, 0), flags | TS_EXPR_PARENS);
  OB_PUTC (' ');
  if (opstring)
    OB_PUTCP (opstring);
  else
    OB_PUTS ("<unknown operator>");
  OB_PUTC (' ');
  dump_expr (TREE_OPERAND (t, 1), flags | TS_EXPR_PARENS);
  OB_PUTC (')');
}

static void
dump_unary_op (opstring, t, flags)
     const char *opstring;
     tree t;
     enum tree_string_flags flags;
{
  if (flags & TS_EXPR_PARENS)
    OB_PUTC ('(');
  OB_PUTCP (opstring);
  dump_expr (TREE_OPERAND (t, 0), flags & ~TS_EXPR_PARENS);
  if (flags & TS_EXPR_PARENS)
    OB_PUTC (')');
}

/* Exported interface to stringifying types, exprs and decls under TS_*
   control.  */

const char *
type_as_string (typ, flags)
     tree typ;
     enum tree_string_flags flags;
{
  OB_INIT ();

  dump_type (typ, flags);

  OB_FINISH ();

  return (char *)obstack_base (&scratch_obstack);
}

const char *
expr_as_string (decl, flags)
     tree decl;
     enum tree_string_flags flags;
{
  OB_INIT ();

  dump_expr (decl, flags);

  OB_FINISH ();

  return (char *)obstack_base (&scratch_obstack);
}

const char *
decl_as_string (decl, flags)
     tree decl;
     enum tree_string_flags flags;
{
  OB_INIT ();

  dump_decl (decl, flags);

  OB_FINISH ();

  return (char *)obstack_base (&scratch_obstack);
}

const char *
context_as_string (context, flags)
     tree context;
     enum tree_string_flags flags;
{
  OB_INIT ();
  
  dump_scope (context, flags);
  
  OB_FINISH ();

  return (char *)obstack_base (&scratch_obstack);
}

/* Generate the three forms of printable names for lang_printable_name.  */

const char *
lang_decl_name (decl, v)
     tree decl;
     int v;
{
  if (v >= 2)
    return decl_as_string (decl, TS_DECL_TYPE);

  OB_INIT ();

  if (v == 1 && DECL_CLASS_SCOPE_P (decl))
    {
      dump_type (CP_DECL_CONTEXT (decl), TS_PLAIN);
      OB_PUTS ("::");
    }

  if (TREE_CODE (decl) == FUNCTION_DECL)
    dump_function_name (decl, TS_PLAIN);
  else
    dump_decl (DECL_NAME (decl), TS_PLAIN);

  OB_FINISH ();

  return (char *)obstack_base (&scratch_obstack);
}

const char *
cp_file_of (t)
     tree t;
{
  if (TREE_CODE (t) == PARM_DECL && DECL_CONTEXT (t))
    return DECL_SOURCE_FILE (DECL_CONTEXT (t));
  else if (TREE_CODE_CLASS (TREE_CODE (t)) == 't')
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

  if (TREE_CODE_CLASS (TREE_CODE (t)) == 't')
    line = DECL_SOURCE_LINE (TYPE_MAIN_DECL (t));
  else if (TREE_CODE (t) == OVERLOAD)
    line = DECL_SOURCE_LINE (OVL_FUNCTION (t));
  else
    line = DECL_SOURCE_LINE (t);

  if (line == 0)
    return lineno;

  return line;
}

/* Now the interfaces from cp_error et al to dump_type et al. Each takes an
   on/off VERBOSE flag and supply the appropriate TS_ flags to a dump_
   function.  */

static const char *
decl_to_string (decl, verbose)
     tree decl;
     int verbose;
{
  enum tree_string_flags flags = 0;

  if (TREE_CODE (decl) == TYPE_DECL || TREE_CODE (decl) == RECORD_TYPE
      || TREE_CODE (decl) == UNION_TYPE || TREE_CODE (decl) == ENUMERAL_TYPE)
    flags = TS_AGGR_TAGS;
  if (verbose)
    flags |= TS_DECL_TYPE | TS_DECORATE | TS_PARM_DEFAULTS;
  else if (TREE_CODE (decl) == FUNCTION_DECL)
    flags |= TS_DECL_TYPE | TS_FUNC_NORETURN;
  flags |= TS_TEMPLATE_PREFIX;
  
  OB_INIT ();

  dump_decl (decl, flags);

  OB_FINISH ();

  return (char *)obstack_base (&scratch_obstack);
}

static const char *
expr_to_string (decl, verbose)
     tree decl;
     int verbose ATTRIBUTE_UNUSED;
{
  OB_INIT ();

  dump_expr (decl, 0);

  OB_FINISH ();

  return (char *)obstack_base (&scratch_obstack);
}

static const char *
fndecl_to_string (fndecl, verbose)
     tree fndecl;
     int verbose;
{
  enum tree_string_flags flags;
  
  flags = TS_FUNC_THROW | TS_DECL_TYPE;
  if (verbose)
    flags |= TS_PARM_DEFAULTS;
  OB_INIT ();

  dump_decl (fndecl, flags);
  
  OB_FINISH ();

  return (char *)obstack_base (&scratch_obstack);
}


static const char *
code_to_string (c, v)
     enum tree_code c;
     int v ATTRIBUTE_UNUSED;
{
  return tree_code_name [c];
}

static const char *
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
      my_friendly_abort (355);
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
  static char buf[] = "operator                ";

  if (p == 0)
    return "{unknown}";
  
  strcpy (buf + 8, opname_tab [p]);
  return buf;
}

static const char *
type_to_string (typ, verbose)
     tree typ;
     int verbose;
{
  enum tree_string_flags flags;
  
  flags = 0;
  if (verbose)
    flags |= TS_AGGR_TAGS;
  flags |= TS_TEMPLATE_PREFIX;
  
  OB_INIT ();

  dump_type (typ, flags);

  OB_FINISH ();

  return (char *)obstack_base (&scratch_obstack);
}

static const char *
assop_to_string (p, v)
     enum tree_code p;
     int v ATTRIBUTE_UNUSED;
{
  static char buf[] = "operator                ";

  if (p == 0)
    return "{unknown}";
  
  strcpy (buf + 9, assignop_tab [p]);
  return buf;
}

static const char *
args_to_string (p, verbose)
     tree p;
     int verbose;
{
  enum tree_string_flags flags = 0;
  if (verbose)
    flags |= TS_AGGR_TAGS;
  
  if (p == NULL_TREE)
    return "";

  if (TREE_CODE_CLASS (TREE_CODE (TREE_VALUE (p))) == 't')
    return type_as_string (p, flags);

  OB_INIT ();
  for (; p; p = TREE_CHAIN (p))
    {
      if (TREE_VALUE (p) == null_node)
	OB_PUTS ("NULL");
      else
	dump_type (error_type (TREE_VALUE (p)), flags);
      if (TREE_CHAIN (p))
	OB_PUTS (", ");
    }
  OB_FINISH ();
  return (char *)obstack_base (&scratch_obstack);
}

static const char *
cv_to_string (p, v)
     tree p;
     int v ATTRIBUTE_UNUSED;
{
  OB_INIT ();

  dump_qualifiers (p, before);

  OB_FINISH ();

  return (char *)obstack_base (&scratch_obstack);
}
