/* Call-backs for C++ error reporting.
   This code is non-reentrant.
   Copyright (C) 1993, 94-97, 1998, 1999 Free Software Foundation, Inc.

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

typedef char* cp_printer ();

#define A args_as_string
#define C code_as_string
#define D decl_as_string
#define E expr_as_string
#define F fndecl_as_string
#define L language_as_string
#define O op_as_string
#define P parm_as_string
#define Q assop_as_string
#define T type_as_string
#define V cv_as_string

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
# define OB_UNPUT(N) obstack_blank (&scratch_obstack, - (N));

# define OB_END_TEMPLATE_ID() 						    \
  ((obstack_next_free (&scratch_obstack) != obstack_base (&scratch_obstack) \
    && obstack_next_free (&scratch_obstack)[-1] == '>')			    \
   ? OB_PUTC2 (' ', '>') : OB_PUTC ('>'))

# define NEXT_CODE(t) (TREE_CODE (TREE_TYPE (t)))

enum pad { none, before, after };

static void dump_type PROTO((tree, int));
static void dump_type_real PROTO((tree, int, int));
static void dump_simple_decl PROTO((tree, tree, int));
static void dump_decl PROTO((tree, int));
static void dump_function_decl PROTO((tree, int));
static void dump_expr PROTO((tree, int));
static void dump_unary_op PROTO((char *, tree, int));
static void dump_binary_op PROTO((char *, tree));
static void dump_aggr_type PROTO((tree, int, int));
static void dump_type_prefix PROTO((tree, int, int));
static void dump_type_suffix PROTO((tree, int, int));
static void dump_function_name PROTO((tree));
static void dump_expr_list PROTO((tree));
static void dump_global_iord PROTO((tree));
static void dump_qualifiers PROTO((tree, enum pad));
static void dump_char PROTO((int));
static void dump_parameters PROTO((tree, int, int));
static void dump_exception_spec PROTO((tree, int));
static char *aggr_variety PROTO((tree));
static tree ident_fndecl PROTO((tree));
static int interesting_scope_p PROTO((tree));

void
init_error ()
{
  gcc_obstack_init (&scratch_obstack);
  scratch_firstobj = (char *)obstack_alloc (&scratch_obstack, 0);
}

/* Returns nonzero if SCOPE is something we want to print for random decls.  */

static int
interesting_scope_p (scope)
     tree scope;
{
  if (scope == NULL_TREE
      || scope == global_namespace)
    return 0;

  return (TREE_CODE (scope) == NAMESPACE_DECL
	  || AGGREGATE_TYPE_P (scope));
}

static void
dump_qualifiers (t, p)
     tree t;
     enum pad p;
{
  if (TYPE_QUALS (t))
    {
      if (p == before) OB_PUTC (' ');
      switch (TYPE_QUALS (t))
	{
	case TYPE_QUAL_CONST:
	  OB_PUTS ("const");
	  break;

	case TYPE_QUAL_VOLATILE:
	  OB_PUTS ("volatile");
	  break;

	case TYPE_QUAL_RESTRICT:
	  OB_PUTS ("__restrict");
	  break;

	case TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE:
	  OB_PUTS ("const volatile");
	  break;

	case TYPE_QUAL_CONST | TYPE_QUAL_RESTRICT:
	  OB_PUTS ("const __restrict");
	  break;

	case TYPE_QUAL_VOLATILE | TYPE_QUAL_RESTRICT:
	  OB_PUTS ("volatile __restrict");
	  break;

	case TYPE_QUAL_CONST | TYPE_QUAL_VOLATILE | TYPE_QUAL_RESTRICT:
	  OB_PUTS ("const volatile __restrict");
	  break;

	default:
	  my_friendly_abort (0);
	}
      if (p == after) OB_PUTC (' ');
    }
}

/* This must be large enough to hold any printed integer or floating-point
   value.  */
static char digit_buffer[128];

/* Dump into the obstack a human-readable equivalent of TYPE.  */

static void
dump_type_real (t, v, canonical_name)
     tree t;
     int v;			/* verbose? */
     int canonical_name;
{
  if (t == NULL_TREE)
    return;
  
  if (TYPE_PTRMEMFUNC_P (t))
    goto offset_type;

  switch (TREE_CODE (t))
    {
    case ERROR_MARK:
      OB_PUTS ("{error}");
      break;

    case UNKNOWN_TYPE:
      OB_PUTS ("{unknown type}");
      break;

    case TREE_LIST:
      /* A list of function parms.  */
      dump_parameters (t, 0, canonical_name);
      break;

    case IDENTIFIER_NODE:
      OB_PUTID (t);
      break;

    case TREE_VEC:
      dump_type_real (BINFO_TYPE (t), v, canonical_name);
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
    case ENUMERAL_TYPE:
      if (TYPE_LANG_SPECIFIC (t)
	  && (IS_SIGNATURE_POINTER (t) || IS_SIGNATURE_REFERENCE (t)))
	{
	  dump_qualifiers (t, after);
	  dump_type_real (SIGNATURE_TYPE (t), v, canonical_name);
	  if (IS_SIGNATURE_POINTER (t))
	    OB_PUTC ('*');
	  else
	    OB_PUTC ('&');
	}
      else
	dump_aggr_type (t, v, canonical_name);
      break;

    case TYPE_DECL:
    case TEMPLATE_DECL:
    case NAMESPACE_DECL:
      dump_decl (t, v);
      break;

    case COMPLEX_TYPE:
      OB_PUTS ("complex ");
      dump_type_real (TREE_TYPE (t), v, canonical_name);
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
	type = canonical_name ? TYPE_MAIN_VARIANT (t) : t;
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
	    OB_PUTS ("{anonymous template template parm}");
	}
      else
	{
	  int i;
	  tree args = TYPE_TI_ARGS (t);
	  OB_PUTID (TYPE_IDENTIFIER (t));
	  OB_PUTC ('<');
	  for (i = 0; i < TREE_VEC_LENGTH (args); i++)
	    {
	      tree arg = TREE_VEC_ELT (args, i);
	      if (TREE_CODE_CLASS (TREE_CODE (arg)) == 't'
		  || TREE_CODE (arg) == TEMPLATE_DECL)
	        dump_type_real (arg, 0, canonical_name);
	      else
	        dump_expr (arg, 0);
	      if (i < TREE_VEC_LENGTH (args)-1)
	        OB_PUTC2 (',', ' ');
	    }
	  OB_END_TEMPLATE_ID ();
	}
      break;

    case TEMPLATE_TYPE_PARM:
      dump_qualifiers (t, after);
      if (TYPE_IDENTIFIER (t))
	OB_PUTID (TYPE_IDENTIFIER (t));
      else
	OB_PUTS ("{anonymous template type parm}");
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
      dump_type_prefix (t, v, canonical_name);
      dump_type_suffix (t, v, canonical_name);
      break;

    case TYPENAME_TYPE:
      OB_PUTS ("typename ");
      dump_type_real (TYPE_CONTEXT (t), 0, canonical_name);
      OB_PUTS ("::");
      dump_decl (TYPENAME_TYPE_FULLNAME (t), v);
      break;

    case TYPEOF_TYPE:
      OB_PUTS ("__typeof (");
      dump_expr (TYPE_FIELDS (t), 1);
      OB_PUTC (')');
      break;

    default:
      sorry ("`%s' not supported by dump_type",
	     tree_code_name[(int) TREE_CODE (t)]);
    }
}

static char *
aggr_variety (t)
     tree t;
{
  if (TREE_CODE (t) == ENUMERAL_TYPE)
    return "enum";
  else if (TREE_CODE (t) == UNION_TYPE)
    return "union";
  else if (TYPE_LANG_SPECIFIC (t) && CLASSTYPE_DECLARED_CLASS (t))
    return "class";
  else if (TYPE_LANG_SPECIFIC (t) && IS_SIGNATURE (t))
    return "signature";
  else
    return "struct";
}

static void
dump_type (t, v)
     tree t;
     int v;			/* verbose? */
{
  dump_type_real (t, v, 0);
}

/* Print out a class declaration, in the form `class foo'.  */

static void
dump_aggr_type (t, v, canonical_name)
     tree t;
     int v;			/* verbose? */
     int canonical_name;
{
  tree name;
  char *variety = aggr_variety (t);

  dump_qualifiers (t, after);

  if (v > 0)
    {
      OB_PUTCP (variety);
      OB_PUTC (' ');
    }
  
  name = TYPE_NAME (canonical_name ? TYPE_MAIN_VARIANT (t) : t);

  if (name && CP_DECL_CONTEXT (name) != global_namespace)
    {
      /* FUNCTION_DECL or RECORD_TYPE */
      dump_decl (DECL_CONTEXT (name), 0);
      OB_PUTC2 (':', ':');
    }

  /* kludge around weird behavior on g++.brendan/line1.C */
  if (name && TREE_CODE (name) != IDENTIFIER_NODE)
    name = DECL_NAME (name);

  if (name == 0 || ANON_AGGRNAME_P (name))
    {
      OB_PUTS ("{anonymous");
      if (!v)
	{
	  OB_PUTC (' ');
	  OB_PUTCP (variety);
	}
      OB_PUTC ('}');
    }
  else
    OB_PUTID (name);
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
dump_type_prefix (t, v, canonical_name)
     tree t;
     int v;			/* verbosity */
     int canonical_name;
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
	
	dump_type_prefix (sub, v, canonical_name);
	/* A tree for a member pointer looks like pointer to offset,
	   so let the OFFSET_TYPE case handle it.  */
	if (!TYPE_PTRMEM_P (t))
	  {
	    switch (TREE_CODE (sub))
	      {
		/* We don't want int ( *)() */
	      case FUNCTION_TYPE:
	      case METHOD_TYPE:
		break;
		
	      case ARRAY_TYPE:
		OB_PUTC2 (' ', '(');
		break;

	      case POINTER_TYPE:
		/* We don't want "char * *" */
		if (TYPE_QUALS (sub) == TYPE_UNQUALIFIED)
		  break;
		/* But we do want "char *const *" */
		
	      default:
		OB_PUTC (' ');
	      }
	    if (TREE_CODE (t) == POINTER_TYPE)
	      OB_PUTC ('*');
	    else
	      OB_PUTC ('&');
	    dump_qualifiers (t, none);
	  }
      }
      break;

    case OFFSET_TYPE:
    offset_type:
      dump_type_prefix (TREE_TYPE (t), v, canonical_name);
      if (TREE_CODE (t) == OFFSET_TYPE)	/* pmfs deal with this in d_t_p */
	{
	  OB_PUTC (' ');
	  dump_type_real (TYPE_OFFSET_BASETYPE (t), 0, canonical_name);
	  OB_PUTC2 (':', ':');
	}
      OB_PUTC ('*');
      dump_qualifiers (t, none);
      break;

      /* Can only be reached through function pointer -- this would not be
         correct if FUNCTION_DECLs used it.  */
    case FUNCTION_TYPE:
      dump_type_prefix (TREE_TYPE (t), v, canonical_name);
      OB_PUTC2 (' ', '(');
      break;

    case METHOD_TYPE:
      dump_type_prefix (TREE_TYPE (t), v, canonical_name);
      OB_PUTC2 (' ', '(');
      dump_aggr_type (TYPE_METHOD_BASETYPE (t), 0, canonical_name);
      OB_PUTC2 (':', ':');
      break;

    case ARRAY_TYPE:
      dump_type_prefix (TREE_TYPE (t), v, canonical_name);
      break;

    case ENUMERAL_TYPE:
    case ERROR_MARK:
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
      dump_type_real (t, v, canonical_name);
      break;
      
    default:
      sorry ("`%s' not supported by dump_type_prefix",
	     tree_code_name[(int) TREE_CODE (t)]);
    }
}

static void
dump_type_suffix (t, v, canonical_name)
     tree t;
     int v;			/* verbose? */
     int canonical_name;
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
      dump_type_suffix (TREE_TYPE (t), v, canonical_name);
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
	dump_parameters (arg, 0, canonical_name);

	if (TREE_CODE (t) == METHOD_TYPE)
	  dump_qualifiers
	    (TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (t))), before);
	dump_type_suffix (TREE_TYPE (t), v, canonical_name);
	dump_exception_spec (TYPE_RAISES_EXCEPTIONS (t), canonical_name);
	break;
      }

    case ARRAY_TYPE:
      OB_PUTC ('[');
      if (TYPE_DOMAIN (t))
	{
	  if (TREE_CODE (TYPE_MAX_VALUE (TYPE_DOMAIN (t))) == INTEGER_CST)
	    OB_PUTI (TREE_INT_CST_LOW (TYPE_MAX_VALUE (TYPE_DOMAIN (t))) + 1);
	  else if (TREE_CODE (TYPE_MAX_VALUE (TYPE_DOMAIN (t))) == MINUS_EXPR)
	    dump_expr (TREE_OPERAND (TYPE_MAX_VALUE (TYPE_DOMAIN (t)), 0), 0);
	  else
	    dump_expr (fold (build_binary_op
			     (PLUS_EXPR, TYPE_MAX_VALUE (TYPE_DOMAIN (t)),
			      integer_one_node)), 0);
	}
      OB_PUTC (']');
      dump_type_suffix (TREE_TYPE (t), v, canonical_name);
      break;
      
    case ENUMERAL_TYPE:
    case ERROR_MARK:
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
  char *name = IDENTIFIER_POINTER (t);

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
dump_simple_decl (t, type, v)
     tree t;
     tree type;
     int v;
{
  if (v > 0)
    {
      dump_type_prefix (type, v, 0);
      OB_PUTC (' ');
    }
  if (interesting_scope_p (DECL_CONTEXT (t)))
    {
      dump_decl (DECL_CONTEXT (t), 0);
      OB_PUTC2 (':',':');
    }
  if (DECL_NAME (t))
    dump_decl (DECL_NAME (t), v);
  else
    OB_PUTS ("{anon}");
  if (v > 0)
    dump_type_suffix (type, v, 0);
}

static void
dump_decl (t, v)
     tree t;
     int v;			/* verbosity */
{
  if (t == NULL_TREE)
    return;

  switch (TREE_CODE (t))
    {
    case ERROR_MARK:
      OB_PUTS (" /* decl error */ ");
      break;

    case TYPE_DECL:
      {
	/* Don't say 'typedef class A' */
        if (DECL_ARTIFICIAL (t))
	  {
	    if (v > 0 && TREE_CODE (TREE_TYPE (t)) == TEMPLATE_TYPE_PARM)
	      /* Say `class T' not just `T'. */
	      OB_PUTS ("class ");

	    dump_type (TREE_TYPE (t), v);
	    break;
	  }
      }
      if (v > 0)
	OB_PUTS ("typedef ");
      dump_simple_decl (t, DECL_ORIGINAL_TYPE (t) 
			? DECL_ORIGINAL_TYPE (t) : TREE_TYPE (t), v);
      break;
      
    case VAR_DECL:
      if (DECL_NAME (t) && VTABLE_NAME_P (DECL_NAME (t)))
	{
	  OB_PUTS ("vtable for ");
	  if (TYPE_P (DECL_CONTEXT (t)))
	    dump_type (DECL_CONTEXT (t), v);
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
      dump_simple_decl (t, TREE_TYPE (t), v);
      break;

    case NAMESPACE_DECL:
      if (CP_DECL_CONTEXT (t) != global_namespace)
	{
	  dump_decl (DECL_CONTEXT (t), v);
	  OB_PUTC2 (':',':');
	}
      if (DECL_NAME (t) == anonymous_namespace_name)
	OB_PUTS ("{anonymous}");
      else
	OB_PUTID (DECL_NAME (t));
      break;

    case SCOPE_REF:
      dump_decl (TREE_OPERAND (t, 0), 0);
      OB_PUTS ("::");
      dump_decl (TREE_OPERAND (t, 1), 0);
      break;      

    case ARRAY_REF:
      dump_decl (TREE_OPERAND (t, 0), v);
      OB_PUTC ('[');
      dump_decl (TREE_OPERAND (t, 1), v);
      OB_PUTC (']');
      break;

      /* So that we can do dump_decl in dump_aggr_type and have it work for
	 both class and function scope.  */
    case RECORD_TYPE:
    case UNION_TYPE:
    case ENUMERAL_TYPE:
      dump_type (t, v);
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
	    dump_decl (DECL_NAME (f), 0);
	  }
	else if (IDENTIFIER_TYPENAME_P (t))
	  {
	    OB_PUTS ("operator ");
	    /* Not exactly IDENTIFIER_TYPE_VALUE.  */
	    dump_type (TREE_TYPE (t), 0);
	    break;
	  }
	else if (IDENTIFIER_OPNAME_P (t))
	  {
	    char *name_string = operator_name_string (t);
	    OB_PUTS ("operator ");
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
      else
	dump_function_decl (t, v);
      break;

    case TEMPLATE_DECL:
      {
	tree orig_args = DECL_TEMPLATE_PARMS (t);
	tree args;
	int i; 
	for (args = orig_args = nreverse (orig_args); 
	     args;
	     args = TREE_CHAIN (args))
	  {
	    int len = TREE_VEC_LENGTH (TREE_VALUE (args));

	    OB_PUTS ("template <");
	    for (i = 0; i < len; i++)
	      {
		tree arg = TREE_VEC_ELT (TREE_VALUE (args), i);
		tree defval = TREE_PURPOSE (arg);
		arg = TREE_VALUE (arg);
		if (TREE_CODE (arg) == TYPE_DECL)
		  {
		    if (DECL_NAME (arg))
		      {
			OB_PUTS ("class ");
			OB_PUTID (DECL_NAME (arg));
		      }
		    else
		      OB_PUTS ("class");
		  }
		else
		  dump_decl (arg, 1);
		
		if (defval)
		  {
		    OB_PUTS (" = ");
		    if (TREE_CODE (arg) == TYPE_DECL
			|| TREE_CODE (arg) == TEMPLATE_DECL)
		      dump_type (defval, 1);
		    else
		      dump_expr (defval, 1);
		  }
		
		OB_PUTC2 (',', ' ');
	      }
	    if (len != 0)
	      OB_UNPUT (2);
	    OB_END_TEMPLATE_ID ();
	    OB_PUTC (' ');
	  }
	nreverse(orig_args);

	if (TREE_CODE (DECL_TEMPLATE_RESULT (t)) == TYPE_DECL)
	  dump_type (TREE_TYPE (t), v);
	else if (TREE_CODE (DECL_TEMPLATE_RESULT (t)) == VAR_DECL)
	  dump_decl (DECL_TEMPLATE_RESULT (t), v);
	else if (TREE_TYPE (t) == NULL_TREE)
	   my_friendly_abort (353);
	else switch (NEXT_CODE (t))
	  {
	  case METHOD_TYPE:
	  case FUNCTION_TYPE:
	    dump_function_decl (t, v);
	    break;

	  default:
	    /* This case can occur with some illegal code.  */
	    dump_type (TREE_TYPE (t), v);
	  }
      }
      break;

    case TEMPLATE_ID_EXPR:
      {
	tree args;
	tree name = TREE_OPERAND (t, 0);
	if (is_overloaded_fn (name))
	  name = DECL_NAME (get_first_fn (name));
	dump_decl (name, v);
	OB_PUTC ('<');
	for (args = TREE_OPERAND (t, 1); args; args = TREE_CHAIN (args))
	  {
	    if (TREE_CODE_CLASS (TREE_CODE (TREE_VALUE (args))) == 't'
		|| TREE_CODE (TREE_VALUE (args)) == TEMPLATE_DECL)
	      dump_type (TREE_VALUE (args), 0);
	    else
	      dump_expr (TREE_VALUE (args), 0);
	    if (TREE_CHAIN (args))
	      OB_PUTC2 (',', ' ');
	  }
	OB_END_TEMPLATE_ID ();
      }
      break;

    case LOOKUP_EXPR:
      dump_decl (TREE_OPERAND (t, 0), v);
      break;

    case LABEL_DECL:
      OB_PUTID (DECL_NAME (t));
      break;

    case CONST_DECL:
      if ((TREE_TYPE (t) != NULL_TREE && NEXT_CODE (t) == ENUMERAL_TYPE)
	  || (DECL_INITIAL (t) &&
	      TREE_CODE (DECL_INITIAL (t)) == TEMPLATE_PARM_INDEX))
	dump_simple_decl (t, TREE_TYPE (t), v);
      else if (DECL_NAME (t))
	dump_decl (DECL_NAME (t), v);
      else if (DECL_INITIAL (t))
	dump_expr (DECL_INITIAL (t), 0);
      else
	OB_PUTS ("enumerator");
      break;

    case USING_DECL:
      OB_PUTS ("using ");
      dump_type (DECL_INITIAL (t), 0);
      OB_PUTS ("::");
      OB_PUTID (DECL_NAME (t));
      break;

    default:
      sorry ("`%s' not supported by dump_decl",
	     tree_code_name[(int) TREE_CODE (t)]);
    }
}

/* Pretty print a function decl. There are several ways we want to print a
   function declaration. We use V to tell us what.
     V    - 01 23
   args   - ++ ++
   retval - -+ ++
   default- -+ -+
   throw  - -- ++
   As cp_error can only apply the '#' flag once to give 0 and 1 for V, there
   is %D which doesn't print the throw specs, and %F which does. */

static void
dump_function_decl (t, v)
     tree t;
     int v;
{
  tree name;
  tree fntype;
  tree parmtypes;
  tree cname = NULL_TREE;

  if (TREE_CODE (t) == TEMPLATE_DECL)
    t = DECL_TEMPLATE_RESULT (t);

  name = DECL_ASSEMBLER_NAME (t);
  fntype = TREE_TYPE (t);
  parmtypes = TYPE_ARG_TYPES (fntype);

  /* Friends have DECL_CLASS_CONTEXT set, but not DECL_CONTEXT.  */
  if (DECL_CLASS_SCOPE_P (t))
    cname = DECL_CLASS_CONTEXT (t);
  /* this is for partially instantiated template methods */
  else if (TREE_CODE (fntype) == METHOD_TYPE)
    cname = TREE_TYPE (TREE_VALUE (parmtypes));

  /* Print the return type.  */
  if (v > 0)
    {
      if (DECL_STATIC_FUNCTION_P (t))
	OB_PUTS ("static ");
    
      if (! DECL_CONV_FN_P (t)
	  && ! DECL_CONSTRUCTOR_P (t)
	  && ! DECL_DESTRUCTOR_P (t))
	{
	  dump_type_prefix (TREE_TYPE (fntype), 1, 0);
	  OB_PUTC (' ');
	}
    }

  /* Print the function name.  */
  if (cname)
    {
      dump_type (cname, 0);
      OB_PUTC2 (':', ':');
      if (TREE_CODE (fntype) == METHOD_TYPE && parmtypes)
	parmtypes = TREE_CHAIN (parmtypes);
      if (DECL_CONSTRUCTOR_FOR_VBASE_P (t))
	/* Skip past "in_charge" identifier.  */
	parmtypes = TREE_CHAIN (parmtypes);
    }
  else if (CP_DECL_CONTEXT (t) != global_namespace)
    {
      dump_decl (DECL_CONTEXT (t), 0);
      OB_PUTC2 (':',':');
    }

  if (DESTRUCTOR_NAME_P (name) && DECL_LANGUAGE (t) == lang_cplusplus)
    parmtypes = TREE_CHAIN (parmtypes);
  
  dump_function_name (t);

  /* If V is negative, we don't print the argument types.  */
  if (v < 0)
    return;

  dump_parameters (parmtypes, v & 1, 0);
  
  if (v && ! DECL_CONV_FN_P (t))
    dump_type_suffix (TREE_TYPE (fntype), 1, 0);

  if (TREE_CODE (fntype) == METHOD_TYPE)
    {
      if (IS_SIGNATURE (cname))
	/* We look at the type pointed to by the `optr' field of `this.'  */
	dump_qualifiers
	  (TREE_TYPE (TREE_TYPE (TYPE_FIELDS (TREE_VALUE (TYPE_ARG_TYPES (fntype))))), before);
      else
	dump_qualifiers
	  (TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (fntype))), before);
    }
  
  if (v >= 2)
    dump_exception_spec (TYPE_RAISES_EXCEPTIONS (fntype), 0);
}

/* Print a parameter list. V indicates if we show default values or not. If
   these are for a member function, the member object ptr
   (and any other hidden args) should have already been removed. */

static void
dump_parameters (parmtypes, v, canonical_name)
     tree parmtypes;
     int v;
     int canonical_name;
{
  int first;
  OB_PUTC ('(');

  for (first = 1; parmtypes != void_list_node;
       parmtypes = TREE_CHAIN (parmtypes))
    {
      if (!first)
        OB_PUTC2 (',', ' ');
      first = 0;
      if (!parmtypes)
        {
          OB_PUTS ("...");
          break;
        }
      dump_type_real (TREE_VALUE (parmtypes), 0, canonical_name);
      
      if (TREE_PURPOSE (parmtypes) && v)
        {
          OB_PUTS (" = ");
          dump_expr (TREE_PURPOSE (parmtypes), 0);
        }
    }

  OB_PUTC (')');
}

/* Print an exception specification. T is the exception specification. */

static void
dump_exception_spec (t, canonical_name)
     tree t;
     int canonical_name;
{
  if (t)
    {
      OB_PUTS (" throw (");
      if (TREE_VALUE (t) != NULL_TREE)
        while (1)
          {
            dump_type_real (TREE_VALUE (t), 0, canonical_name);
            t = TREE_CHAIN (t);
            if (!t)
              break;
            OB_PUTC2 (',', ' ');
          }
      OB_PUTC (')');
    }
}

/* Handle the function name for a FUNCTION_DECL node, grokking operators
   and destructors properly.  */

static void
dump_function_name (t)
     tree t;
{
  tree name = DECL_NAME (t);

  if (DECL_DESTRUCTOR_P (t))
    {
      OB_PUTC ('~');
      dump_decl (name, 0);
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
      dump_type (TREE_TYPE (TREE_TYPE (t)), 0);
    }
  else if (IDENTIFIER_OPNAME_P (name))
    {
      char *name_string = operator_name_string (name);
      OB_PUTS ("operator ");
      OB_PUTCP (name_string);
    }
  else
    dump_decl (name, 0);

  if (DECL_LANG_SPECIFIC (t) && DECL_USE_TEMPLATE (t) 
      && DECL_TEMPLATE_INFO (t)
      && (DECL_TEMPLATE_SPECIALIZATION (t) 
	  || TREE_CODE (DECL_TI_TEMPLATE (t)) != TEMPLATE_DECL
	  || DECL_TEMPLATE_SPECIALIZATION (DECL_TI_TEMPLATE (t))
	  || PRIMARY_TEMPLATE_P (DECL_TI_TEMPLATE (t))))
    {
      tree args = DECL_TEMPLATE_INFO (t) ? DECL_TI_ARGS (t) : NULL_TREE; 
      OB_PUTC ('<');

      /* Be careful only to print things when we have them, so as not
	 to crash producing error messages.  */
      if (args)
	{
	  if (TREE_CODE (args) == TREE_LIST)
	    {
	      tree arg;
	      int need_comma = 0;
	      
	      for (arg = args; arg; arg = TREE_CHAIN (arg))
		{
		  tree a = TREE_VALUE (arg);
		  
		  if (need_comma)
		    OB_PUTS (", ");
		  
		  if (a)
		    {
		      if (TREE_CODE_CLASS (TREE_CODE (a)) == 't'
			  || TREE_CODE (a) == TEMPLATE_DECL)
			dump_type (a, 0);
		      else
			dump_expr (a, 0);
		    }
		  
		  need_comma = 1;
		}
	    }
	  else if (TREE_CODE (args) == TREE_VEC)
	    {
	      int i;
	      int need_comma = 0;
	      
	      if (TREE_VEC_LENGTH (args) > 0
		  && TREE_CODE (TREE_VEC_ELT (args, 0)) == TREE_VEC)
		args = TREE_VEC_ELT (args, 
				     TREE_VEC_LENGTH (args) - 1);
	      
	      for (i = 0; i < TREE_VEC_LENGTH (args); i++)
		{
		  tree a = TREE_VEC_ELT (args, i);
		  
		  if (need_comma)
		    OB_PUTS (", ");
		  
		  if (a)
		    {
		      if (TREE_CODE_CLASS (TREE_CODE (a)) == 't'
			  || TREE_CODE (a) == TEMPLATE_DECL)
			dump_type (a, 0);
		      else
			dump_expr (a, 0);
		    }
		  
		  need_comma = 1;
		}
	    }
	}
      OB_END_TEMPLATE_ID ();
    }
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
dump_expr_list (l)
     tree l;
{
  while (l)
    {
      dump_expr (TREE_VALUE (l), 0);
      if (TREE_CHAIN (l))
	OB_PUTC2 (',', ' ');
      l = TREE_CHAIN (l);
    }
}

/* Print out an expression */

static void
dump_expr (t, nop)
     tree t;
     int nop;			/* suppress parens */
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
      dump_decl (t, -1);
      break;

    case INTEGER_CST:
      {
	tree type = TREE_TYPE (t);
	my_friendly_assert (type != 0, 81);

	/* If it's an enum, output its tag, rather than its value.  */
	if (TREE_CODE (type) == ENUMERAL_TYPE)
	  {
	    char *p = enum_name_string (t, type);
	    OB_PUTCP (p);
	  }
	else if (type == boolean_type_node)
	  {
	    if (t == boolean_false_node
		|| (TREE_INT_CST_LOW (t) == 0
		    && TREE_INT_CST_HIGH (t) == 0))
	      OB_PUTS ("false");
	    else if (t == boolean_true_node)
	      OB_PUTS ("true");
	  }
	else if (type == char_type_node)
	  {
	    OB_PUTC ('\'');
	    dump_char (TREE_INT_CST_LOW (t));
	    OB_PUTC ('\'');
	  }
	else if (TREE_INT_CST_HIGH (t)
		 != (TREE_INT_CST_LOW (t) >> (HOST_BITS_PER_WIDE_INT - 1)))
	  {
	    tree val = t;
	    if (TREE_INT_CST_HIGH (val) < 0)
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
	unsigned char *p = (unsigned char *) &TREE_REAL_CST (t);
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
      dump_type (PTRMEM_CST_CLASS (t), 0);
      OB_PUTS ("::");
      OB_PUTID (DECL_NAME (PTRMEM_CST_MEMBER (t)));
      break;

    case STRING_CST:
      {
	char *p = TREE_STRING_POINTER (t);
	int len = TREE_STRING_LENGTH (t) - 1;
	int i;

	OB_PUTC ('\"');
	for (i = 0; i < len; i++)
	  dump_char (p[i]);
	OB_PUTC ('\"');
      }
      break;

    case COMPOUND_EXPR:
      dump_binary_op (",", t);
      break;

    case COND_EXPR:
      OB_PUTC ('(');
      dump_expr (TREE_OPERAND (t, 0), 0);
      OB_PUTS (" ? ");
      dump_expr (TREE_OPERAND (t, 1), 0);
      OB_PUTS (" : ");
      dump_expr (TREE_OPERAND (t, 2), 0);
      OB_PUTC (')');
      break;

    case SAVE_EXPR:
      if (TREE_HAS_CONSTRUCTOR (t))
	{
	  OB_PUTS ("new ");
	  dump_type (TREE_TYPE (TREE_TYPE (t)), 0);
	}
      else
	{
	  dump_expr (TREE_OPERAND (t, 0), 0);
	}
      break;

    case AGGR_INIT_EXPR:
      OB_PUTID (TYPE_IDENTIFIER (TREE_TYPE (t)));
      OB_PUTC ('(');
      if (TREE_OPERAND (t, 1))
	dump_expr_list (TREE_CHAIN (TREE_OPERAND (t, 1)));
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
		dump_expr (TREE_OPERAND (ob, 0), 0);
		OB_PUTC ('.');
	      }
	    else if (TREE_CODE (ob) != PARM_DECL
		     || strcmp (IDENTIFIER_POINTER (DECL_NAME (ob)), "this"))
	      {
		dump_expr (ob, 0);
		OB_PUTC2 ('-', '>');
	      }
	    args = TREE_CHAIN (args);
	  }
	dump_expr (fn, 0);
	OB_PUTC ('(');
	dump_expr_list (args);
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
	    dump_expr_list (TREE_OPERAND (t, 0));
	    OB_PUTS (") ");
	  }
	if (TREE_CODE (type) == ARRAY_REF)
	  type = build_cplus_array_type
	    (TREE_OPERAND (type, 0),
	     build_index_type (size_binop (MINUS_EXPR, TREE_OPERAND (type, 1),
					   integer_one_node)));
	dump_type (type, 0);
	if (TREE_OPERAND (t, 2))
	  {
	    OB_PUTC ('(');
	    dump_expr_list (TREE_OPERAND (t, 2));
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
	dump_expr (TREE_OPERAND (t, 1), 0);
      break;

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
      dump_binary_op (opname_tab[(int) TREE_CODE (t)], t);
      break;

    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
      dump_binary_op ("/", t);
      break;

    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
      dump_binary_op ("%", t);
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
		dump_expr (ob, 0);
		OB_PUTC2 ('-', '>');
	      }
	  }
	else
	  {
	    dump_expr (ob, 0);
	    OB_PUTC ('.');
	  }
	dump_expr (TREE_OPERAND (t, 1), 1);
      }
      break;

    case ARRAY_REF:
      dump_expr (TREE_OPERAND (t, 0), 0);
      OB_PUTC ('[');
      dump_expr (TREE_OPERAND (t, 1), 0);
      OB_PUTC (']');
      break;

    case CONVERT_EXPR:
      dump_unary_op ("+", t, nop);
      break;

    case ADDR_EXPR:
      if (TREE_CODE (TREE_OPERAND (t, 0)) == FUNCTION_DECL
	  || TREE_CODE (TREE_OPERAND (t, 0)) == STRING_CST)
	dump_expr (TREE_OPERAND (t, 0), 0);
      else
	dump_unary_op ("&", t, nop);
      break;

    case INDIRECT_REF:
      if (TREE_HAS_CONSTRUCTOR (t))
	{
	  t = TREE_OPERAND (t, 0);
	  my_friendly_assert (TREE_CODE (t) == CALL_EXPR, 237);
	  dump_expr (TREE_OPERAND (t, 0), 0);
	  OB_PUTC ('(');
	  dump_expr_list (TREE_CHAIN (TREE_OPERAND (t, 1)));
	  OB_PUTC (')');
	}
      else
	{
	  if (TREE_OPERAND (t,0) != NULL_TREE
	      && TREE_TYPE (TREE_OPERAND (t, 0))
	      && NEXT_CODE (TREE_OPERAND (t, 0)) == REFERENCE_TYPE)
	    dump_expr (TREE_OPERAND (t, 0), nop);
	  else
	    dump_unary_op ("*", t, nop);
	}
      break;

    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
      dump_unary_op (opname_tab [(int)TREE_CODE (t)], t, nop);
      break;

    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
      OB_PUTC ('(');
      dump_expr (TREE_OPERAND (t, 0), 0);
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
	      if (!nop) OB_PUTC ('(');
	      OB_PUTC ('*');
	      dump_expr (TREE_OPERAND (t, 0), 1);
	      if (!nop) OB_PUTC (')');
	      break;
	    }
	  /* else FALLTHRU */
	}
      dump_expr (TREE_OPERAND (t, 0), 0);
      break;

    case NOP_EXPR:
      dump_expr (TREE_OPERAND (t, 0), nop);
      break;

    case CONSTRUCTOR:
      if (TREE_TYPE (t) && TYPE_PTRMEMFUNC_P (TREE_TYPE (t)))
	{
	  tree idx = build_component_ref (t, index_identifier, NULL_TREE, 0);

	  if (integer_all_onesp (idx))
	    {
	      tree pfn = PFN_FROM_PTRMEMFUNC (t);
	      dump_unary_op ("&", pfn, 0);
	      break;
	    }
	  else if (TREE_CODE (idx) == INTEGER_CST
		   && tree_int_cst_equal (idx, integer_zero_node))
	    {
	      /* A NULL pointer-to-member constant.  */
	      OB_PUTS ("((");
	      dump_type (TREE_TYPE (t), 0);
	      OB_PUTS (") 0)");
	      break;
	    }
	  else if (TREE_CODE (idx) == INTEGER_CST
		   && TREE_INT_CST_HIGH (idx) == 0)
	    {
	      tree virtuals;
	      unsigned HOST_WIDE_INT n;

	      t = TREE_TYPE (TYPE_PTRMEMFUNC_FN_TYPE (TREE_TYPE (t)));
	      t = TYPE_METHOD_BASETYPE (t);
	      virtuals = BINFO_VIRTUALS (TYPE_BINFO (TYPE_MAIN_VARIANT (t)));
	      
	      n = TREE_INT_CST_LOW (idx);

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
		  dump_expr (FNADDR_FROM_VTABLE_ENTRY (TREE_VALUE (virtuals)), 0);
		  break;
		}
	    }
	}
      OB_PUTC ('{');
      dump_expr_list (CONSTRUCTOR_ELTS (t));
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
	      dump_expr (t, 0);
	    else if (BASELINK_P (t))
	      dump_expr (OVL_CURRENT (TREE_VALUE (t)), 0);
	    else
	      dump_decl (t, 0);
	  }
	else
	  {
	    if (TREE_CODE (ob) == INDIRECT_REF)
	      {
		dump_expr (TREE_OPERAND (ob, 0), 0);
		OB_PUTS (" ->* ");
	      }
	    else
	      {
		dump_expr (ob, 0);
		OB_PUTS (" .* ");
	      }
	    dump_expr (TREE_OPERAND (t, 1), 0);
	  }
	break;
      }

    case TEMPLATE_PARM_INDEX:
      dump_decl (TEMPLATE_PARM_DECL (t), -1);
      break;

    case IDENTIFIER_NODE:
      OB_PUTID (t);
      break;

    case SCOPE_REF:
      dump_type (TREE_OPERAND (t, 0), 0);
      OB_PUTS ("::");
      dump_expr (TREE_OPERAND (t, 1), 0);
      break;

    case CAST_EXPR:
      if (TREE_OPERAND (t, 0) == NULL_TREE
	  || TREE_CHAIN (TREE_OPERAND (t, 0)))
	{
	  dump_type (TREE_TYPE (t), 0);
	  OB_PUTC ('(');
	  dump_expr_list (TREE_OPERAND (t, 0));
	  OB_PUTC (')');
	}
      else
	{
	  OB_PUTC ('(');
	  dump_type (TREE_TYPE (t), 0);
	  OB_PUTC (')');
	  OB_PUTC ('(');
	  dump_expr_list (TREE_OPERAND (t, 0));
	  OB_PUTC (')');
	}
      break;

    case LOOKUP_EXPR:
      OB_PUTID (TREE_OPERAND (t, 0));
      break;

    case ARROW_EXPR:
      dump_expr (TREE_OPERAND (t, 0), nop);
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
	dump_type (TREE_OPERAND (t, 0), 0);
      else
	dump_unary_op ("*", t, 0);
      OB_PUTC (')');
      break;

    case DEFAULT_ARG:
      OB_PUTS ("{unparsed}");
      break;

    case TRY_CATCH_EXPR:
    case WITH_CLEANUP_EXPR:
    case CLEANUP_POINT_EXPR:
      dump_expr (TREE_OPERAND (t, 0), nop);
      break;

    case TEMPLATE_ID_EXPR:
      dump_decl (t, 0);
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
      OB_PUTCP ("{error}");
      break;
    }
}

static void
dump_binary_op (opstring, t)
     char *opstring;
     tree t;
{
  OB_PUTC ('(');
  dump_expr (TREE_OPERAND (t, 0), 1);
  OB_PUTC (' ');
  OB_PUTCP (opstring);
  OB_PUTC (' ');
  dump_expr (TREE_OPERAND (t, 1), 1);
  OB_PUTC (')');
}

static void
dump_unary_op (opstring, t, nop)
     char *opstring;
     tree t;
     int nop;
{
  if (!nop) OB_PUTC ('(');
  OB_PUTCP (opstring);
  dump_expr (TREE_OPERAND (t, 0), 1);
  if (!nop) OB_PUTC (')');
}

/* Print a function decl with exception specification included. */

char *
fndecl_as_string (fndecl, print_default_args_p)
     tree fndecl;
     int print_default_args_p;
{
  OB_INIT ();

  dump_function_decl (fndecl, 2 + print_default_args_p);
  
  OB_FINISH ();

  return (char *)obstack_base (&scratch_obstack);
}

/* Same, but handle a _TYPE.
   Called from convert_to_reference, mangle_class_name_for_template,
   build_unary_op, and GNU_xref_decl.  If CANONICAL_NAME is non-zero,
   when describing a typedef, we use the name of the type described,
   rather than the name of the typedef.  */

char *
type_as_string_real (typ, v, canonical_name)
     tree typ;
     int v;
     int canonical_name;
{
  OB_INIT ();

  dump_type_real (typ, v, canonical_name);

  OB_FINISH ();

  return (char *)obstack_base (&scratch_obstack);
}


char *
type_as_string (typ, v)
     tree typ;
     int v;
{
  return type_as_string_real (typ, v, 0);
}

char *
expr_as_string (decl, v)
     tree decl;
     int v ATTRIBUTE_UNUSED;
{
  OB_INIT ();

  dump_expr (decl, 1);

  OB_FINISH ();

  return (char *)obstack_base (&scratch_obstack);
}

/* A cross between type_as_string and fndecl_as_string.
   Only called from substitute_nice_name.  */

char *
decl_as_string (decl, v)
     tree decl;
     int v;
{
  OB_INIT ();

  dump_decl (decl, v);

  OB_FINISH ();

  return (char *)obstack_base (&scratch_obstack);
}

/* Generate the three forms of printable names for lang_printable_name.  */

char *
lang_decl_name (decl, v)
     tree decl;
     int v;
{
  if (v >= 2)
    return decl_as_string (decl, 1);

  OB_INIT ();

  if (v == 1 && DECL_CLASS_SCOPE_P (decl))
    {
      tree cname;
      if (TREE_CODE (decl) == FUNCTION_DECL)
	cname = DECL_CLASS_CONTEXT (decl);
      else
	cname = DECL_CONTEXT (decl);
      dump_type (cname, 0);
      OB_PUTC2 (':', ':');
    }

  if (TREE_CODE (decl) == FUNCTION_DECL)
    dump_function_name (decl);
  else
    dump_decl (DECL_NAME (decl), 0);

  OB_FINISH ();

  return (char *)obstack_base (&scratch_obstack);
}
  

char *
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

char *
code_as_string (c, v)
     enum tree_code c;
     int v ATTRIBUTE_UNUSED;
{
  return tree_code_name [c];
}

char *
language_as_string (c, v)
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

char *
parm_as_string (p, v)
     int p;
     int v ATTRIBUTE_UNUSED;
{
  if (p < 0)
    return "`this'";

  sprintf (digit_buffer, "%d", p+1);
  return digit_buffer;
}

char *
op_as_string (p, v)
     enum tree_code p;
     int v ATTRIBUTE_UNUSED;
{
  static char buf[] = "operator                ";

  if (p == 0)
    return "{unknown}";
  
  strcpy (buf + 9, opname_tab [p]);
  return buf;
}

char *
assop_as_string (p, v)
     enum tree_code p;
     int v ATTRIBUTE_UNUSED;
{
  static char buf[] = "operator                ";

  if (p == 0)
    return "{unknown}";
  
  strcpy (buf + 9, assignop_tab [p]);
  return buf;
}

char *
args_as_string (p, v)
     tree p;
     int v;
{
  if (p == NULL_TREE)
    return "";

  if (TREE_CODE_CLASS (TREE_CODE (TREE_VALUE (p))) == 't')
    return type_as_string (p, v);

  OB_INIT ();
  for (; p; p = TREE_CHAIN (p))
    {
      if (TREE_VALUE (p) == null_node)
	OB_PUTS ("NULL");
      else
	dump_type (error_type (TREE_VALUE (p)), v);
      if (TREE_CHAIN (p))
	OB_PUTS (", ");
    }
  OB_FINISH ();
  return (char *)obstack_base (&scratch_obstack);
}

char *
cv_as_string (p, v)
     tree p;
     int v ATTRIBUTE_UNUSED;
{
  OB_INIT ();

  dump_qualifiers (p, before);

  OB_FINISH ();

  return (char *)obstack_base (&scratch_obstack);
}
