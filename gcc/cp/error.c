/* Call-backs for C++ error reporting.
   This code is non-reentrant.
   Copyright (C) 1993, 1994, 1995, 1996 Free Software Foundation, Inc.

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
#include <stdio.h>
#include "tree.h"
#include "cp-tree.h"
#include "obstack.h"
#include <ctype.h>

typedef char* cp_printer ();

#define A args_as_string
#define C code_as_string
#define D decl_as_string
#define E expr_as_string
#define L language_as_string
#define O op_as_string
#define P parm_as_string
#define Q assop_as_string
#define T type_as_string
#define V cv_as_string

#define _ (cp_printer *) 0
cp_printer * cp_printers[256] =
{ 
/*0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F */
  _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, /* 0x00 */
  _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, /* 0x10 */
  _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, /* 0x20 */
  _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, /* 0x30 */
  _, A, _, C, D, E, _, _, _, _, _, _, L, _, _, O, /* 0x40 */
  P, Q, _, _, T, _, V, _, _, _, _, _, _, _, _, _, /* 0x50 */
  _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, /* 0x60 */
  _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, /* 0x70 */
};
#undef C
#undef D
#undef E
#undef L
#undef O
#undef P
#undef Q
#undef T
#undef V
#undef _

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
# define OB_PUTI(CST) do { sprintf (digit_buffer, "%d", (CST)); \
			   OB_PUTCP (digit_buffer); } while (0)
# define OB_UNPUT(N) obstack_blank (&scratch_obstack, - (N));

# define NEXT_CODE(t) (TREE_CODE (TREE_TYPE (t)))

enum pad { none, before, after };

static void dump_type PROTO((tree, int));
static void dump_decl PROTO((tree, int));
static void dump_function_decl PROTO((tree, int));
static void dump_expr PROTO((tree, int));
static void dump_unary_op PROTO((char *, tree, int));
static void dump_binary_op PROTO((char *, tree));
static void dump_aggr_type PROTO((tree, int));
static void dump_type_prefix PROTO((tree, int));
static void dump_type_suffix PROTO((tree, int));
static void dump_function_name PROTO((tree));
static void dump_expr_list PROTO((tree));
static void dump_global_iord PROTO((tree));
static void dump_readonly_or_volatile PROTO((tree, enum pad));
static void dump_char PROTO((int));
static char *aggr_variety PROTO((tree));
static tree ident_fndecl PROTO((tree));

void
init_error ()
{
  gcc_obstack_init (&scratch_obstack);
  scratch_firstobj = (char *)obstack_alloc (&scratch_obstack, 0);
}

static void
dump_readonly_or_volatile (t, p)
     tree t;
     enum pad p;
{
  if (TYPE_READONLY (t) || TYPE_VOLATILE (t))
    {
      if (p == before) OB_PUTC (' ');
      if (TYPE_READONLY (t))
	OB_PUTS ("const");
      if (TYPE_READONLY (t) && TYPE_VOLATILE (t))
	OB_PUTC (' ');
      if (TYPE_VOLATILE (t))
	OB_PUTS ("volatile");
      if (p == after) OB_PUTC (' ');
    }
}

/* This must be large enough to hold any printed integer or floating-point
   value.  */
static char digit_buffer[128];

/* Dump into the obstack a human-readable equivalent of TYPE.  */

static void
dump_type (t, v)
     tree t;
     int v;			/* verbose? */
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
      /* i.e. function taking no arguments */
      if (t != void_list_node)
	{
	  dump_type (TREE_VALUE (t), v);
	  /* Can this happen other than for default arguments? */
	  if (TREE_PURPOSE (t) && v)
	    {
	      OB_PUTS (" = ");
	      dump_expr (TREE_PURPOSE (t), 0);
	    }
	  if (TREE_CHAIN (t))
	    {
	      if (TREE_CHAIN (t) != void_list_node)
		{
		  OB_PUTC2 (',', ' ');
		  dump_type (TREE_CHAIN (t), v);
		}
	    }
	  else OB_PUTS (" ...");
	}
      break;

    case IDENTIFIER_NODE:
      OB_PUTID (t);
      break;

    case TREE_VEC:
      dump_type (BINFO_TYPE (t), v);
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
    case ENUMERAL_TYPE:
      if (TYPE_LANG_SPECIFIC (t)
	  && (IS_SIGNATURE_POINTER (t) || IS_SIGNATURE_REFERENCE (t)))
	{
	  if (TYPE_READONLY (t) | TYPE_VOLATILE (t))
	    dump_readonly_or_volatile (t, after);
	  dump_type (SIGNATURE_TYPE (t), v);
	  if (IS_SIGNATURE_POINTER (t))
	    OB_PUTC ('*');
	  else
	    OB_PUTC ('&');
	}
      else
	dump_aggr_type (t, v);
      break;

    case TYPE_DECL:
      dump_decl (t, v);
      break;

    case COMPLEX_TYPE:
      OB_PUTS ("complex ");
      dump_type (TREE_TYPE (t), v);
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
      dump_readonly_or_volatile (t, after);
      OB_PUTID (TYPE_IDENTIFIER (t));
      break;

    case TEMPLATE_TYPE_PARM:
      dump_readonly_or_volatile (t, after);
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
      dump_type_prefix (t, v);
      dump_type_suffix (t, v);
      break;

    case TYPENAME_TYPE:
      OB_PUTS ("typename ");
      dump_type (TYPE_CONTEXT (t), 0);
      OB_PUTS ("::");
      OB_PUTID (TYPE_IDENTIFIER (t));
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

/* Print out a class declaration, in the form `class foo'.  */

static void
dump_aggr_type (t, v)
     tree t;
     int v;			/* verbose? */
{
  tree name;
  char *variety = aggr_variety (t);

  dump_readonly_or_volatile (t, after);

  if (v > 0)
    {
      OB_PUTCP (variety);
      OB_PUTC (' ');
    }
  
  name = TYPE_NAME (t);

  if (name && DECL_CONTEXT (name))
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
dump_type_prefix (t, v)
     tree t;
     int v;			/* verbosity */
{
  if (TYPE_PTRMEMFUNC_P (t))
    {
      t = TYPE_PTRMEMFUNC_FN_TYPE (t);
      goto offset_type;
    }
  
  switch (TREE_CODE (t))
    {
    case POINTER_TYPE:
      {
	tree sub = TREE_TYPE (t);
	
	dump_type_prefix (sub, v);
	/* A tree for a member pointer looks like pointer to offset,
	   so let the OFFSET_TYPE case handle it.  */
	if (TREE_CODE (sub) != OFFSET_TYPE)
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
		if (! (TYPE_READONLY (sub) || TYPE_VOLATILE (sub)))
		  break;
		/* But we do want "char *const *" */
		
	      default:
		OB_PUTC (' ');
	      }
	    OB_PUTC ('*');
	    dump_readonly_or_volatile (t, none);
	  }
      }
      break;

    case REFERENCE_TYPE:
      {
	tree sub = TREE_TYPE (t);
	dump_type_prefix (sub, v);

	switch (TREE_CODE (sub))
	  {
	  case ARRAY_TYPE:
	    OB_PUTC2 (' ', '(');
	    break;

	  case POINTER_TYPE:
	    /* We don't want "char * &" */
	    if (! (TYPE_READONLY (sub) || TYPE_VOLATILE (sub)))
	      break;
	    /* But we do want "char *const &" */

	  default:
	    OB_PUTC (' ');
	  }
      }
      OB_PUTC ('&');
      dump_readonly_or_volatile (t, none);
      break;

    case OFFSET_TYPE:
    offset_type:
      dump_type_prefix (TREE_TYPE (t), v);
      if (TREE_CODE (t) == OFFSET_TYPE)	/* pmfs deal with this in d_t_p */
	{
	  OB_PUTC (' ');
	  dump_type (TYPE_OFFSET_BASETYPE (t), 0);
	  OB_PUTC2 (':', ':');
	}
      OB_PUTC ('*');
      dump_readonly_or_volatile (t, none);
      break;

      /* Can only be reached through function pointer -- this would not be
         correct if FUNCTION_DECLs used it.  */
    case FUNCTION_TYPE:
      dump_type_prefix (TREE_TYPE (t), v);
      OB_PUTC2 (' ', '(');
      break;

    case METHOD_TYPE:
      dump_type_prefix (TREE_TYPE (t), v);
      OB_PUTC2 (' ', '(');
      dump_aggr_type (TYPE_METHOD_BASETYPE (t), 0);
      OB_PUTC2 (':', ':');
      break;

    case ARRAY_TYPE:
      dump_type_prefix (TREE_TYPE (t), v);
      break;

    case ENUMERAL_TYPE:
    case ERROR_MARK:
    case IDENTIFIER_NODE:
    case INTEGER_TYPE:
    case BOOLEAN_TYPE:
    case REAL_TYPE:
    case RECORD_TYPE:
    case TEMPLATE_TYPE_PARM:
    case TREE_LIST:
    case TYPE_DECL:
    case TREE_VEC:
    case UNION_TYPE:
    case UNKNOWN_TYPE:
    case VOID_TYPE:
    case TYPENAME_TYPE:
    case COMPLEX_TYPE:
      dump_type (t, v);
      break;
      
    default:
      sorry ("`%s' not supported by dump_type_prefix",
	     tree_code_name[(int) TREE_CODE (t)]);
    }
}

static void
dump_type_suffix (t, v)
     tree t;
     int v;			/* verbose? */
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
      dump_type_suffix (TREE_TYPE (t), v);
      break;

      /* Can only be reached through function pointer */
    case FUNCTION_TYPE:
    case METHOD_TYPE:
      {
	tree arg;
	OB_PUTC2 (')', '(');
	arg = TYPE_ARG_TYPES (t);
	if (TREE_CODE (t) == METHOD_TYPE)
	  arg = TREE_CHAIN (arg);

	if (arg)
	  dump_type (arg, v);
	else
	  OB_PUTS ("...");
	OB_PUTC (')');
	if (TREE_CODE (t) == METHOD_TYPE)
	  dump_readonly_or_volatile
	    (TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (t))), before);
	dump_type_suffix (TREE_TYPE (t), v);
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
			      integer_one_node, 1)), 0);
	}
      OB_PUTC (']');
      dump_type_suffix (TREE_TYPE (t), v);
      break;
      
    case ENUMERAL_TYPE:
    case ERROR_MARK:
    case IDENTIFIER_NODE:
    case INTEGER_TYPE:
    case BOOLEAN_TYPE:
    case REAL_TYPE:
    case RECORD_TYPE:
    case TEMPLATE_TYPE_PARM:
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
	    dump_type (TREE_TYPE (t), v);
	    break;
	  }
      }
      if (v > 0)
	OB_PUTS ("typedef ");
      goto general;
      break;
      
    case VAR_DECL:
      if (DECL_NAME (t) && VTABLE_NAME_P (DECL_NAME (t)))
	{
	  OB_PUTS ("vtable for ");
	  dump_type (DECL_CONTEXT (t), v);
	  break;
	}
      /* else fall through */
    case FIELD_DECL:
    case PARM_DECL:
    general:
      if (v > 0)
	{
	  dump_type_prefix (TREE_TYPE (t), v);
	  OB_PUTC (' ');
	  dump_readonly_or_volatile (t, after);
	}
      /* DECL_CLASS_CONTEXT isn't being set in some cases.  Hmm...  */
      if (DECL_CONTEXT (t)
	  && TREE_CODE_CLASS (TREE_CODE (DECL_CONTEXT (t))) == 't')
	{
	  dump_type (DECL_CONTEXT (t), 0);
	  OB_PUTC2 (':', ':');
	}
      if (DECL_NAME (t))
	dump_decl (DECL_NAME (t), v);
      else
	OB_PUTS ("{anon}");
      if (v > 0)
	dump_type_suffix (TREE_TYPE (t), v);
      break;

    case NAMESPACE_DECL:
      OB_PUTID (DECL_NAME (t));
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
		    OB_PUTS ("class ");
		    OB_PUTID (DECL_NAME (arg));
		  }
		else
		  dump_decl (arg, 1);
		
		if (defval)
		  {
		    OB_PUTS (" = ");
		    dump_decl (defval, 1);
		  }
		
		OB_PUTC2 (',', ' ');
	      }
	    if (len != 0)
	      OB_UNPUT (2);
	    OB_PUTC2 ('>', ' ');
	  }
	nreverse(orig_args);

	if (TREE_CODE (DECL_TEMPLATE_RESULT (t)) == TYPE_DECL)
	  dump_type (TREE_TYPE (t), v);
	else if (TREE_TYPE (t) == NULL_TREE)
	   my_friendly_abort (353);
	else switch (NEXT_CODE (t))
	  {
	  case METHOD_TYPE:
	  case FUNCTION_TYPE:
	    dump_function_decl (t, v);
	    break;

	  default:
	    my_friendly_abort (353);
	  }
      }
      break;

    case LABEL_DECL:
      OB_PUTID (DECL_NAME (t));
      break;

    case CONST_DECL:
      if ((TREE_TYPE (t) != NULL_TREE && NEXT_CODE (t) == ENUMERAL_TYPE)
	  || (DECL_INITIAL (t) &&
	      TREE_CODE (DECL_INITIAL (t)) == TEMPLATE_CONST_PARM))
	goto general;
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

/* Pretty printing for announce_function.  T is the declaration of the
   function we are interested in seeing.  V is non-zero if we should print
   the type that this function returns.  */

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
  if (DECL_CONTEXT (t))
    cname = DECL_CLASS_CONTEXT (t);
  /* this is for partially instantiated template methods */
  else if (TREE_CODE (fntype) == METHOD_TYPE)
    cname = TREE_TYPE (TREE_VALUE (parmtypes));

  v = (v > 0);
  
  if (v)
    {
      if (DECL_STATIC_FUNCTION_P (t))
	OB_PUTS ("static ");
    
      if (! IDENTIFIER_TYPENAME_P (name)
	  && ! DECL_CONSTRUCTOR_P (t)
	  && ! DESTRUCTOR_NAME_P (name))
	{
	  dump_type_prefix (TREE_TYPE (fntype), 1);
	  OB_PUTC (' ');
	}
    }

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

  if (DESTRUCTOR_NAME_P (name) && DECL_LANGUAGE (t) == lang_cplusplus)
    parmtypes = TREE_CHAIN (parmtypes);
  
  dump_function_name (t);
  
  OB_PUTC ('(');

  if (parmtypes)
    dump_type (parmtypes, v);
  else
    OB_PUTS ("...");

  OB_PUTC (')');

  if (v && ! IDENTIFIER_TYPENAME_P (name))
    dump_type_suffix (TREE_TYPE (fntype), 1);

  if (TREE_CODE (fntype) == METHOD_TYPE)
    {
      if (IS_SIGNATURE (cname))
	/* We look at the type pointed to by the `optr' field of `this.'  */
	dump_readonly_or_volatile
	  (TREE_TYPE (TREE_TYPE (TYPE_FIELDS (TREE_VALUE (TYPE_ARG_TYPES (fntype))))), before);
      else
	dump_readonly_or_volatile
	  (TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (fntype))), before);
    }
}

/* Handle the function name for a FUNCTION_DECL node, grokking operators
   and destructors properly.  */

static void
dump_function_name (t)
     tree t;
{
  tree name = DECL_NAME (t);

  /* There ought to be a better way to find out whether or not something is
     a destructor.  */
  if (DESTRUCTOR_NAME_P (DECL_ASSEMBLER_NAME (t))
      && DECL_LANGUAGE (t) == lang_cplusplus)
    {
      OB_PUTC ('~');
      dump_decl (name, 0);
    }
  else if (IDENTIFIER_TYPENAME_P (name))
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

  if ((DECL_TEMPLATE_SPECIALIZATION (t) || DECL_IMPLICIT_INSTANTIATION (t))
      && (DECL_CLASS_CONTEXT (t) == NULL_TREE || is_member_template (t)))
    {
      tree args = DECL_TEMPLATE_INFO (t) 
	? DECL_TI_ARGS (t) : NULL_TREE; 

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
		      if (TREE_CODE_CLASS (TREE_CODE (a)) == 't')
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
		args = TREE_VEC_ELT (args, 0);

	      for (i = 0; i < TREE_VEC_LENGTH (args); i++)
		{
		  tree a = TREE_VEC_ELT (args, i);

		  if (need_comma)
		    OB_PUTS (", ");

		  if (a)
		    {
		      if (TREE_CODE_CLASS (TREE_CODE (a)) == 't')
			dump_type (a, 0);
		      else
			dump_expr (a, 0);
		    }
		  
		  need_comma = 1;
		}
	    }
	}
      OB_PUTC ('>');
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
      if (isprint (c))
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
	int i;
	strcpy (digit_buffer, "0x");
	for (i = 0; i < sizeof TREE_REAL_CST (t); i++)
	  sprintf (digit_buffer + 2 + 2*i, "%02x", *p++);
      }
#endif
      OB_PUTCP (digit_buffer);
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
	  PARM_DECL_EXPR (t) = 1;
	}
      else
	{
	  dump_expr (TREE_OPERAND (t, 0), 0);
	}
      break;

    case NEW_EXPR:
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
	      dump_expr (pfn, 0);
	      break;
	    }
	  if (TREE_CODE (idx) == INTEGER_CST
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
	if (TREE_CODE (ob) == NOP_EXPR
	    && TREE_OPERAND (ob, 0) == error_mark_node
	    && TREE_CODE (TREE_OPERAND (t, 1)) == FUNCTION_DECL)
	    /* A::f */
	  dump_expr (TREE_OPERAND (t, 1), 0);
	else
	  {
	    dump_expr (TREE_OPERAND (t, 0), 0);
	    OB_PUTS (" .* ");
	    dump_expr (TREE_OPERAND (t, 1), 0);
	  }
	break;
      }

    case TEMPLATE_CONST_PARM:
      {
	int l = current_template_parms ? 
	  list_length (current_template_parms) : 0;

	if (l >= TEMPLATE_CONST_LEVEL (t))
	  {
	    int i;
	    tree parms = current_template_parms;
	    tree r;
	    
	    for (i = 0; i < l - TEMPLATE_CONST_LEVEL (t); ++i)
	      {
		parms = TREE_CHAIN (parms);
		my_friendly_assert (parms != NULL_TREE, 0);
	      }
	    
	    r = TREE_VEC_ELT (TREE_VALUE (parms),
			      TEMPLATE_CONST_IDX (t));
	    dump_decl (TREE_VALUE (r), -1);
	  }
	else
	  {
	    OB_PUTS ("<tparm ");
	    OB_PUTI (TEMPLATE_CONST_IDX (t));
	    OB_PUTS (">");
	  }
      }
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

    case SIZEOF_EXPR:
      OB_PUTS ("sizeof (");
      if (TREE_CODE_CLASS (TREE_CODE (TREE_OPERAND (t, 0))) == 't')
	dump_type (TREE_OPERAND (t, 0), 0);
      else
	dump_unary_op ("*", t, 0);
      OB_PUTC (')');
      break;

    case DEFAULT_ARG:
      OB_PUTS ("{unparsed}");
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
    error:
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

char *
fndecl_as_string (fndecl, print_ret_type_p)
     tree fndecl;
     int print_ret_type_p;
{
  return decl_as_string (fndecl, print_ret_type_p);
}

/* Same, but handtype a _TYPE.
   Called from convert_to_reference, mangle_class_name_for_template,
   build_unary_op, and GNU_xref_decl.  */

char *
type_as_string (typ, v)
     tree typ;
     int v;
{
  OB_INIT ();

  dump_type (typ, v);

  OB_FINISH ();

  return (char *)obstack_base (&scratch_obstack);
}

char *
expr_as_string (decl, v)
     tree decl;
     int v;
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

  if (v == 1 && DECL_CONTEXT (decl)
      && TREE_CODE_CLASS (TREE_CODE (DECL_CONTEXT (decl))) == 't')
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
  if (TREE_CODE (t) == PARM_DECL)
    return DECL_SOURCE_FILE (DECL_CONTEXT (t));
  else if (TREE_CODE_CLASS (TREE_CODE (t)) == 't')
    return DECL_SOURCE_FILE (TYPE_MAIN_DECL (t));
  else
    return DECL_SOURCE_FILE (t);
}

int
cp_line_of (t)
     tree t;
{
  int line = 0;
  if (TREE_CODE (t) == PARM_DECL)
    line = DECL_SOURCE_LINE (DECL_CONTEXT (t));
  if (TREE_CODE (t) == TYPE_DECL && DECL_ARTIFICIAL (t))
    t = TREE_TYPE (t);

  if (TREE_CODE_CLASS (TREE_CODE (t)) == 't')
    line = DECL_SOURCE_LINE (TYPE_MAIN_DECL (t));
  else
    line = DECL_SOURCE_LINE (t);

  if (line == 0)
    return lineno;

  return line;
}

char *
code_as_string (c, v)
     enum tree_code c;
     int v;
{
  return tree_code_name [c];
}

char *
language_as_string (c, v)
     enum languages c;
     int v;
{
  switch (c)
    {
    case lang_c:
      return "C";

    case lang_cplusplus:
      return "C++";

    default:
      my_friendly_abort (355);
      return 0;
    }
}

/* Return the proper printed version of a parameter to a C++ function.  */

char *
parm_as_string (p, v)
     int p, v;
{
  if (p < 0)
    return "`this'";

  sprintf (digit_buffer, "%d", p+1);
  return digit_buffer;
}

char *
op_as_string (p, v)
     enum tree_code p;
     int v;
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
     int v;
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
     int v;
{
  OB_INIT ();

  dump_readonly_or_volatile (p, before);

  OB_FINISH ();

  return (char *)obstack_base (&scratch_obstack);
}
