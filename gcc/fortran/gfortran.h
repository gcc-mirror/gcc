/* gfortran header file
   Copyright (C) 2000, 2001, 2002, 2003
   Free Software Foundation, Inc.
   Contributed by Andy Vaught

This file is part of GNU G95.

GNU G95 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU G95 is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU G95; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef GCC_GFORTRAN_H
#define GCC_GFORTRAN_H

/* It's probably insane to have this large of a header file, but it
   seemed like everything had to be recompiled anyway when a change
   was made to a header file, and there were ordering issues with
   multiple header files.  Besides, Microsoft's winnt.h was 250k last
   time I looked, so by comparison this is perfectly reasonable.  */

/* We need system.h for HOST_WIDE_INT. Including hwint.h by itself doesn't
   seem to be sufficient on some systems.  */
#include "system.h"
#include "coretypes.h"

/* The following ifdefs are recommended by the autoconf documentation
   for any code using alloca.  */

/* AIX requires this to be the first thing in the file.  */
#ifdef __GNUC__
#else /* not __GNUC__ */
#ifdef HAVE_ALLOCA_H
#include <alloca.h>
#else /* do not HAVE_ALLOCA_H */
#ifdef _AIX
#pragma alloca
#else
#ifndef alloca			/* predefined by HP cc +Olibcalls */
char *alloca ();
#endif /* not predefined */
#endif /* not _AIX */
#endif /* do not HAVE_ALLOCA_H */
#endif /* not __GNUC__ */


#include <stdio.h>		/* need FILE * here */

/* Major control parameters.  */

#define GFC_VERSION "0.23"
#define GFC_MAX_SYMBOL_LEN 63
#define GFC_REAL_BITS 100	/* Number of bits in g95's floating point numbers.  */
#define GFC_MAX_LINE 132	/* Characters beyond this are not seen.  */
#define GFC_MAX_DIMENSIONS 7	/* Maximum dimensions in an array.  */
#define GFC_LETTERS 26		/* Number of letters in the alphabet.  */
#define MAX_ERROR_MESSAGE 1000	/* Maximum length of an error message.  */

#define free(x) Use_gfc_free_instead_of_free()
#define gfc_is_whitespace(c) ((c==' ') || (c=='\t'))

#ifndef NULL
#define NULL ((void *) 0)
#endif

/* Stringization.  */
#define stringize(x) expand_macro(x)
#define expand_macro(x) # x

/* For a the runtime library, a standard prefix is a requirement to
   avoid cluttering the namespace with things nobody asked for.  It's
   ugly to look at and a pain to type when you add the prefix by hand,
   so we hide it behind a macro.  */
#define PREFIX(x) "_gfortran_" x

/* Macro to initialize an mstring structure.  */
#define minit(s, t) { s, NULL, t }

/* Structure for storing strings to be matched by gfc_match_string.  */
typedef struct
{
  const char *string;
  const char *mp;
  int tag;
}
mstring;


/* Flags to specify which standardi/extension contains a feature.  */
#define GFC_STD_GNU		(1<<5)	/* GNU Fortran extension.  */
#define GFC_STD_F2003		(1<<4)	/* New in F2003.  */
#define GFC_STD_F2003_DEL	(1<<3)	/* Deleted in F2003.  */
#define GFC_STD_F2003_OBS	(1<<2)	/* Obsoleted in F2003.  */
#define GFC_STD_F95_DEL		(1<<1)	/* Deleted in F95.  */
#define GFC_STD_F95_OBS		(1<<0)	/* Obsoleted in F95.  */

/*************************** Enums *****************************/

/* The author remains confused to this day about the convention of
   returning '0' for 'SUCCESS'... or was it the other way around?  The
   following enum makes things much more readable.  We also start
   values off at one instead of zero.  */

typedef enum
{ SUCCESS = 1, FAILURE }
try;

/* Matchers return one of these three values.  The difference between
   MATCH_NO and MATCH_ERROR is that MATCH_ERROR means that a match was
   successful, but that something non-syntactic is wrong and an error
   has already been issued.  */

typedef enum
{ MATCH_NO = 1, MATCH_YES, MATCH_ERROR }
match;

typedef enum
{ FORM_FREE, FORM_FIXED, FORM_UNKNOWN }
gfc_source_form;

typedef enum
{ BT_UNKNOWN = 1, BT_INTEGER, BT_REAL, BT_COMPLEX,
  BT_LOGICAL, BT_CHARACTER, BT_DERIVED, BT_PROCEDURE
}
bt;

/* Expression node types.  */
typedef enum
{ EXPR_OP = 1, EXPR_FUNCTION, EXPR_CONSTANT, EXPR_VARIABLE,
  EXPR_SUBSTRING, EXPR_STRUCTURE, EXPR_ARRAY, EXPR_NULL
}
expr_t;

/* Array types.  */
typedef enum
{ AS_EXPLICIT = 1, AS_ASSUMED_SHAPE, AS_DEFERRED,
  AS_ASSUMED_SIZE, AS_UNKNOWN
}
array_type;

typedef enum
{ AR_FULL = 1, AR_ELEMENT, AR_SECTION, AR_UNKNOWN }
ar_type;

/* Statement label types.  */
typedef enum
{ ST_LABEL_UNKNOWN = 1, ST_LABEL_TARGET,
  ST_LABEL_BAD_TARGET, ST_LABEL_FORMAT
}
gfc_sl_type;

/* Intrinsic operators.  */
typedef enum
{ GFC_INTRINSIC_BEGIN = 0,
  INTRINSIC_NONE = -1, INTRINSIC_UPLUS = GFC_INTRINSIC_BEGIN,
  INTRINSIC_UMINUS, INTRINSIC_PLUS, INTRINSIC_MINUS, INTRINSIC_TIMES,
  INTRINSIC_DIVIDE, INTRINSIC_POWER, INTRINSIC_CONCAT,
  INTRINSIC_AND, INTRINSIC_OR, INTRINSIC_EQV, INTRINSIC_NEQV,
  INTRINSIC_EQ, INTRINSIC_NE, INTRINSIC_GT, INTRINSIC_GE,
  INTRINSIC_LT, INTRINSIC_LE, INTRINSIC_NOT, INTRINSIC_USER,
  INTRINSIC_ASSIGN,
  GFC_INTRINSIC_END /* Sentinel */
}
gfc_intrinsic_op;


/* Strings for all intrinsic operators.  */
extern mstring intrinsic_operators[];


/* This macro is the number of intrinsic operators that exist.
   Assumptions are made about the numbering of the interface_op enums.  */
#define GFC_INTRINSIC_OPS GFC_INTRINSIC_END

/* Arithmetic results.  */
typedef enum
{ ARITH_OK = 1, ARITH_OVERFLOW, ARITH_UNDERFLOW,
  ARITH_DIV0, ARITH_0TO0, ARITH_INCOMMENSURATE
}
arith;

/* Statements.  */
typedef enum
{
  ST_ARITHMETIC_IF, ST_ALLOCATE, ST_ATTR_DECL, ST_BACKSPACE, ST_BLOCK_DATA,
  ST_CALL, ST_CASE, ST_CLOSE, ST_COMMON, ST_CONTINUE, ST_CONTAINS, ST_CYCLE,
  ST_DATA, ST_DATA_DECL, ST_DEALLOCATE, ST_DO, ST_ELSE, ST_ELSEIF,
  ST_ELSEWHERE, ST_END_BLOCK_DATA, ST_ENDDO, ST_IMPLIED_ENDDO,
  ST_END_FILE, ST_END_FORALL, ST_END_FUNCTION, ST_ENDIF, ST_END_INTERFACE,
  ST_END_MODULE, ST_END_PROGRAM, ST_END_SELECT, ST_END_SUBROUTINE,
  ST_END_WHERE, ST_END_TYPE, ST_ENTRY, ST_EQUIVALENCE, ST_EXIT, ST_FORALL,
  ST_FORALL_BLOCK, ST_FORMAT, ST_FUNCTION, ST_GOTO, ST_IF_BLOCK, ST_IMPLICIT,
  ST_IMPLICIT_NONE, ST_INQUIRE, ST_INTERFACE, ST_PARAMETER, ST_MODULE,
  ST_MODULE_PROC, ST_NAMELIST, ST_NULLIFY, ST_OPEN, ST_PAUSE, ST_PRIVATE,
  ST_PROGRAM, ST_PUBLIC, ST_READ, ST_RETURN, ST_REWIND, ST_STOP,
  ST_SUBROUTINE,
  ST_TYPE, ST_USE, ST_WHERE_BLOCK, ST_WHERE, ST_WRITE, ST_ASSIGNMENT,
  ST_POINTER_ASSIGNMENT, ST_SELECT_CASE, ST_SEQUENCE, ST_SIMPLE_IF,
  ST_STATEMENT_FUNCTION, ST_DERIVED_DECL, ST_LABEL_ASSIGNMENT, ST_NONE
}
gfc_statement;


/* Types of interfaces that we can have.  Assignment interfaces are
   considered to be intrinsic operators.  */
typedef enum
{
  INTERFACE_NAMELESS = 1, INTERFACE_GENERIC,
  INTERFACE_INTRINSIC_OP, INTERFACE_USER_OP
}
interface_type;

/* Symbol flavors: these are all mutually exclusive.
   10 elements = 4 bits.  */
typedef enum
{
  FL_UNKNOWN = 0, FL_PROGRAM, FL_BLOCK_DATA, FL_MODULE, FL_VARIABLE,
  FL_PARAMETER, FL_LABEL, FL_PROCEDURE, FL_DERIVED, FL_NAMELIST
}
sym_flavor;

/* Procedure types.  7 elements = 3 bits.  */
typedef enum
{ PROC_UNKNOWN, PROC_MODULE, PROC_INTERNAL, PROC_DUMMY,
  PROC_INTRINSIC, PROC_ST_FUNCTION, PROC_EXTERNAL
}
procedure_type;

/* Intent types.  */
typedef enum
{ INTENT_UNKNOWN = 0, INTENT_IN, INTENT_OUT, INTENT_INOUT
}
sym_intent;

/* Access types.  */
typedef enum
{ ACCESS_PUBLIC = 1, ACCESS_PRIVATE, ACCESS_UNKNOWN
}
gfc_access;

/* Flags to keep track of where an interface came from.
   4 elements = 2 bits.  */
typedef enum
{ IFSRC_UNKNOWN = 0, IFSRC_DECL, IFSRC_IFBODY, IFSRC_USAGE
}
ifsrc;

/* Strings for all symbol attributes.  We use these for dumping the
   parse tree, in error messages, and also when reading and writing
   modules.  In symbol.c.  */
extern const mstring flavors[];
extern const mstring procedures[];
extern const mstring intents[];
extern const mstring access_types[];
extern const mstring ifsrc_types[];

/* Enumeration of all the generic intrinsic functions.  Used by the
   backend for identification of a function.  */

enum gfc_generic_isym_id
{
  /* GFC_ISYM_NONE is used for intrinsics which will never be seen by
     the backend (eg. KIND).  */
  GFC_ISYM_NONE = 0,
  GFC_ISYM_ABS,
  GFC_ISYM_ACHAR,
  GFC_ISYM_ACOS,
  GFC_ISYM_ADJUSTL,
  GFC_ISYM_ADJUSTR,
  GFC_ISYM_AIMAG,
  GFC_ISYM_AINT,
  GFC_ISYM_ALL,
  GFC_ISYM_ALLOCATED,
  GFC_ISYM_ANINT,
  GFC_ISYM_ANY,
  GFC_ISYM_ASIN,
  GFC_ISYM_ASSOCIATED,
  GFC_ISYM_ATAN,
  GFC_ISYM_ATAN2,
  GFC_ISYM_BTEST,
  GFC_ISYM_CEILING,
  GFC_ISYM_CHAR,
  GFC_ISYM_CMPLX,
  GFC_ISYM_CONJG,
  GFC_ISYM_COS,
  GFC_ISYM_COSH,
  GFC_ISYM_COUNT,
  GFC_ISYM_CSHIFT,
  GFC_ISYM_DBLE,
  GFC_ISYM_DIM,
  GFC_ISYM_DOT_PRODUCT,
  GFC_ISYM_DPROD,
  GFC_ISYM_EOSHIFT,
  GFC_ISYM_EXP,
  GFC_ISYM_EXPONENT,
  GFC_ISYM_FLOOR,
  GFC_ISYM_FRACTION,
  GFC_ISYM_IACHAR,
  GFC_ISYM_IAND,
  GFC_ISYM_IBCLR,
  GFC_ISYM_IBITS,
  GFC_ISYM_IBSET,
  GFC_ISYM_ICHAR,
  GFC_ISYM_IEOR,
  GFC_ISYM_INDEX,
  GFC_ISYM_INT,
  GFC_ISYM_IOR,
  GFC_ISYM_ISHFT,
  GFC_ISYM_ISHFTC,
  GFC_ISYM_LBOUND,
  GFC_ISYM_LEN,
  GFC_ISYM_LEN_TRIM,
  GFC_ISYM_LGE,
  GFC_ISYM_LGT,
  GFC_ISYM_LLE,
  GFC_ISYM_LLT,
  GFC_ISYM_LOG,
  GFC_ISYM_LOG10,
  GFC_ISYM_LOGICAL,
  GFC_ISYM_MATMUL,
  GFC_ISYM_MAX,
  GFC_ISYM_MAXLOC,
  GFC_ISYM_MAXVAL,
  GFC_ISYM_MERGE,
  GFC_ISYM_MIN,
  GFC_ISYM_MINLOC,
  GFC_ISYM_MINVAL,
  GFC_ISYM_MOD,
  GFC_ISYM_MODULO,
  GFC_ISYM_NEAREST,
  GFC_ISYM_NINT,
  GFC_ISYM_NOT,
  GFC_ISYM_PACK,
  GFC_ISYM_PRESENT,
  GFC_ISYM_PRODUCT,
  GFC_ISYM_REAL,
  GFC_ISYM_REPEAT,
  GFC_ISYM_RESHAPE,
  GFC_ISYM_RRSPACING,
  GFC_ISYM_SCALE,
  GFC_ISYM_SCAN,
  GFC_ISYM_SET_EXPONENT,
  GFC_ISYM_SHAPE,
  GFC_ISYM_SI_KIND,
  GFC_ISYM_SIGN,
  GFC_ISYM_SIN,
  GFC_ISYM_SINH,
  GFC_ISYM_SIZE,
  GFC_ISYM_SPACING,
  GFC_ISYM_SPREAD,
  GFC_ISYM_SQRT,
  GFC_ISYM_SR_KIND,
  GFC_ISYM_SUM,
  GFC_ISYM_TAN,
  GFC_ISYM_TANH,
  GFC_ISYM_TRANSFER,
  GFC_ISYM_TRANSPOSE,
  GFC_ISYM_TRIM,
  GFC_ISYM_UBOUND,
  GFC_ISYM_UNPACK,
  GFC_ISYM_VERIFY,
  GFC_ISYM_CONVERSION
};
typedef enum gfc_generic_isym_id gfc_generic_isym_id;

/************************* Structures *****************************/

/* Symbol attribute structure.  */
typedef struct
{
  /* Variable attributes.  */
  unsigned allocatable:1, dimension:1, external:1, intrinsic:1,
    optional:1, pointer:1, save:1, target:1,
    dummy:1, common:1, result:1, entry:1, assign:1;

  unsigned data:1,		/* Symbol is named in a DATA statement.  */
    use_assoc:1;		/* Symbol has been use-associated.  */

  unsigned in_namelist:1, in_common:1, saved_common:1;
  unsigned function:1, subroutine:1, generic:1;
  unsigned implicit_type:1;	/* Type defined via implicit rules */

  /* Function/subroutine attributes */
  unsigned sequence:1, elemental:1, pure:1, recursive:1;
  unsigned unmaskable:1, masked:1, contained:1;

  /* Set if a function must always be referenced by an explicit interface.  */
  unsigned always_explicit:1;

  /* Set if the symbol has been referenced in an expression.  No further
     modification of type or type parameters is permitted.  */
  unsigned referenced:1;

  /* Mutually exclusive multibit attributes.  */
  gfc_access access:2;
  sym_intent intent:2;
  sym_flavor flavor:4;
  ifsrc if_source:2;

  procedure_type proc:3;

}
symbol_attribute;


typedef struct
{
  char *nextc;
  int line;			/* line within the lp structure */
  struct linebuf *lp;
  struct gfc_file *file;
}
locus;

/* The linebuf structure deserves some explanation.  This is the
   primary structure for holding lines.  A source file is stored in a
   singly linked list of these structures.  Each structure holds an
   integer number of lines.  The line[] member is actually an array of
   pointers that point to the NULL-terminated lines.  This list grows
   upwards, and the actual lines are stored at the top of the
   structure and grow downward.  Each structure is packed with as many
   lines as it can hold, then another linebuf is allocated.  */

/* Chosen so that sizeof(linebuf) = 4096 on most machines */
#define LINEBUF_SIZE 4080

typedef struct linebuf
{
  int start_line, lines;
  struct linebuf *next;
  char *line[1];
  char buf[LINEBUF_SIZE];
}
linebuf;


#include <limits.h>
#ifndef PATH_MAX
# include <sys/param.h>
# define PATH_MAX MAXPATHLEN
#endif


typedef struct gfc_file
{
  char filename[PATH_MAX + 1];
  gfc_source_form form;
  struct gfc_file *included_by, *next;
  locus loc;
  struct linebuf *start;
}
gfc_file;


extern int gfc_suppress_error;


/* Character length structures hold the expression that gives the
   length of a character variable.  We avoid putting these into
   gfc_typespec because doing so prevents us from doing structure
   copies and forces us to deallocate any typespecs we create, as well
   as structures that contain typespecs.  They also can have multiple
   character typespecs pointing to them.

   These structures form a singly linked list within the current
   namespace and are deallocated with the namespace.  It is possible to
   end up with gfc_charlen structures that have nothing pointing to them.  */

typedef struct gfc_charlen
{
  struct gfc_expr *length;
  struct gfc_charlen *next;
  tree backend_decl;
}
gfc_charlen;

#define gfc_get_charlen() gfc_getmem(sizeof(gfc_charlen))

/* Type specification structure.  FIXME: derived and cl could be union???  */
typedef struct
{
  bt type;
  int kind;
  struct gfc_symbol *derived;
  gfc_charlen *cl;	/* For character types only.  */
}
gfc_typespec;

/* Array specification.  */
typedef struct
{
  int rank;	/* A rank of zero means that a variable is a scalar.  */
  array_type type;
  struct gfc_expr *lower[GFC_MAX_DIMENSIONS], *upper[GFC_MAX_DIMENSIONS];
}
gfc_array_spec;

#define gfc_get_array_spec() gfc_getmem(sizeof(gfc_array_spec))


/* Components of derived types.  */
typedef struct gfc_component
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_typespec ts;

  int pointer, dimension;
  gfc_array_spec *as;

  tree backend_decl;
  locus loc;
  struct gfc_expr *initializer;
  struct gfc_component *next;
}
gfc_component;

#define gfc_get_component() gfc_getmem(sizeof(gfc_component))

/* Formal argument lists are lists of symbols.  */
typedef struct gfc_formal_arglist
{
  struct gfc_symbol *sym;
  struct gfc_formal_arglist *next;
}
gfc_formal_arglist;

#define gfc_get_formal_arglist() gfc_getmem(sizeof(gfc_formal_arglist))


/* The gfc_actual_arglist structure is for actual arguments.  */
typedef struct gfc_actual_arglist
{
  char name[GFC_MAX_SYMBOL_LEN + 1];
  /* Alternate return label when the expr member is null.  */
  struct gfc_st_label *label;

  struct gfc_expr *expr;
  struct gfc_actual_arglist *next;
}
gfc_actual_arglist;

#define gfc_get_actual_arglist() gfc_getmem(sizeof(gfc_actual_arglist))


/* Because a symbol can belong to multiple namelists, they must be
   linked externally to the symbol itself.  */
typedef struct gfc_namelist
{
  struct gfc_symbol *sym;
  struct gfc_namelist *next;
}
gfc_namelist;

#define gfc_get_namelist() gfc_getmem(sizeof(gfc_namelist))


/* The gfc_st_label structure is a doubly linked list attached to a
   namespace that records the usage of statement labels within that
   space.  */
/* TODO: Make format/statement specifics a union.  */
typedef struct gfc_st_label
{
  int value;

  gfc_sl_type defined, referenced;

  struct gfc_expr *format;

  tree backend_decl;

  locus where;

  struct gfc_st_label *prev, *next;
}
gfc_st_label;


/* gfc_interface()-- Interfaces are lists of symbols strung together.  */
typedef struct gfc_interface
{
  struct gfc_symbol *sym;
  locus where;
  struct gfc_interface *next;
}
gfc_interface;

#define gfc_get_interface() gfc_getmem(sizeof(gfc_interface))


/* User operator nodes.  These are like stripped down symbols.  */
typedef struct
{
  char name[GFC_MAX_SYMBOL_LEN + 1];

  gfc_interface *operator;
  struct gfc_namespace *ns;
  gfc_access access;
}
gfc_user_op;

/* Symbol nodes.  These are important things.  They are what the
   standard refers to as "entities".  The possibly multiple names that
   refer to the same entity are accomplished by a binary tree of
   symtree structures that is balanced by the red-black method-- more
   than one symtree node can point to any given symbol.  */

typedef struct gfc_symbol
{
  char name[GFC_MAX_SYMBOL_LEN + 1];	/* Primary name, before renaming */
  char module[GFC_MAX_SYMBOL_LEN + 1];	/* Module this symbol came from */
  locus declared_at;

  gfc_typespec ts;
  symbol_attribute attr;

  /* The interface member points to the formal argument list if the
     symbol is a function or subroutine name.  If the symbol is a
     generic name, the generic member points to the list of
     interfaces.  */

  gfc_interface *generic;
  gfc_access component_access;

  gfc_formal_arglist *formal;
  struct gfc_namespace *formal_ns;

  struct gfc_expr *value;	/* Parameter/Initializer value */
  gfc_array_spec *as;
  struct gfc_symbol *result;	/* function result symbol */
  gfc_component *components;	/* Derived type components */

  /* TODO: These three fields are mutually exclusive.  */
  struct gfc_symbol *common_head, *common_next;	/* Links for COMMON syms */
  /* Make sure setup code for dummy arguments is generated in the correct
     order.  */
  int dummy_order;

  gfc_namelist *namelist, *namelist_tail;

  /* Change management fields.  Symbols that might be modified by the
     current statement have the mark member nonzero and are kept in a
     singly linked list through the tlink field.  Of these symbols,
     symbols with old_symbol equal to NULL are symbols created within
     the current statement.  Otherwise, old_symbol points to a copy of
     the old symbol.  */

  struct gfc_symbol *old_symbol, *tlink;
  unsigned mark:1, new:1;
  int refs;
  struct gfc_namespace *ns;	/* namespace containing this symbol */

  tree backend_decl;

}
gfc_symbol;


/* Within a namespace, symbols are pointed to by symtree nodes that
   are linked together in a balanced binary tree.  There can be
   several symtrees pointing to the same symbol node via USE
   statements.  */

#define BBT_HEADER(self) int priority; struct self *left, *right

typedef struct gfc_symtree
{
  BBT_HEADER (gfc_symtree);
  char name[GFC_MAX_SYMBOL_LEN + 1];
  int ambiguous;
  union
  {
    gfc_symbol *sym;		/* Symbol associated with this node */
    gfc_user_op *uop;
  }
  n;

}
gfc_symtree;


typedef struct gfc_namespace
{
  gfc_symtree *sym_root, *uop_root;	/* Roots of the red/black symbol trees */

  int set_flag[GFC_LETTERS];
  gfc_typespec default_type[GFC_LETTERS];	/* IMPLICIT typespecs */

  struct gfc_symbol *proc_name;
  gfc_interface *operator[GFC_INTRINSIC_OPS];
  struct gfc_namespace *parent, *contained, *sibling;
  struct gfc_code *code;
  gfc_symbol *blank_common;
  struct gfc_equiv *equiv;
  gfc_access default_access, operator_access[GFC_INTRINSIC_OPS];

  gfc_st_label *st_labels;
  struct gfc_data *data;

  gfc_charlen *cl_list;

  int save_all, seen_save;
}
gfc_namespace;

extern gfc_namespace *gfc_current_ns;


/* Information on interfaces being built.  */
typedef struct
{
  interface_type type;
  gfc_symbol *sym;
  gfc_namespace *ns;
  gfc_user_op *uop;
  gfc_intrinsic_op op;
}
gfc_interface_info;

extern gfc_interface_info current_interface;


/* Array reference.  */
typedef struct gfc_array_ref
{
  ar_type type;
  int dimen;			/* # of components in the reference */
  locus where;
  gfc_array_spec *as;

  locus c_where[GFC_MAX_DIMENSIONS];	/* All expressions can be NULL */
  struct gfc_expr *start[GFC_MAX_DIMENSIONS], *end[GFC_MAX_DIMENSIONS],
    *stride[GFC_MAX_DIMENSIONS];

  enum
  { DIMEN_ELEMENT = 1, DIMEN_RANGE, DIMEN_VECTOR, DIMEN_UNKNOWN }
  dimen_type[GFC_MAX_DIMENSIONS];

  struct gfc_expr *offset;
}
gfc_array_ref;

#define gfc_get_array_ref() gfc_getmem(sizeof(gfc_array_ref))


/* Component reference nodes.  A variable is stored as an expression
   node that points to the base symbol.  After that, a singly linked
   list of component reference nodes gives the variable's complete
   resolution.  The array_ref component may be present and comes
   before the component component.  */

typedef enum
  { REF_ARRAY, REF_COMPONENT, REF_SUBSTRING }
ref_type;

typedef struct gfc_ref
{
  ref_type type;

  union
  {
    struct gfc_array_ref ar;

    struct
    {
      gfc_component *component;
      gfc_symbol *sym;
    }
    c;

    struct
    {
      struct gfc_expr *start, *end;	/* Substring */
      gfc_charlen *length;
    }
    ss;

  }
  u;

  struct gfc_ref *next;
}
gfc_ref;

#define gfc_get_ref() gfc_getmem(sizeof(gfc_ref))


/* Structures representing intrinsic symbols and their arguments lists.  */
typedef struct gfc_intrinsic_arg
{
  char name[GFC_MAX_SYMBOL_LEN + 1];

  gfc_typespec ts;
  int optional;
  gfc_actual_arglist *actual;

  struct gfc_intrinsic_arg *next;

}
gfc_intrinsic_arg;


typedef union
{
  try (*f1)(struct gfc_expr *);
  try (*f1m)(gfc_actual_arglist *);
  try (*f2)(struct gfc_expr *, struct gfc_expr *);
  try (*f3)(struct gfc_expr *, struct gfc_expr *, struct gfc_expr *);
  try (*f4)(struct gfc_expr *, struct gfc_expr *, struct gfc_expr *,
	    struct gfc_expr *);
  try (*f5)(struct gfc_expr *, struct gfc_expr *, struct gfc_expr *,
	    struct gfc_expr *, struct gfc_expr *);
}
gfc_check_f;


typedef union
{
  struct gfc_expr *(*f1)(struct gfc_expr *);
  struct gfc_expr *(*f2)(struct gfc_expr *, struct gfc_expr *);
  struct gfc_expr *(*f3)(struct gfc_expr *, struct gfc_expr *,
			 struct gfc_expr *);
  struct gfc_expr *(*f4)(struct gfc_expr *, struct gfc_expr *,
			 struct gfc_expr *, struct gfc_expr *);
  struct gfc_expr *(*f5)(struct gfc_expr *, struct gfc_expr *,
			 struct gfc_expr *, struct gfc_expr *,
			 struct gfc_expr *);
  struct gfc_expr *(*cc)(struct gfc_expr *, bt, int);
}
gfc_simplify_f;


typedef union
{
  void (*f0)(struct gfc_expr *);
  void (*f1)(struct gfc_expr *, struct gfc_expr *);
  void (*f1m)(struct gfc_expr *, struct gfc_actual_arglist *);
  void (*f2)(struct gfc_expr *, struct gfc_expr *, struct gfc_expr *);
  void (*f3)(struct gfc_expr *, struct gfc_expr *, struct gfc_expr *,
	     struct gfc_expr *);
  void (*f4)(struct gfc_expr *, struct gfc_expr *, struct gfc_expr *,
	     struct gfc_expr *, struct gfc_expr *);
  void (*f5)(struct gfc_expr *, struct gfc_expr *, struct gfc_expr *,
	     struct gfc_expr *, struct gfc_expr *, struct gfc_expr *);
  void (*s1)(struct gfc_code *);
}
gfc_resolve_f;


typedef struct gfc_intrinsic_sym
{
  char name[GFC_MAX_SYMBOL_LEN + 1], lib_name[GFC_MAX_SYMBOL_LEN + 1];
  gfc_intrinsic_arg *formal;
  gfc_typespec ts;
  int elemental, pure, generic, specific, actual_ok;

  gfc_simplify_f simplify;
  gfc_check_f check;
  gfc_resolve_f resolve;
  struct gfc_intrinsic_sym *specific_head, *next;
  gfc_generic_isym_id generic_id;

}
gfc_intrinsic_sym;


/* Expression nodes.  The expression node types deserve explanations,
   since the last couple can be easily misconstrued:

   EXPR_OP         Operator node pointing to one or two other nodes
   EXPR_FUNCTION   Function call, symbol points to function's name
   EXPR_CONSTANT   A scalar constant: Logical, String, Real, Int or Complex
   EXPR_VARIABLE   An Lvalue with a root symbol and possible reference list
                   which expresses structure, array and substring refs.
   EXPR_NULL       The NULL pointer value (which also has a basic type).
   EXPR_SUBSTRING  A substring of a constant string
   EXPR_STRUCTURE  A structure constructor
   EXPR_ARRAY      An array constructor.  */

#include <gmp.h>

typedef struct gfc_expr
{
  expr_t expr_type;

  gfc_typespec ts;	/* These two refer to the overall expression */

  int rank;
  mpz_t *shape;		/* Can be NULL if shape is unknown at compile time */

  gfc_intrinsic_op operator;

  /* Nonnull for functions and structure constructors */
  gfc_symtree *symtree;

  gfc_user_op *uop;
  gfc_ref *ref;

  struct gfc_expr *op1, *op2;
  locus where;

  union
  {
    mpz_t integer;
    mpf_t real;
    int logical;

    struct
    {
      mpf_t r, i;
    }
    complex;

    struct
    {
      gfc_actual_arglist *actual;
      char *name;	/* Points to the ultimate name of the function */
      gfc_intrinsic_sym *isym;
      gfc_symbol *esym;
    }
    function;

    struct
    {
      int length;
      char *string;
    }
    character;

    struct gfc_constructor *constructor;
  }
  value;

}
gfc_expr;


#define gfc_get_shape(rank) ((mpz_t *) gfc_getmem(rank*sizeof(mpz_t)))

/* Structures for information associated with different kinds of
   numbers.  The first set of integer parameters define all there is
   to know about a particular kind.  The rest of the elements are
   computed from the first elements.  */

typedef struct
{
  int kind, radix, digits, bit_size;

  int range;
  mpz_t huge;

  mpz_t min_int, max_int;	/* Values really representable by the target */
}
gfc_integer_info;

extern gfc_integer_info gfc_integer_kinds[];


typedef struct
{
  int kind, bit_size;

}
gfc_logical_info;

extern gfc_logical_info gfc_logical_kinds[];


typedef struct
{
  int kind, radix, digits, min_exponent, max_exponent;

  int range, precision;
  mpf_t epsilon, huge, tiny;
}
gfc_real_info;

extern gfc_real_info gfc_real_kinds[];


/* Equivalence structures.  Equivalent lvalues are linked along the
   *eq pointer, equivalence sets are strung along the *next node.  */
typedef struct gfc_equiv
{
  struct gfc_equiv *next, *eq;
  gfc_expr *expr;
  int used;
}
gfc_equiv;

#define gfc_get_equiv() gfc_getmem(sizeof(gfc_equiv))


/* gfc_case stores the selector list of a case statement.  The *low
   and *high pointers can point to the same expression in the case of
   a single value.  If *high is NULL, the selection is from *low
   upwards, if *low is NULL the selection is *high downwards.

   This structure has separate fields to allow singe and double linked
   lists of CASEs the same time.  The singe linked list along the NEXT
   field is a list of cases for a single CASE label.  The double linked
   list along the LEFT/RIGHT fields is used to detect overlap and to
   build a table of the cases for SELECT constructs with a CHARACTER
   case expression.  */

typedef struct gfc_case
{
  /* Where we saw this case.  */
  locus where;
  int n;

  /* Case range values.  If (low == high), it's a single value.  If one of
     the labels is NULL, it's an unbounded case.  If both are NULL, this
     represents the default case.  */
  gfc_expr *low, *high;

  /* Next case label in the list of cases for a single CASE label.  */
  struct gfc_case *next;

  /* Used for detecting overlap, and for code generation.  */
  struct gfc_case *left, *right;

  /* True if this case label can never be matched.  */
  int unreachable;
}
gfc_case;

#define gfc_get_case() gfc_getmem(sizeof(gfc_case))


typedef struct
{
  gfc_expr *var, *start, *end, *step;
}
gfc_iterator;

#define gfc_get_iterator() gfc_getmem(sizeof(gfc_iterator))


/* Allocation structure for ALLOCATE, DEALLOCATE and NULLIFY statements. */

typedef struct gfc_alloc
{
  gfc_expr *expr;
  struct gfc_alloc *next;
}
gfc_alloc;

#define gfc_get_alloc() gfc_getmem(sizeof(gfc_alloc))


typedef struct
{
  gfc_expr *unit, *file, *status, *access, *form, *recl,
    *blank, *position, *action, *delim, *pad, *iostat;
  gfc_st_label *err;
}
gfc_open;


typedef struct
{
  gfc_expr *unit, *status, *iostat;
  gfc_st_label *err;
}
gfc_close;


typedef struct
{
  gfc_expr *unit, *iostat;
  gfc_st_label *err;
}
gfc_filepos;


typedef struct
{
  gfc_expr *unit, *file, *iostat, *exist, *opened, *number, *named,
    *name, *access, *sequential, *direct, *form, *formatted,
    *unformatted, *recl, *nextrec, *blank, *position, *action, *read,
    *write, *readwrite, *delim, *pad, *iolength;

  gfc_st_label *err;

}
gfc_inquire;


typedef struct
{
  gfc_expr *io_unit, *format_expr, *rec, *advance, *iostat, *size;

  gfc_symbol *namelist;
  /* A format_label of `format_asterisk' indicates the "*" format */
  gfc_st_label *format_label;
  gfc_st_label *err, *end, *eor;

  locus eor_where, end_where;
}
gfc_dt;


typedef struct gfc_forall_iterator
{
  gfc_expr *var, *start, *end, *stride;
  struct gfc_forall_iterator *next;
}
gfc_forall_iterator;


/* Executable statements that fill gfc_code structures.  */
typedef enum
{
  EXEC_NOP = 1, EXEC_ASSIGN, EXEC_LABEL_ASSIGN, EXEC_POINTER_ASSIGN,
  EXEC_GOTO, EXEC_CALL, EXEC_RETURN, EXEC_PAUSE, EXEC_STOP, EXEC_CONTINUE,
  EXEC_IF, EXEC_ARITHMETIC_IF, EXEC_DO, EXEC_DO_WHILE, EXEC_SELECT,
  EXEC_FORALL, EXEC_WHERE, EXEC_CYCLE, EXEC_EXIT,
  EXEC_ALLOCATE, EXEC_DEALLOCATE,
  EXEC_OPEN, EXEC_CLOSE,
  EXEC_READ, EXEC_WRITE, EXEC_IOLENGTH, EXEC_TRANSFER, EXEC_DT_END,
  EXEC_BACKSPACE, EXEC_ENDFILE, EXEC_INQUIRE, EXEC_REWIND
}
gfc_exec_op;

typedef struct gfc_code
{
  gfc_exec_op op;

  struct gfc_code *block, *next;
  locus loc;

  gfc_st_label *here, *label, *label2, *label3;
  gfc_symtree *symtree;
  gfc_expr *expr, *expr2;
  /* A name isn't sufficient to identify a subroutine, we need the actual
     symbol for the interface definition.
  const char *sub_name;  */
  gfc_symbol *resolved_sym;

  union
  {
    gfc_actual_arglist *actual;
    gfc_case *case_list;
    gfc_iterator *iterator;
    gfc_alloc *alloc_list;
    gfc_open *open;
    gfc_close *close;
    gfc_filepos *filepos;
    gfc_inquire *inquire;
    gfc_dt *dt;
    gfc_forall_iterator *forall_iterator;
    struct gfc_code *whichloop;
    int stop_code;
  }
  ext;		/* Points to additional structures required by statement */

  /* Backend_decl is used for cycle and break labels in do loops, and
   * probably for other constructs as well, once we translate them.  */
  tree backend_decl;
}
gfc_code;


/* Storage for DATA statements.  */
typedef struct gfc_data_variable
{
  gfc_expr *expr;
  gfc_iterator iter;
  struct gfc_data_variable *list, *next;
}
gfc_data_variable;


typedef struct gfc_data_value
{
  int repeat;
  gfc_expr *expr;

  struct gfc_data_value *next;
}
gfc_data_value;


typedef struct gfc_data
{
  gfc_data_variable *var;
  gfc_data_value *value;
  locus where;

  struct gfc_data *next;
}
gfc_data;

#define gfc_get_data_variable() gfc_getmem(sizeof(gfc_data_variable))
#define gfc_get_data_value() gfc_getmem(sizeof(gfc_data_value))
#define gfc_get_data() gfc_getmem(sizeof(gfc_data))


/* Structure for holding compile options */
typedef struct
{
  const char *source;
  char *module_dir;
  gfc_source_form source_form;
  int fixed_line_length;
  int max_identifier_length;
  int verbose;

  int warn_aliasing;
  int warn_conversion;
  int warn_implicit_interface;
  int warn_line_truncation;
  int warn_surprising;
  int warn_unused_labels;

  int flag_dollar_ok;
  int flag_underscoring;
  int flag_second_underscore;
  int flag_implicit_none;
  int flag_max_stack_var_size;
  int flag_module_access_private;
  int flag_no_backend;
  int flag_pack_derived;
  int flag_repack_arrays;

  int q_kind;
  int r8;
  int i8;
  int d8;
  int warn_std;
  int allow_std;
}
gfc_option_t;

extern gfc_option_t gfc_option;


/* Constructor nodes for array and structure constructors.  */
typedef struct gfc_constructor
{
  gfc_expr *expr;
  gfc_iterator *iterator;
  locus where;
  struct gfc_constructor *next;
  struct
  {
    mpz_t offset; /* Record the offset of array element which appears in
                     data statement like "data a(5)/4/".  */
    gfc_component *component; /* Record the component being initialized.  */
  }
  n;
  mpz_t repeat; /* Record the repeat number of initial values in data
                 statement like "data a/5*10/".  */
}
gfc_constructor;


typedef struct iterator_stack
{
  gfc_symtree *variable;
  mpz_t value;
  struct iterator_stack *prev;
}
iterator_stack;
extern iterator_stack *iter_stack;

/************************ Function prototypes *************************/

/* data.c  */
void gfc_formalize_init_value (gfc_symbol *);
void gfc_get_section_index (gfc_array_ref *, mpz_t *, mpz_t *);
void gfc_assign_data_value (gfc_expr *, gfc_expr *, mpz_t);
void gfc_advance_section (mpz_t *, gfc_array_ref *, mpz_t *);

/* scanner.c */
void gfc_scanner_done_1 (void);
void gfc_scanner_init_1 (void);

void gfc_add_include_path (const char *);
void gfc_release_include_path (void);
FILE *gfc_open_included_file (const char *);

locus *gfc_current_locus (void);
void gfc_set_locus (locus *);

int gfc_at_end (void);
int gfc_at_eof (void);
int gfc_at_bol (void);
int gfc_at_eol (void);
void gfc_advance_line (void);
int gfc_check_include (void);

void gfc_skip_comments (void);
int gfc_next_char_literal (int);
int gfc_next_char (void);
int gfc_peek_char (void);
void gfc_error_recovery (void);
void gfc_gobble_whitespace (void);
try gfc_new_file (const char *, gfc_source_form);

extern gfc_file *gfc_current_file;

/* misc.c */
void *gfc_getmem (size_t) ATTRIBUTE_MALLOC;
void gfc_free (void *);
int gfc_terminal_width(void);
void gfc_clear_ts (gfc_typespec *);
FILE *gfc_open_file (const char *);
const char *gfc_article (const char *);
const char *gfc_basic_typename (bt);
const char *gfc_typename (gfc_typespec *);

#define gfc_op2string(OP) (OP == INTRINSIC_ASSIGN ? \
			   "=" : gfc_code2string (intrinsic_operators, OP))

const char *gfc_code2string (const mstring *, int);
int gfc_string2code (const mstring *, const char *);
const char *gfc_intent_string (sym_intent);

void gfc_init_1 (void);
void gfc_init_2 (void);
void gfc_done_1 (void);
void gfc_done_2 (void);

/* options.c */
unsigned int gfc_init_options (unsigned int, const char **);
int gfc_handle_option (size_t, const char *, int);
bool gfc_post_options (const char **);

/* iresolve.c */
char * gfc_get_string (const char *, ...) ATTRIBUTE_PRINTF_1;
void gfc_iresolve_init_1 (void);
void gfc_iresolve_done_1 (void);

/* error.c */

typedef struct gfc_error_buf
{
  int flag;
  char message[MAX_ERROR_MESSAGE];
} gfc_error_buf;

void gfc_error_init_1 (void);
void gfc_buffer_error (int);

void gfc_warning (const char *, ...);
void gfc_warning_now (const char *, ...);
void gfc_clear_warning (void);
void gfc_warning_check (void);

void gfc_error (const char *, ...);
void gfc_error_now (const char *, ...);
void gfc_fatal_error (const char *, ...) ATTRIBUTE_NORETURN;
void gfc_internal_error (const char *, ...) ATTRIBUTE_NORETURN;
void gfc_clear_error (void);
int gfc_error_check (void);

try gfc_notify_std (int, const char *, ...);

/* A general purpose syntax error.  */
#define gfc_syntax_error(ST)	\
  gfc_error ("Syntax error in %s statement at %C", gfc_ascii_statement (ST));

void gfc_push_error (gfc_error_buf *);
void gfc_pop_error (gfc_error_buf *);

void gfc_status (const char *, ...) ATTRIBUTE_PRINTF_1;
void gfc_status_char (char);

void gfc_get_errors (int *, int *);

/* arith.c */
void gfc_arith_init_1 (void);
void gfc_arith_done_1 (void);

/* FIXME: These should go to symbol.c, really...  */
int gfc_default_integer_kind (void);
int gfc_default_real_kind (void);
int gfc_default_double_kind (void);
int gfc_default_character_kind (void);
int gfc_default_logical_kind (void);
int gfc_default_complex_kind (void);
int gfc_validate_kind (bt, int);
extern int gfc_index_integer_kind;

/* symbol.c */
void gfc_clear_new_implicit (void);
try gfc_add_new_implicit_range (int, int, gfc_typespec *);
try gfc_merge_new_implicit (void);
void gfc_set_implicit_none (void);
void gfc_set_implicit (void);

gfc_typespec *gfc_get_default_type (gfc_symbol *, gfc_namespace *);
try gfc_set_default_type (gfc_symbol *, int, gfc_namespace *);

void gfc_set_component_attr (gfc_component *, symbol_attribute *);
void gfc_get_component_attr (symbol_attribute *, gfc_component *);

void gfc_set_sym_referenced (gfc_symbol * sym);

try gfc_add_allocatable (symbol_attribute *, locus *);
try gfc_add_dimension (symbol_attribute *, locus *);
try gfc_add_external (symbol_attribute *, locus *);
try gfc_add_intrinsic (symbol_attribute *, locus *);
try gfc_add_optional (symbol_attribute *, locus *);
try gfc_add_pointer (symbol_attribute *, locus *);
try gfc_add_result (symbol_attribute *, locus *);
try gfc_add_save (symbol_attribute *, locus *);
try gfc_add_saved_common (symbol_attribute *, locus *);
try gfc_add_target (symbol_attribute *, locus *);
try gfc_add_dummy (symbol_attribute *, locus *);
try gfc_add_generic (symbol_attribute *, locus *);
try gfc_add_common (symbol_attribute *, locus *);
try gfc_add_in_common (symbol_attribute *, locus *);
try gfc_add_in_namelist (symbol_attribute *, locus *);
try gfc_add_sequence (symbol_attribute *, locus *);
try gfc_add_elemental (symbol_attribute *, locus *);
try gfc_add_pure (symbol_attribute *, locus *);
try gfc_add_recursive (symbol_attribute *, locus *);
try gfc_add_function (symbol_attribute *, locus *);
try gfc_add_subroutine (symbol_attribute *, locus *);

try gfc_add_access (symbol_attribute *, gfc_access, locus *);
try gfc_add_flavor (symbol_attribute *, sym_flavor, locus *);
try gfc_add_entry (symbol_attribute *, locus *);
try gfc_add_procedure (symbol_attribute *, procedure_type, locus *);
try gfc_add_intent (symbol_attribute *, sym_intent, locus *);
try gfc_add_explicit_interface (gfc_symbol *, ifsrc,
				gfc_formal_arglist *, locus *);
try gfc_add_type (gfc_symbol *, gfc_typespec *, locus *);

void gfc_clear_attr (symbol_attribute *);
try gfc_missing_attr (symbol_attribute *, locus *);
try gfc_copy_attr (symbol_attribute *, symbol_attribute *, locus *);

try gfc_add_component (gfc_symbol *, const char *, gfc_component **);
gfc_symbol *gfc_use_derived (gfc_symbol *);
gfc_symtree *gfc_use_derived_tree (gfc_symtree *);
gfc_component *gfc_find_component (gfc_symbol *, const char *);

gfc_st_label *gfc_get_st_label (int);
void gfc_free_st_label (gfc_st_label *);
void gfc_define_st_label (gfc_st_label *, gfc_sl_type, locus *);
try gfc_reference_st_label (gfc_st_label *, gfc_sl_type);

gfc_namespace *gfc_get_namespace (gfc_namespace *);
gfc_symtree *gfc_new_symtree (gfc_symtree **, const char *);
gfc_symtree *gfc_find_symtree (gfc_symtree *, const char *);
gfc_user_op *gfc_get_uop (const char *);
gfc_user_op *gfc_find_uop (const char *, gfc_namespace *);
void gfc_free_symbol (gfc_symbol *);
gfc_symbol *gfc_new_symbol (const char *, gfc_namespace *);
int gfc_find_symbol (const char *, gfc_namespace *, int, gfc_symbol **);
int gfc_find_sym_tree (const char *, gfc_namespace *, int, gfc_symtree **);
int gfc_get_symbol (const char *, gfc_namespace *, gfc_symbol **);
int gfc_get_sym_tree (const char *, gfc_namespace *, gfc_symtree **);
int gfc_get_ha_symbol (const char *, gfc_symbol **);
int gfc_get_ha_sym_tree (const char *, gfc_symtree **);

int gfc_symbols_could_alias (gfc_symbol *, gfc_symbol *);

void gfc_undo_symbols (void);
void gfc_commit_symbols (void);
void gfc_free_namespace (gfc_namespace *);

void gfc_symbol_init_2 (void);
void gfc_symbol_done_2 (void);

void gfc_traverse_symtree (gfc_namespace *, void (*)(gfc_symtree *));
void gfc_traverse_ns (gfc_namespace *, void (*)(gfc_symbol *));
void gfc_traverse_user_op (gfc_namespace *, void (*)(gfc_user_op *));
void gfc_save_all (gfc_namespace *);

void gfc_symbol_state (void);

/* intrinsic.c */
extern int gfc_init_expr;

/* Given a symbol that we have decided is intrinsic, mark it as such
   by placing it into a special module that is otherwise impossible to
   read or write.  */

#define gfc_intrinsic_symbol(SYM) strcpy (SYM->module, "(intrinsic)")

void gfc_intrinsic_init_1 (void);
void gfc_intrinsic_done_1 (void);

char gfc_type_letter (bt);
gfc_symbol * gfc_get_intrinsic_sub_symbol (const char *);
try gfc_convert_type (gfc_expr *, gfc_typespec *, int);
try gfc_convert_type_warn (gfc_expr *, gfc_typespec *, int, int);
int gfc_generic_intrinsic (const char *);
int gfc_specific_intrinsic (const char *);
int gfc_intrinsic_name (const char *, int);
gfc_intrinsic_sym *gfc_find_function (const char *);

match gfc_intrinsic_func_interface (gfc_expr *, int);
match gfc_intrinsic_sub_interface (gfc_code *, int);

/* simplify.c */
void gfc_simplify_init_1 (void);
void gfc_simplify_done_1 (void);

/* match.c -- FIXME */
void gfc_free_iterator (gfc_iterator *, int);
void gfc_free_forall_iterator (gfc_forall_iterator *);
void gfc_free_alloc_list (gfc_alloc *);
void gfc_free_namelist (gfc_namelist *);
void gfc_free_equiv (gfc_equiv *);
void gfc_free_data (gfc_data *);
void gfc_free_case_list (gfc_case *);

/* expr.c */
void gfc_free_actual_arglist (gfc_actual_arglist *);
gfc_actual_arglist *gfc_copy_actual_arglist (gfc_actual_arglist *);
const char *gfc_extract_int (gfc_expr *, int *);

gfc_expr *gfc_build_conversion (gfc_expr *);
void gfc_free_ref_list (gfc_ref *);
void gfc_type_convert_binary (gfc_expr *);
int gfc_is_constant_expr (gfc_expr *);
try gfc_simplify_expr (gfc_expr *, int);

gfc_expr *gfc_get_expr (void);
void gfc_free_expr (gfc_expr *);
void gfc_replace_expr (gfc_expr *, gfc_expr *);
gfc_expr *gfc_int_expr (int);
gfc_expr *gfc_logical_expr (int, locus *);
mpz_t *gfc_copy_shape (mpz_t *, int);
gfc_expr *gfc_copy_expr (gfc_expr *);

try gfc_specification_expr (gfc_expr *);

int gfc_numeric_ts (gfc_typespec *);
int gfc_kind_max (gfc_expr *, gfc_expr *);

try gfc_check_conformance (const char *, gfc_expr *, gfc_expr *);
try gfc_check_assign (gfc_expr *, gfc_expr *, int);
try gfc_check_pointer_assign (gfc_expr *, gfc_expr *);
try gfc_check_assign_symbol (gfc_symbol *, gfc_expr *);

/* st.c */
extern gfc_code new_st;

void gfc_clear_new_st (void);
gfc_code *gfc_get_code (void);
gfc_code *gfc_append_code (gfc_code *, gfc_code *);
void gfc_free_statement (gfc_code *);
void gfc_free_statements (gfc_code *);

/* resolve.c */
try gfc_resolve_expr (gfc_expr *);
void gfc_resolve (gfc_namespace *);
int gfc_impure_variable (gfc_symbol *);
int gfc_pure (gfc_symbol *);
int gfc_elemental (gfc_symbol *);
try gfc_resolve_iterator (gfc_iterator *);
try gfc_resolve_index (gfc_expr *, int);

/* array.c */
void gfc_free_array_spec (gfc_array_spec *);
gfc_array_ref *gfc_copy_array_ref (gfc_array_ref *);

try gfc_set_array_spec (gfc_symbol *, gfc_array_spec *, locus *);
gfc_array_spec *gfc_copy_array_spec (gfc_array_spec *);
try gfc_resolve_array_spec (gfc_array_spec *, int);

int gfc_compare_array_spec (gfc_array_spec *, gfc_array_spec *);

gfc_expr *gfc_start_constructor (bt, int, locus *);
void gfc_append_constructor (gfc_expr *, gfc_expr *);
void gfc_free_constructor (gfc_constructor *);
void gfc_simplify_iterator_var (gfc_expr *);
try gfc_expand_constructor (gfc_expr *);
int gfc_constant_ac (gfc_expr *);
int gfc_expanded_ac (gfc_expr *);
try gfc_resolve_array_constructor (gfc_expr *);
try gfc_check_constructor_type (gfc_expr *);
try gfc_check_iter_variable (gfc_expr *);
try gfc_check_constructor (gfc_expr *, try (*)(gfc_expr *));
gfc_constructor *gfc_copy_constructor (gfc_constructor * src);
gfc_expr *gfc_get_array_element (gfc_expr *, int);
try gfc_array_size (gfc_expr *, mpz_t *);
try gfc_array_dimen_size (gfc_expr *, int, mpz_t *);
try gfc_array_ref_shape (gfc_array_ref *, mpz_t *);
gfc_array_ref *gfc_find_array_ref (gfc_expr *);
void gfc_insert_constructor (gfc_expr *, gfc_constructor *);
gfc_constructor *gfc_get_constructor (void);
tree gfc_conv_array_initializer (tree type, gfc_expr * expr);
try spec_size (gfc_array_spec *, mpz_t *);

/* interface.c -- FIXME: some of these should be in symbol.c */
void gfc_free_interface (gfc_interface *);
int gfc_compare_types (gfc_typespec *, gfc_typespec *);
void gfc_check_interfaces (gfc_namespace *);
void gfc_procedure_use (gfc_symbol *, gfc_actual_arglist **, locus *);
gfc_symbol *gfc_search_interface (gfc_interface *, int,
				  gfc_actual_arglist **);
try gfc_extend_expr (gfc_expr *);
void gfc_free_formal_arglist (gfc_formal_arglist *);
try gfc_extend_assign (gfc_code *, gfc_namespace *);
try gfc_add_interface (gfc_symbol * sym);

/* io.c */
extern gfc_st_label format_asterisk;

void gfc_free_open (gfc_open *);
try gfc_resolve_open (gfc_open *);
void gfc_free_close (gfc_close *);
try gfc_resolve_close (gfc_close *);
void gfc_free_filepos (gfc_filepos *);
try gfc_resolve_filepos (gfc_filepos *);
void gfc_free_inquire (gfc_inquire *);
try gfc_resolve_inquire (gfc_inquire *);
void gfc_free_dt (gfc_dt *);
try gfc_resolve_dt (gfc_dt *);

/* module.c */
void gfc_module_init_2 (void);
void gfc_module_done_2 (void);
void gfc_dump_module (const char *, int);

/* primary.c */
symbol_attribute gfc_variable_attr (gfc_expr *, gfc_typespec *);
symbol_attribute gfc_expr_attr (gfc_expr *);

/* trans.c */
void gfc_generate_code (gfc_namespace *);
void gfc_generate_module_code (gfc_namespace *);

/* bbt.c */
typedef int (*compare_fn) (void *, void *);
void gfc_insert_bbt (void *, void *, compare_fn);
void gfc_delete_bbt (void *, void *, compare_fn);

/* dump-parse-tree.c */
void gfc_show_namespace (gfc_namespace *);

/* parse.c */
try gfc_parse_file (void);

#endif /* GFC_GFC_H  */
