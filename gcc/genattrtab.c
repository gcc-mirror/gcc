/* Generate code from machine description to compute values of attributes.
   Copyright (C) 1991 Free Software Foundation, Inc.
   Contributed by Richard Kenner (kenner@nyu.edu)

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* This program handles insn attribues and the DEFINE_DELAY and
   DEFINE_FUNCTION_UNIT definitions.

   It produces a series of functions named `get_attr_...', one for each insn
   attribute.  Each of these is given the rtx for an insn and returns a member
   of the enum for the attribute.

   These subroutines have the form of a `switch' on the INSN_CODE (via
   `recog_memoized').  Each case either returns a constant attribute value
   or a value that depends on tests on other attributes, the form of
   operands, or some random C expression (encoded with a SYMBOL_REF
   expression).

   If the attribute `alternative', or a random C expression is present,
   `constrain_operands' is called.  If either of these cases of a reference to
   an operand is found, `insn_extract' is called.

   The special attribute `length' is also recognized.  For this operand, 
   expressions involving the address of an operand or the current insn,
   (address (pc)), are valid.  In this case, an initial pass is made to
   set all lengths that do not depend on address.  Those that do are set to
   the maximum length.  Then each insn that depends on an address is checked
   and possibly has its length changed.  The process repeats until no further
   changed are made.  The resulting lengths are saved for use by
   `get_attr_length'.

   A special form of DEFINE_ATTR, where the expression for default value is a
   CONST expression, indicates an attribute that is constant for a given run
   of the compiler.  The subroutine generated for these attributes has no
   parameters as it does not depend on any particular insn.  Constant
   attributes are typically used to specify which variety of processor is
   used.
   
   Internal attributes are defined to handle DEFINE_DELAY and
   DEFINE_FUNCTION_UNIT.  Special routines are output for these cases.

   This program works by keeping a list of possible values for each attribute.
   These include the basic attribute choices, default values for attribute, and
   all derived quantities.

   As the description file is read, the definition for each insn is saved in a
   `struct insn_def'.   When the file reading is complete, a `struct insn_ent'
   is created for each insn and chained to the corresponding attribute value,
   either that specified, or the default.

   An optimization phase is then run.  This simplifies expressions for each
   insn.  EQ_ATTR tests are resolved, whenever possible, to a test that
   indicates when the attribute has the specified value for the insn.  This
   avoids recursive calls during compilation.

   The strategy used when processing DEFINE_DELAY and DEFINE_FUNCTION_UNIT
   definitions is to create arbitrarily complex expressions and have the
   optimization simplify them.

   Once optimization is complete, any required routines and definitions
   will be written.

   An optimization that is not yet implemented is to hoist the constant
   expressions entirely out of the routines and definitions that are written.
   A way to do this is to iterate over all possible combinations of values
   for constant attributes and generate a set of functions for that given
   combination.  An initialization function would be written that evaluates
   the attributes and installs the corresponding set of routines and
   definitions (each would be accessed through a pointer).  */

#include <stdio.h>
#include "gvarargs.h"
#include "config.h"
#include "rtl.h"
#include "obstack.h"
#include "insn-config.h"	/* For REGISTER_CONSTRAINTS */

static struct obstack obstack;
struct obstack *rtl_obstack = &obstack;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern void free ();

static void fatal ();
void fancy_abort ();

/* Define structures used to record attributes and values.  */

/* As each DEFINE_INSN, DEFINE_PEEPHOLE, or DEFINE_ASM_ATTRIBUTES is
   encountered, we store all the relevant information into a
   `struct insn_def'.  This is done to allow attribute definitions to occur
   anywhere in the file.  */

struct insn_def
{
  int insn_code;		/* Instruction number. */
  int insn_index;		/* Expression numer in file, for errors. */
  struct insn_def *next;	/* Next insn in chain. */
  rtx def;			/* The DEFINE_... */
  int num_alternatives;		/* Number of alternatives.  */
  int vec_idx;			/* Index of attribute vector in `def'. */
};

/* Once everything has been read in, we store in each attribute value a list
   of insn codes that have that value.  Here is the structure used for the
   list.  */

struct insn_ent
{
  int insn_code;		/* Instruction number.  */
  int insn_index;		/* Index of definition in file */
  struct insn_ent *next;	/* Next in chain.  */
};

/* Each value of an attribute (either constant or computed) is assigned a
   structure which is used as the listhead of the insns that have that
   value.  */

struct attr_value
{
  rtx value;			/* Value of attribute.  */
  struct attr_value *next;	/* Next attribute value in chain.  */
  struct insn_ent *first_insn;	/* First insn with this value.  */
  int num_insns;		/* Number of insns with this value.  */
  int has_asm_insn;		/* True if this value used for `asm' insns */
};

/* Structure for each attribute.  */

struct attr_desc
{
  char *name;			/* Name of attribute. */
  struct attr_desc *next;	/* Next attribute. */
  int is_numeric;		/* Values of this attribute are numeric. */
  int is_const;			/* Attribute value constant for each run.  */
  int is_special;		/* Don't call `write_attr_set'. */
  struct attr_value *first_value; /* First value of this attribute. */
  struct attr_value *default_val; /* Default value for this attribute. */
};

/* Structure for each DEFINE_DELAY.  */

struct delay_desc
{
  rtx def;			/* DEFINE_DELAY expression.  */
  struct delay_desc *next;	/* Next DEFINE_DELAY. */
  int num;			/* Number of DEFINE_DELAY, starting at 1.  */
};

/* Record information about each DEFINE_FUNCTION_UNIT.  */

struct function_unit_op
{
  rtx condexp;			/* Expression TRUE for applicable insn.  */
  struct function_unit_op *next; /* Next operation for this function unit.  */
  int num;			/* Ordinal for this operation type in unit.  */
  int ready;			/* Cost until data is ready.  */
  rtx busyexp;			/* Expression computing conflict cost.  */
};

/* Record information about each function unit mentioned in a
   DEFINE_FUNCTION_UNIT.  */

struct function_unit
{
  char *name;			/* Function unit name.  */
  struct function_unit *next;	/* Next function unit.  */
  int num;			/* Ordinal of this unit type.  */
  int multiplicity;		/* Number of units of this type.  */
  int simultaneity;		/* Maximum number of simultaneous insns
				   on this function unit or 0 if unlimited.  */
  rtx condexp;			/* Expression TRUE for insn needing unit. */
  rtx costexp;			/* Worst-case cost as function of insn. */
  int num_opclasses;		/* Number of different operation types.  */
  struct function_unit_op *ops;	/* Pointer to first operation type.  */
  int needs_conflict_function;	/* Nonzero if a conflict function required.  */
  rtx default_cost;		/* Conflict cost, if constant.  */
};

/* Listheads of above structures.  */

static struct attr_desc *attrs;
static struct insn_def *defs;
static struct delay_desc *delays;
static struct function_unit *units;

/* Other variables. */

static int insn_code_number;
static int insn_index_number;
static int got_define_asm_attributes;
static int must_extract;
static int must_constrain;
static int address_used;
static int num_delays;
static int have_annul_true, have_annul_false;
static int num_units;

/* Used as operand to `operate_exp':  */

enum operator {PLUS_OP, MINUS_OP, OR_OP, MAX_OP};

/* Stores, for each insn code, a bitmap that has bits on for each possible
   alternative.  */

static int *insn_alternatives;

/* Used to simplify expressions.  */

static rtx true_rtx, false_rtx;

/* Used to reduce calls to `strcmp' */

static char *alternative_name = "alternative";

/* Simplify an expression.  Only call the routine if there is something to
   simplify.  */
#define SIMPLIFY_TEST_EXP(EXP,INSN_CODE,INSN_INDEX)	\
  (RTX_UNCHANGING_P (EXP) ? (EXP)			\
   : simplify_test_exp (EXP, INSN_CODE, INSN_INDEX))
  
/* These are referenced by rtlanal.c and hence need to be defined somewhere.
   They won't actually be used.  */

rtx frame_pointer_rtx, stack_pointer_rtx, arg_pointer_rtx;

static rtx attr_rtx ();
static char *attr_printf ();
static char *attr_string ();
static rtx check_attr_test ();
static void check_attr_value ();
static rtx convert_set_attr_alternative ();
static rtx convert_set_attr ();
static void check_defs ();
static rtx convert_const_symbol_ref ();
static rtx make_canonical ();
static struct attr_value *get_attr_value ();
static void expand_delays ();
static rtx operate_exp ();
static void expand_units ();
static void fill_attr ();
static rtx substitute_address ();
static void make_length_attrs ();
static rtx identity_fn ();
static rtx zero_fn ();
static rtx one_fn ();
static rtx max_fn ();
static rtx simplify_cond ();
static void remove_insn_ent ();
static void insert_insn_ent ();
static rtx insert_right_side ();
static rtx make_alternative_compare ();
static int compute_alternative_mask ();
static rtx evaluate_eq_attr ();
static rtx simplify_and_tree ();
static rtx simplify_or_tree ();
static rtx simplify_test_exp ();
static void optimize_attrs ();
static void gen_attr ();
static int count_alternatives ();
static int compares_alternatives_p ();
static int contained_in_p ();
static void gen_insn ();
static void gen_delay ();
static void gen_unit ();
static void write_test_expr ();
static int max_attr_value ();
static void walk_attr_value ();
static void write_attr_get ();
static rtx eliminate_known_true ();
static void write_attr_set ();
static void write_attr_case ();
static void write_attr_value ();
static void write_attr_valueq ();
static void write_upcase ();
static void write_indent ();
static void write_eligible_delay ();
static void write_function_unit_info ();
static int n_comma_elts ();
static char *next_comma_elt ();
static struct attr_desc *find_attr ();
static void make_internal_attr ();
static struct attr_value *find_most_used ();
static rtx find_single_value ();
static rtx make_numeric_value ();
char *xrealloc ();
char *xmalloc ();
static void fatal ();

/* Hash table for sharing RTL and strings.  */

/* Each hash table slot is a bucket containing a chain of these structures.
   Strings are given negative hash codes; RTL expressions are given positive
   hash codes.  */

struct attr_hash
{
  struct attr_hash *next;	/* Next structure in the bucket.  */
  int hashcode;			/* Hash code of this rtx or string.  */
  union
    {
      char *str;		/* The string (negative hash codes) */
      rtx rtl;			/* or the RTL recorded here.  */
    } u;
};

/* Now here is the hash table.  When recording an RTL, it is added to
   the slot whose index is the hash code mod the table size.  Note
   that the hash table is used for several kinds of RTL (see attr_rtx)
   and for strings.  While all these live in the same table, they are
   completely independent, and the hash code is computed differently
   for each.  */

#define RTL_HASH_SIZE 4093
struct attr_hash *attr_hash_table[RTL_HASH_SIZE];

/* Here is how primitive or already-shared RTL's hash
   codes are made.  */
#define RTL_HASH(RTL) ((int) (RTL) & 0777777)

/* Add an entry to the hash table for RTL with hash code HASHCODE.  */

static void
attr_hash_add_rtx (hashcode, rtl)
     int hashcode;
     rtx rtl;
{
  register struct attr_hash *h;

  h = (struct attr_hash *) xmalloc (sizeof (struct attr_hash));
  h->hashcode = hashcode;
  h->u.rtl = rtl;
  h->next = attr_hash_table[hashcode % RTL_HASH_SIZE];
  attr_hash_table[hashcode % RTL_HASH_SIZE] = h;
}

/* Add an entry to the hash table for STRING with hash code HASHCODE.  */

static void
attr_hash_add_string (hashcode, str)
     int hashcode;
     char *str;
{
  register struct attr_hash *h;

  h = (struct attr_hash *) xmalloc (sizeof (struct attr_hash));
  h->hashcode = -hashcode;
  h->u.str = str;
  h->next = attr_hash_table[hashcode % RTL_HASH_SIZE];
  attr_hash_table[hashcode % RTL_HASH_SIZE] = h;
}

/* Generate an RTL expression, but allow sharing.  Like gen_rtx, but the 
   mode is not used:

   rtx attr_rtx (code, [element1, ..., elementn])  */

/*VARARGS1*/
static rtx
attr_rtx (va_alist)
     va_dcl
{
  va_list p;
  enum rtx_code code;
  register int i;		/* Array indices...			*/
  register char *fmt;		/* Current rtx's format...		*/
  register rtx rt_val;		/* RTX to return to caller...		*/
  int hashcode;
  register struct attr_hash *h;

  va_start (p);
  code = va_arg (p, enum rtx_code);

  /* For each of several cases, search the hash table for an existing entry.
     Use that entry if one is found; otherwise create a new RTL and add it
     to the table.  */

  if (GET_RTX_CLASS (code) == '1')
    {
      rtx arg0 = va_arg (p, rtx);

      hashcode = (code + RTL_HASH (arg0));
      for (h = attr_hash_table[hashcode % RTL_HASH_SIZE]; h; h = h->next)
	if (h->hashcode == hashcode
	    && GET_CODE (h->u.rtl) == code
	    && XEXP (h->u.rtl, 0) == arg0)
	  goto found;

      if (h == 0)
	{
	  rt_val = rtx_alloc (code);
	  XEXP (rt_val, 0) = arg0;
	}
    }
  else if (GET_RTX_CLASS (code) == 'c'
	   || GET_RTX_CLASS (code) == '2'
	   || GET_RTX_CLASS (code) == '<')
    {
      rtx arg0 = va_arg (p, rtx);
      rtx arg1 = va_arg (p, rtx);

      hashcode = (code + RTL_HASH (arg0) + RTL_HASH (arg1));
      for (h = attr_hash_table[hashcode % RTL_HASH_SIZE]; h; h = h->next)
	if (h->hashcode == hashcode
	    && GET_CODE (h->u.rtl) == code
	    && XEXP (h->u.rtl, 0) == arg0
	    && XEXP (h->u.rtl, 1) == arg1)
	  goto found;

      if (h == 0)
	{
	  rt_val = rtx_alloc (code);
	  XEXP (rt_val, 0) = arg0;
	  XEXP (rt_val, 1) = arg1;
	}
    }
  else if (GET_RTX_LENGTH (code) == 1
	   && GET_RTX_FORMAT (code)[0] == 's')
    {
      char * arg0 = va_arg (p, char *);

      hashcode = (code + RTL_HASH (arg0));
      for (h = attr_hash_table[hashcode % RTL_HASH_SIZE]; h; h = h->next)
	if (h->hashcode == hashcode
	    && GET_CODE (h->u.rtl) == code
	    && XSTR (h->u.rtl, 0) == arg0)
	  goto found;

      if (h == 0)
	{
	  rt_val = rtx_alloc (code);
	  XSTR (rt_val, 0) = arg0;
	}
    }
  else if (GET_RTX_LENGTH (code) == 2
	   && GET_RTX_FORMAT (code)[0] == 's'
	   && GET_RTX_FORMAT (code)[1] == 's')
    {
      char * arg0 = va_arg (p, char *);
      char * arg1 = va_arg (p, char *);

      hashcode = (code + RTL_HASH (arg0) + RTL_HASH (arg1));
      for (h = attr_hash_table[hashcode % RTL_HASH_SIZE]; h; h = h->next)
	if (h->hashcode == hashcode
	    && GET_CODE (h->u.rtl) == code
	    && XSTR (h->u.rtl, 0) == arg0
	    && XSTR (h->u.rtl, 1) == arg1)
	  goto found;

      if (h == 0)
	{
	  rt_val = rtx_alloc (code);
	  XSTR (rt_val, 0) = arg0;
	  XSTR (rt_val, 1) = arg1;
	}
    }
  else
    {
      rt_val = rtx_alloc (code);	/* Allocate the storage space.  */
      
      fmt = GET_RTX_FORMAT (code);	/* Find the right format...  */
      for (i = 0; i < GET_RTX_LENGTH (code); i++)
	{
	  switch (*fmt++)
	    {
	    case '0':		/* Unused field.  */
	      break;

	    case 'i':		/* An integer?  */
	      XINT (rt_val, i) = va_arg (p, int);
	      break;

	    case 's':		/* A string?  */
	      XSTR (rt_val, i) = va_arg (p, char *);
	      break;

	    case 'e':		/* An expression?  */
	    case 'u':		/* An insn?  Same except when printing.  */
	      XEXP (rt_val, i) = va_arg (p, rtx);
	      break;

	    case 'E':		/* An RTX vector?  */
	      XVEC (rt_val, i) = va_arg (p, rtvec);
	      break;

	    default:
	      abort();
	    }
	}
      va_end (p);
      return rt_val;
    }

  va_end (p);
  attr_hash_add_rtx (hashcode, rt_val);
  return rt_val;

 found:
  va_end (p);
  return h->u.rtl;
}

/* Create a new string printed with the printf line arguments into a space
   of at most LEN bytes:

   rtx attr_printf (len, format, [arg1, ..., argn])  */

#ifdef HAVE_VPRINTF

/*VARARGS2*/
static char *
attr_printf (va_alist)
     va_dcl
{
  va_list p;
  register int len;
  register char *fmt;
  register char *str;

  /* Print the string into a temporary location.  */
  va_start (p);
  len = va_arg (p, int);
  str = (char *) alloca (len);
  fmt = va_arg (p, char *);
  vsprintf (str, fmt, p);
  va_end (p);

  return attr_string (str, strlen (str));
}

#else /* not HAVE_VPRINTF */

static char *
attr_printf (len, fmt, arg1, arg2, arg3)
     int len;
     char *fmt;
     char *arg1, *arg2, *arg3; /* also int */
{
  register char *str;

  /* Print the string into a temporary location.  */
  str = (char *) alloca (len);
  sprintf (str, fmt, arg1, arg2, arg3);

  return attr_string (str, strlen (str));
}
#endif /* not HAVE_VPRINTF */

/* Return a permanent (possibly shared) copy of a string STR (not assumed
   to be null terminated) with LEN bytes.  */

static char *
attr_string (str, len)
     char *str;
     int len;
{
  register struct attr_hash *h;
  int hashcode;
  int i;
  register char *new_str;

  /* Compute the hash code.  */
  hashcode = (len + 1) * 613 + (unsigned)str[0];
  for (i = 1; i <= len; i += 2)
    hashcode = ((hashcode * 613) + (unsigned)str[i]);
  if (hashcode < 0)
    hashcode = -hashcode;

  /* Search the table for the string.  */
  for (h = attr_hash_table[hashcode % RTL_HASH_SIZE]; h; h = h->next)
    if (h->hashcode == -hashcode
	&& !strcmp (h->u.str, str))
      return h->u.str;			/* <-- return if found.  */

  /* Not found; create a permanent copy and add it to the hash table.  */
  new_str = (char *) xmalloc (len + 1);
  bcopy (str, new_str, len);
  new_str[len] = '\0';
  attr_hash_add_string (hashcode, new_str);

  return new_str;			/* Return the new string.  */
}

/* Given a test expression for an attribute, ensure it is validly formed.
   IS_CONST indicates whether the expression is constant for each compiler
   run (a constant expression may not test any particular insn).

   Convert (eq_attr "att" "a1,a2") to (ior (eq_attr ... ) (eq_attrq ..))
   and (eq_attr "att" "!a1") to (not (eq_attr "att" "a1")).  Do the latter
   test first so that (eq_attr "att" "!a1,a2,a3") works as expected.

   Update the string address in EQ_ATTR expression to be the same used
   in the attribute (or `alternative_name') to speed up subsequent
   `find_attr' calls and eliminate most `strcmp' calls.

   Return the new expression, if any.   */

static rtx
check_attr_test (exp, is_const)
     rtx exp;
     int is_const;
{
  struct attr_desc *attr;
  struct attr_value *av;
  char *name_ptr, *p;
  rtx orexp, newexp;

  switch (GET_CODE (exp))
    {
    case EQ_ATTR:
      /* Handle negation test.  */
      if (XSTR (exp, 1)[0] == '!')
	return check_attr_test (attr_rtx (NOT,
					  attr_rtx (EQ_ATTR,
						    XSTR (exp, 0),
						    &XSTR(exp, 1)[1])),
				is_const);

      else if (n_comma_elts (XSTR (exp, 1)) == 1)
	{
	  attr = find_attr (XEXP (exp, 0), 0);
	  if (attr == NULL)
	    {
	      if (! strcmp (XSTR (exp, 0), "alternative"))
		{
		  XSTR (exp, 0) = alternative_name;
		  /* This can't be simplified any further.  */
		  RTX_UNCHANGING_P (exp) = 1;
		  return exp;
		}
	    else
		fatal ("Unknown attribute `%s' in EQ_ATTR", XEXP (exp, 0));
	    }

	  if (is_const && ! attr->is_const)
	    fatal ("Constant expression uses insn attribute `%s' in EQ_ATTR",
		   XEXP (exp, 0));

	  XSTR (exp, 0) = attr->name;

	  if (attr->is_numeric)
	    {
	      for (p = XSTR (exp, 1); *p; p++)
		if (*p < '0' || *p > '9')
		   fatal ("Attribute `%s' takes only numeric values", 
			  XEXP (exp, 0));
	    }
	  else
	    {
	      for (av = attr->first_value; av; av = av->next)
		if (GET_CODE (av->value) == CONST_STRING
		    && ! strcmp (XSTR (exp, 1), XSTR (av->value, 0)))
		  break;

	      if (av == NULL)
		fatal ("Unknown value `%s' for `%s' attribute",
		       XEXP (exp, 1), XEXP (exp, 0));
	    }
	}
      else
	{
	  /* Make an IOR tree of the possible values.  */
	  orexp = false_rtx;
	  name_ptr = XSTR (exp, 1);
	  while ((p = next_comma_elt (&name_ptr)) != NULL)
	    {
	      newexp = attr_rtx (EQ_ATTR, XSTR (exp, 0), p);
	      orexp = insert_right_side (IOR, orexp, newexp, -2);
	    }

	  return check_attr_test (orexp, is_const);
	}
      break;

    case CONST_INT:
      /* Either TRUE or FALSE.  */
      if (XINT (exp, 0))
	return true_rtx;
      else
	return false_rtx;

    case IOR:
    case AND:
      XEXP (exp, 0) = check_attr_test (XEXP (exp, 0), is_const);
      XEXP (exp, 1) = check_attr_test (XEXP (exp, 1), is_const);
      break;

    case NOT:
      XEXP (exp, 0) = check_attr_test (XEXP (exp, 0), is_const);
      break;

    case MATCH_OPERAND:
      if (is_const)
	fatal ("RTL operator \"%s\" not valid in constant attribute test",
	       GET_RTX_NAME (MATCH_OPERAND));

    case LE:  case LT:  case GT:  case GE:
    case LEU: case LTU: case GTU: case GEU:
    case NE:  case EQ:
      /* These cases can't be simplified.  */
      RTX_UNCHANGING_P (exp) = 1;
      break;

    case SYMBOL_REF:
      if (is_const)
	{
	  /* These cases are valid for constant attributes, but can't be
	     simplified.  */
	  RTX_UNCHANGING_P (exp) = 1;
	  break;
	}
    default:
      fatal ("RTL operator \"%s\" not valid in attribute test",
	     GET_RTX_NAME (GET_CODE (exp)));
    }

  return exp;
}

/* Given an expression, ensure that it is validly formed and that all named
   attribute values are valid for the given attribute.  Issue a fatal error
   if not.  If no attribute is specified, assume a numeric attribute.  */

static void
check_attr_value (exp, attr)
     rtx exp;
     struct attr_desc *attr;
{
  struct attr_value *av;
  char *p;
  int i;

  switch (GET_CODE (exp))
    {
    case CONST_INT:
      if (attr && ! attr->is_numeric)
	fatal ("CONST_INT not valid for non-numeric `%s' attribute",
	       attr->name);

      if (INTVAL (exp) < 0)
	fatal ("Negative numeric value specified for `%s' attribute",
	       attr->name);

      break;

    case CONST_STRING:
      if (! strcmp (XSTR (exp, 0), "*"))
	break;

      if (attr == 0 || attr->is_numeric)
	{
	  for (p = XSTR (exp, 0); *p; p++)
	    if (*p > '9' || *p < '0')
	      fatal ("Non-numeric value for numeric `%s' attribute",
		     attr ? "internal" : attr->name);
	  break;
	}

      for (av = attr->first_value; av; av = av->next)
	if (GET_CODE (av->value) == CONST_STRING
	    && ! strcmp (XSTR (av->value, 0), XSTR (exp, 0)))
	  break;

      if (av == NULL)
	fatal ("Unknown value `%s' for `%s' attribute",
	       XSTR (exp, 0), attr ? "internal" : attr->name);

      return;

    case IF_THEN_ELSE:
      XEXP (exp, 0) = check_attr_test (XEXP (exp, 0),
				       attr ? attr->is_const : 0);
      check_attr_value (XEXP (exp, 1), attr);
      check_attr_value (XEXP (exp, 2), attr);
      return;

    case COND:
      if (XVECLEN (exp, 0) % 2 != 0)
	fatal ("First operand of COND must have even length");

      for (i = 0; i < XVECLEN (exp, 0); i += 2)
	{
	  XVECEXP (exp, 0, i) = check_attr_test (XVECEXP (exp, 0, i),
						 attr ? attr->is_const : 0);
	  check_attr_value (XVECEXP (exp, 0, i + 1), attr);
	}

      check_attr_value (XEXP (exp, 1), attr);
      return;

    case SYMBOL_REF:
      if (attr && attr->is_const)
	/* A constant SYMBOL_REF is valid as a constant attribute test and
	   is expanded later by make_canonical into a COND.  */
	return;
      /* Otherwise, fall through... */

    default:
      fatal ("Illegal operation `%s' for attribute value",
	     GET_RTX_NAME (GET_CODE (exp)));
    }
}

/* Given an SET_ATTR_ALTERNATIVE expression, convert to the canonical SET.
   It becomes a COND with each test being (eq_attr "alternative "n") */

static rtx
convert_set_attr_alternative (exp, num_alt, insn_code, insn_index)
     rtx exp;
     int num_alt;
     int insn_code, insn_index;
{
  rtx newexp;
  rtx condexp;
  int i;

  if (XVECLEN (exp, 1) != num_alt)
    fatal ("Bad number of entries in SET_ATTR_ALTERNATIVE for insn %d",
	   insn_index);

  /* Make a COND with all tests but the last.  Select the last value via the
     default.  */
  condexp = rtx_alloc (COND);
  XVEC (condexp, 0) = rtvec_alloc ((num_alt - 1) * 2);

  for (i = 0; i < num_alt - 1; i++)
    {
      char *p;
      p = attr_printf (3, "%d", i);

      /* Sharing this EQ_ATTR rtl causes trouble.  */   
      XVECEXP (condexp, 0, 2 * i) = rtx_alloc (EQ_ATTR);
      XSTR (XVECEXP (condexp, 0, 2 * i), 0) = alternative_name;
      XSTR (XVECEXP (condexp, 0, 2 * i), 1) = p;
      XVECEXP (condexp, 0, 2 * i + 1) = XVECEXP (exp, 1, i);
    }

  XEXP (condexp, 1) = XVECEXP (exp, 1, i);

  return attr_rtx (SET, attr_rtx (ATTR, XSTR (exp, 0)), condexp);
}

/* Given a SET_ATTR, convert to the appropriate SET.  If a comma-separated
   list of values is given, convert to SET_ATTR_ALTERNATIVE first.  */

static rtx
convert_set_attr (exp, num_alt, insn_code, insn_index)
     rtx exp;
     int num_alt;
     int insn_code, insn_index;
{
  rtx newexp;
  char *name_ptr;
  char *p;
  int n;

  /* See how many alternative specified.  */
  n = n_comma_elts (XSTR (exp, 1));
  if (n == 1)
    return attr_rtx (SET,
		     attr_rtx (ATTR, XSTR (exp, 0)),
		     attr_rtx (CONST_STRING, XSTR (exp, 1)));

  newexp = rtx_alloc (SET_ATTR_ALTERNATIVE);
  XSTR (newexp, 0) = XSTR (exp, 0);
  XVEC (newexp, 1) = rtvec_alloc (n);

  /* Process each comma-separated name.  */
  name_ptr = XSTR (exp, 1);
  n = 0;
  while ((p = next_comma_elt (&name_ptr)) != NULL)
    XVECEXP (newexp, 1, n++) = attr_rtx (CONST_STRING, p);

  return convert_set_attr_alternative (newexp, num_alt, insn_code, insn_index);
}

/* Scan all definitions, checking for validity.  Also, convert any SET_ATTR
   and SET_ATTR_ALTERNATIVE expressions to the corresponding SET
   expressions. */

static void
check_defs ()
{
  struct insn_def *id;
  struct attr_desc *attr;
  int i;
  rtx value;

  for (id = defs; id; id = id->next)
    {
      if (XVEC (id->def, id->vec_idx) == NULL)
	continue;

      for (i = 0; i < XVECLEN (id->def, id->vec_idx); i++)
	{
	  value = XVECEXP (id->def, id->vec_idx, i);
	  switch (GET_CODE (value))
	    {
	    case SET:
	      if (GET_CODE (XEXP (value, 0)) != ATTR)
		fatal ("Bad attribute set in pattern %d", id->insn_index);
	      break;

	    case SET_ATTR_ALTERNATIVE:
	      value = convert_set_attr_alternative (value,
						    id->num_alternatives,
						    id->insn_code,
						    id->insn_index);
	      break;

	    case SET_ATTR:
	      value = convert_set_attr (value, id->num_alternatives,
					id->insn_code, id->insn_index);
	      break;

	    default:
	      fatal ("Invalid attribute code `%s' for pattern %d",
		     GET_RTX_NAME (GET_CODE (value)), id->insn_index);
	    }

	  if ((attr = find_attr (XSTR (XEXP (value, 0), 0), 0)) == NULL)
	    fatal ("Unknown attribute `%s' for pattern number %d",
		   XSTR (XEXP (value, 0), 0), id->insn_index);

	  XVECEXP (id->def, id->vec_idx, i) = value;
	  check_attr_value (XEXP (value, 1), attr);
	}
    }
}

/* Given a constant SYMBOL_REF expression, convert to a COND that
   explicitly tests each enumerated value.  */

static rtx
convert_const_symbol_ref (exp, attr)
     rtx exp;
     struct attr_desc *attr;
{
  rtx condexp;
  struct attr_value *av;
  int i;
  int num_alt = 0;

  for (av = attr->first_value; av; av = av->next)
    num_alt++;

  /* Make a COND with all tests but the last, and in the original order.
     Select the last value via the default.  Note that the attr values
     are constructed in reverse order.  */

  condexp = rtx_alloc (COND);
  XVEC (condexp, 0) = rtvec_alloc ((num_alt - 1) * 2);
  av = attr->first_value;
  XEXP (condexp, 1) = av->value;

  for (i = num_alt - 2; av = av->next, i >= 0; i--)
    {
      char * p;
      rtx value;

      XVECEXP (condexp, 0, 2 * i) = rtx_alloc (EQ);
      XEXP (XVECEXP (condexp, 0, 2 * i), 0) = exp;
      XEXP (XVECEXP (condexp, 0, 2 * i), 1) = value = rtx_alloc (SYMBOL_REF);
      RTX_UNCHANGING_P (value) = 1;
      XSTR (value, 0) = p = (char *) xmalloc (2
					      + strlen (attr->name)
					      + strlen (XSTR (av->value, 0)));
      strcpy (p, attr->name);
      strcat (p, "_");
      strcat (p, XSTR (av->value, 0));
      for (; *p != '\0'; p++)
	if (*p >= 'a' && *p <= 'z')
	  *p -= 'a' - 'A';

      XVECEXP (condexp, 0, 2 * i + 1) = av->value;
    }

  return condexp;
}

/* Given a valid expression for an attribute value, remove any IF_THEN_ELSE
   expressions by converting them into a COND.  This removes cases from this
   program.  Also, replace an attribute value of "*" with the default attribute
   value.  */

static rtx
make_canonical (attr, exp)
     struct attr_desc *attr;
     rtx exp;
{
  int i;
  rtx newexp;

  switch (GET_CODE (exp))
    {
    case CONST_INT:
      exp = make_numeric_value (INTVAL (exp));
      break;

    case CONST_STRING:
      if (! strcmp (XSTR (exp, 0), "*"))
	{
	  if (attr == 0 || attr->default_val == 0)
	    fatal ("(attr_value \"*\") used in invalid context.");
	  exp = attr->default_val->value;
	}

      break;

    case SYMBOL_REF:
      if (!attr->is_const || RTX_UNCHANGING_P (exp))
	break;
      RTX_UNCHANGING_P (exp) = 1;
      exp = convert_const_symbol_ref (exp, attr);
      check_attr_value (exp, attr);
      /* Goto COND case since this is now a COND.  Note that while the
         new expression is rescanned, all symbol_ref notes are mared as
	 unchanging.  */
      goto cond;

    case IF_THEN_ELSE:
      newexp = rtx_alloc (COND);
      XVEC (newexp, 0) = rtvec_alloc (2);
      XVECEXP (newexp, 0, 0) = XEXP (exp, 0);
      XVECEXP (newexp, 0, 1) = XEXP (exp, 1);

      XEXP (newexp, 1) = XEXP (exp, 2);

      exp = newexp;
      /* Fall through to COND case since this is now a COND.  */

    case COND:
    cond:
      /* First, check for degenerate COND. */
      if (XVECLEN (exp, 0) == 0)
	return make_canonical (attr, XEXP (exp, 1));

      for (i = 0; i < XVECLEN (exp, 0); i += 2)
	XVECEXP (exp, 0, i + 1)
		= make_canonical (attr, XVECEXP (exp, 0, i + 1));

      XEXP (exp, 1) = make_canonical (attr, XEXP (exp, 1));
      break;
    }

  return exp;
}

/* Given a value and an attribute description, return a `struct attr_value *'
   that represents that value.  This is either an existing structure, if the
   value has been previously encountered, or a newly-created structure.

   `insn_code' is the code of an insn whose attribute has the specified
   value (-2 if not processing an insn).  We ensure that all insns for
   a given value have the same number of alternatives if the value checks
   alternatives.  */

static struct attr_value *
get_attr_value (value, attr, insn_code)
     rtx value;
     struct attr_desc *attr;
     int insn_code;
{
  struct attr_value *av;
  int num_alt = 0;

  value = make_canonical (attr, value);
  if (compares_alternatives_p (value))
    {
      if (insn_code < 0 || insn_alternatives == NULL)
	fatal ("(eq_attr \"alternatives\" ...) used in non-insn context");
      else
	num_alt = insn_alternatives[insn_code];
    }

  for (av = attr->first_value; av; av = av->next)
    if (rtx_equal_p (value, av->value)
	&& (num_alt == 0 || av->first_insn == NULL
	    || insn_alternatives[av->first_insn->insn_code]))
      return av;

  av = (struct attr_value *) xmalloc (sizeof (struct attr_value));
  av->value = value;
  av->next = attr->first_value;
  attr->first_value = av;
  av->first_insn = NULL;
  av->num_insns = 0;
  av->has_asm_insn = 0;

  return av;
}

/* After all DEFINE_DELAYs have been read in, create internal attributes
   to generate the required routines.

   First, we compute the number of delay slots for each insn (as a COND of
   each of the test expressions in DEFINE_DELAYs).  Then, if more than one
   delay type is specified, we compute a similar function giving the
   DEFINE_DELAY ordinal for each insn.

   Finally, for each [DEFINE_DELAY, slot #] pair, we compute an attribute that
   tells whether a given insn can be in that delay slot.

   Normal attrbute filling and optimization expands these to contain the
   information needed to handle delay slots.  */

static void
expand_delays ()
{
  struct delay_desc *delay;
  rtx condexp;
  rtx newexp;
  int i;
  char *p;

  /* First, generate data for `num_delay_slots' function.  */

  condexp = rtx_alloc (COND);
  XVEC (condexp, 0) = rtvec_alloc (num_delays * 2);
  XEXP (condexp, 1) = make_numeric_value (0);

  for (i = 0, delay = delays; delay; i += 2, delay = delay->next)
    {
      XVECEXP (condexp, 0, i) = XEXP (delay->def, 0);
      XVECEXP (condexp, 0, i + 1)
	= make_numeric_value (XVECLEN (delay->def, 1) / 3);
    }

  make_internal_attr ("*num_delay_slots", condexp, 0);

  /* If more than one delay type, do the same for computing the delay type.  */
  if (num_delays > 1)
    {
      condexp = rtx_alloc (COND);
      XVEC (condexp, 0) = rtvec_alloc (num_delays * 2);
      XEXP (condexp, 1) = make_numeric_value (0);

      for (i = 0, delay = delays; delay; i += 2, delay = delay->next)
	{
	  XVECEXP (condexp, 0, i) = XEXP (delay->def, 0);
	  XVECEXP (condexp, 0, i + 1) = make_numeric_value (delay->num);
	}

      make_internal_attr ("*delay_type", condexp, 1);
    }

  /* For each delay possibility and delay slot, compute an eligability
     attribute for non-anulled insns and for each type of annulled (annul
     if true and annul if false).  */
 for (delay = delays; delay; delay = delay->next)
   {
     for (i = 0; i < XVECLEN (delay->def, 1); i += 3)
       {
	 condexp = XVECEXP (delay->def, 1, i);
	 if (condexp == 0) condexp = false_rtx;
	 newexp = attr_rtx (IF_THEN_ELSE, condexp,
			    make_numeric_value (1), make_numeric_value (0));

	 p = attr_printf (13, "*delay_%d_%d", delay->num, i / 3);
	 make_internal_attr (p, newexp, 1);

	 if (have_annul_true)
	   {
	     condexp = XVECEXP (delay->def, 1, i + 1);
	     if (condexp == 0) condexp = false_rtx;
	     newexp = attr_rtx (IF_THEN_ELSE, condexp,
				make_numeric_value (1),
				make_numeric_value (0));
	     p = attr_printf (18, "*annul_true_%d_%d", delay->num, i / 3);
	     make_internal_attr (p, newexp, 1);
	   }

	 if (have_annul_false)
	   {
	     condexp = XVECEXP (delay->def, 1, i + 2);
	     if (condexp == 0) condexp = false_rtx;
	     newexp = attr_rtx (IF_THEN_ELSE, condexp,
				make_numeric_value (1),
				make_numeric_value (0));
	     p = attr_printf (18, "*annul_false_%d_%d", delay->num, i / 3);
	     make_internal_attr (p, newexp, 1);
	   }
       }
   }
}

/* This function is given a left and right side expression and an operator.
   Each side is a conditional expression, each alternative of which has a
   numerical value.  The function returns another conditional expression
   which, for every possible set of condition values, returns a value that is
   the operator applied to the values of the two sides.

   Since this is called early, it must also support IF_THEN_ELSE.  */

static rtx
operate_exp (op, left, right)
     enum operator op;
     rtx left, right;
{
  int left_value, right_value;
  rtx newexp;
  int i;

  /* If left is a string, apply operator to it and the right side.  */
  if (GET_CODE (left) == CONST_STRING)
    {
      /* If right is also a string, just perform the operation.  */
      if (GET_CODE (right) == CONST_STRING)
	{
	  left_value = atoi (XSTR (left, 0));
	  right_value = atoi (XSTR (right, 0));
	  switch (op)
	    {
	    case PLUS_OP:
	      i = left_value + right_value;
	      break;

	    case MINUS_OP:
	      i = left_value - right_value;
	      break;

	    case OR_OP:
	      i = left_value | right_value;
	      break;

	    case MAX_OP:
	      if (left_value > right_value)
		i = left_value;
	      else
		i = right_value;
	      break;

	    default:
	      abort ();
	    }

	  return make_numeric_value (i);
	}
      else if (GET_CODE (right) == IF_THEN_ELSE)
	{
	  /* Apply recursively to all values within.  */
	  return attr_rtx (IF_THEN_ELSE, XEXP (right, 0),
			   operate_exp (op, left, XEXP (right, 1)),
			   operate_exp (op, left, XEXP (right, 2)));
	}
      else if (GET_CODE (right) == COND)
	{
	  newexp = rtx_alloc (COND);
	  XVEC (newexp, 0) = rtvec_alloc (XVECLEN (right, 0));
	  for (i = 0; i < XVECLEN (right, 0); i += 2)
	    {
	      XVECEXP (newexp, 0, i) = XVECEXP (right, 0, i);
	      XVECEXP (newexp, 0, i + 1)
		= operate_exp (op, left, XVECEXP (right, 0, i + 1));
	    }

	  XEXP (newexp, 1) = operate_exp (op, left, XEXP (right, 1));

	  return newexp;
	}
      else
	fatal ("Badly formed attribute value");
    }

  /* Otherwise, do recursion the other way.  */
  else if (GET_CODE (left) == IF_THEN_ELSE)
    {
      return attr_rtx (IF_THEN_ELSE, XEXP (left, 0),
		       operate_exp (op, XEXP (left, 1), right),
		       operate_exp (op, XEXP (left, 2), right));
    }

  else if (GET_CODE (left) == COND)
    {
      newexp = rtx_alloc (COND);
      XVEC (newexp, 0) = rtvec_alloc (XVECLEN (left, 0));
      for (i = 0; i < XVECLEN (left, 0); i += 2)
	{
	  XVECEXP (newexp, 0, i) = XVECEXP (left, 0, i);
	  XVECEXP (newexp, 0, i + 1)
	    = operate_exp (op, XVECEXP (left, 0, i + 1), right);
	}

      XEXP (newexp, 1) = operate_exp (op, XEXP (left, 1), right);

      return newexp;
    }

  else
    fatal ("Badly formed attribute value.");
  /* NOTREACHED */
  return NULL;
}

/* Once all attributes and DEFINE_FUNCTION_UNITs have been read, we
   construct a number of attributes.

   The first produces a function `function_units_used' which is given an
   insn and produces a mask showing which function units are required for
   the execution of that insn.

   The second produces a function `result_ready_cost' which is used to
   determine the time that the result of an insn will be ready and hence
   a worst-case schedule.

   Both of these produce quite complex expressions which are then set as the
   default value of internal attributes.  Normal attribute simplification
   should produce reasonable expressions.

   For each unit, a `<name>_unit_ready_cost' function will take an
   insn and give the delay until that unit will be ready with the result
   and a `<name>_unit_busy_delay' function is given an insn already
   executing on the unit and a candidate to execute and will give the
   cost from the time the executing insn started until the candidate
   can start (ignore limitations on the number of simultaneous insns).  */

static void
expand_units ()
{
  struct function_unit *unit;
  struct function_unit_op *op;
  rtx unitsmask;
  rtx readycost;
  rtx newexp;
  char *str;

  /* Initially, cost and masks are zero.  */
  unitsmask = readycost = make_numeric_value (0);

  /* Set up a conditional for costs and unit mask.  */
  newexp = rtx_alloc (IF_THEN_ELSE);
  XEXP (newexp, 2) = make_numeric_value (0);

  /* For each unit, insert its contribution to the above three values.  */
  for (unit = units; unit; unit = unit->next)
    {
      /* An expression that computes the ready cost for this unit.  */
      rtx readyexp = rtx_alloc (COND);
      /* An expression that maps insns to operation number for conflicts.  */
      rtx caseexp = rtx_alloc (COND);

      XVEC (readyexp, 0) = rtvec_alloc ((unit->num_opclasses - 1) * 2);
      XVEC (caseexp, 0) = rtvec_alloc ((unit->num_opclasses - 1) * 2);

      for (op = unit->ops; op; op = op->next)
	{
	  /* Validate the expressions we were given for the conditions
	     and busy cost.  Then make an attribute for use in the conflict
	     function.  */
	  op->condexp = check_attr_test (op->condexp, 0);
	  check_attr_value (op->busyexp, 0);
	  str = attr_printf (strlen (unit->name) + 11, "*%s_case_%d",
			     unit->name, op->num);
	  make_internal_attr (str, make_canonical (0, op->busyexp));

	  /* Make our adjustment to the two COND's being computed.  If we are
	     the last operation class, place our values into the default of
	     the COND.  */
	  if (op->num == unit->num_opclasses - 1)
	    {
	      XEXP (readyexp, 1) = make_numeric_value (op->ready);
	      XEXP (caseexp, 1) = make_numeric_value (op->num);
	    }
	  else
	    {
	      XVECEXP (readyexp, 0, op->num * 2) = op->condexp;
	      XVECEXP (readyexp, 0, op->num * 2 + 1)
		= make_numeric_value (op->ready);
	      XVECEXP (caseexp, 0, op->num * 2) = op->condexp;
	      XVECEXP (caseexp, 0, op->num * 2 + 1)
		= make_numeric_value (op->num);
	    }
	}

      /* Make an attribute for the case number and ready delay.  */
      str = attr_printf (strlen (unit->name) + 8, "*%s_cases", unit->name);
      make_internal_attr (str, caseexp, 1);

      str = attr_printf (strlen (unit->name) + 20, "*%s_unit_ready_cost",
			 unit->name);
      make_internal_attr (str, readyexp, 0);

      /* Merge this function unit into the ready cost and unit mask
	 attributes.  */
      XEXP (newexp, 0) = check_attr_test (unit->condexp, 0);
      XEXP (newexp, 1) = make_numeric_value (1 << unit->num);
      unitsmask = operate_exp (OR_OP, unitsmask, newexp);

      XEXP (newexp, 1) = readyexp;
      readycost = operate_exp (MAX_OP, readycost, newexp);
    }

  make_internal_attr ("*function_units_used", unitsmask, 0);
  make_internal_attr ("*result_ready_cost", readycost, 0);
}

/* Once all attributes and insns have been read and checked, we construct for
   each attribute value a list of all the insns that have that value for
   the attribute.  */

static void
fill_attr (attr)
     struct attr_desc *attr;
{
  struct attr_value *av;
  struct insn_ent *ie;
  struct insn_def *id;
  int i;
  rtx value;

  for (id = defs; id; id = id->next)
    {
      /* If no value is specified for this insn for this attribute, use the
	 default.  */
      value = NULL;
      if (XVEC (id->def, id->vec_idx))
	for (i = 0; i < XVECLEN (id->def, id->vec_idx); i++)
	  if (! strcmp (XSTR (XEXP (XVECEXP (id->def, id->vec_idx, i), 0), 0), 
			attr->name))
	    value = XEXP (XVECEXP (id->def, id->vec_idx, i), 1);

      if (value == NULL)
	av = attr->default_val;
      else
	av = get_attr_value (value, attr, id->insn_code);

      ie = (struct insn_ent *) xmalloc (sizeof (struct insn_ent));
      ie->insn_code = id->insn_code;
      ie->insn_index = id->insn_code;
      insert_insn_ent (av, ie);
    }
}

/* Given an expression EXP, see if it is a COND or IF_THEN_ELSE that has a
   test that checks relative positions of insns (uses MATCH_DUP or PC).
   If so, replace it with what is obtained by passing the expression to
   ADDRESS_FN.  If not but it is a COND or IF_THEN_ELSE, call this routine
   recursively on each value (including the default value).  Otherwise,
   return the value returned by NO_ADDRESS_FN applied to EXP.  */

static rtx
substitute_address (exp, no_address_fn, address_fn)
     rtx exp;
     rtx (*no_address_fn) ();
     rtx (*address_fn) ();
{
  int i;
  rtx newexp;

  if (GET_CODE (exp) == COND)
    {
      /* See if any tests use addresses.  */
      address_used = 0;
      for (i = 0; i < XVECLEN (exp, 0); i += 2)
	walk_attr_value (XVECEXP (exp, 0, i));

      if (address_used)
	return (*address_fn) (exp);

      /* Make a new copy of this COND, replacing each element.  */
      newexp = rtx_alloc (COND);
      XVEC (newexp, 0) = rtvec_alloc (XVECLEN (exp, 0));
      for (i = 0; i < XVECLEN (exp, 0); i += 2)
	{
	  XVECEXP (newexp, 0, i) = XVECEXP (exp, 0, i);
	  XVECEXP (newexp, 0, i + 1)
	    = substitute_address (XVECEXP (exp, 0, i + 1),
				  no_address_fn, address_fn);
	}

      XEXP (newexp, 1) = substitute_address (XEXP (exp, 1),
					     no_address_fn, address_fn);

      return newexp;
    }

  else if (GET_CODE (exp) == IF_THEN_ELSE)
    {
      address_used = 0;
      walk_attr_value (XEXP (exp, 0));
      if (address_used)
	return (*address_fn) (exp);

      return attr_rtx (IF_THEN_ELSE,
		       substitute_address (XEXP (exp, 0),
					   no_address_fn, address_fn),
		       substitute_address (XEXP (exp, 1),
					   no_address_fn, address_fn),
		       substitute_address (XEXP (exp, 2),
					   no_address_fn, address_fn));
    }

  return (*no_address_fn) (exp);
}

/* Make new attributes from the `length' attribute.  The following are made,
   each corresponding to a function called from `shorten_branches' or
   `get_attr_length':

   *insn_default_length		This is the length of the insn to be returned
				by `get_attr_length' before `shorten_branches'
				has been called.  In each case where the length
				depends on relative addresses, the largest
				possible is used.  This routine is also used
				to compute the initial size of the insn.

   *insn_variable_length_p	This returns 1 if the insn's length depends
				on relative addresses, zero otherwise.

   *insn_current_length		This is only called when it is known that the
				insn has a variable length and returns the
				current length, based on relative addresses.
  */

static void
make_length_attrs ()
{
  static char *new_names[] = {"*insn_default_length",
			      "*insn_variable_length_p",
			      "*insn_current_length"};
  static rtx (*no_address_fn[]) () = {identity_fn, zero_fn, zero_fn};
  static rtx (*address_fn[]) () = {max_fn, one_fn, identity_fn};
  int i;
  struct attr_desc *length_attr, *new_attr;
  struct attr_value *av, *new_av;
  struct insn_ent *ie, *new_ie;

  /* See if length attribute is defined.  If so, it must be numeric.  Make
     it special so we don't output anything for it.  */
  length_attr = find_attr ("length", 0);
  if (length_attr == 0)
    return;

  if (! length_attr->is_numeric)
    fatal ("length attribute must be numeric.");

  length_attr->is_const = 0;
  length_attr->is_special = 1;

  /* Make each new attribute, in turn.  */
  for (i = 0; i < sizeof new_names / sizeof new_names[0]; i++)
    {
      make_internal_attr (new_names[i],
			  substitute_address (length_attr->default_val->value,
					      no_address_fn[i], address_fn[i]),
			  0);
      new_attr = find_attr (new_names[i], 0);
      for (av = length_attr->first_value; av; av = av->next)
	for (ie = av->first_insn; ie; ie = ie->next)
	  {
	    new_av = get_attr_value (substitute_address (av->value,
							 no_address_fn[i],
							 address_fn[i]),
				     new_attr, ie->insn_code);
	    new_ie = (struct insn_ent *) xmalloc (sizeof (struct insn_ent));
	    new_ie->insn_code = ie->insn_code;
	    new_ie->insn_index = ie->insn_index;
	    insert_insn_ent (new_av, new_ie);
	  }
    }
}

/* Utility functions called from above routine.  */

static rtx
identity_fn (exp)
     rtx exp;
{
  return exp;
}

static rtx
zero_fn (exp)
     rtx exp;
{
  return make_numeric_value (0);
}

static rtx
one_fn (exp)
     rtx exp;
{
  return make_numeric_value (1);
}

static rtx
max_fn (exp)
     rtx exp;
{
  return make_numeric_value (max_attr_value (exp));
}

/* Take a COND expression and see if any of the conditions in it can be
   simplified.  If any are known true or known false for the particular insn
   code, the COND can be further simplified.

   Also call ourselves on any COND operations that are values of this COND.

   We only do the first replacement found directly and call ourselves
   recursively for subsequent replacements.  */

static rtx
simplify_cond (exp, insn_code, insn_index)
     rtx exp;
     int insn_code, insn_index;
{
  int i, j;
  rtx newtest;
  rtx value;
  rtx newexp = exp;

  for (i = 0; i < XVECLEN (exp, 0); i += 2)
    {
      newtest = SIMPLIFY_TEST_EXP (XVECEXP (exp, 0, i), insn_code, insn_index);
      if (newtest == true_rtx)
	{
	  /* Make a new COND with any previous conditions and the value for
	     this pair as the default value.  */
	  newexp = rtx_alloc (COND);
	  XVEC (newexp, 0) = rtvec_alloc (i);
	  for (j = 0; j < i; j++)
	    XVECEXP (newexp, 0, j) = XVECEXP (exp, 0, j);

	  XEXP (newexp, 1) = XVECEXP (exp, 0, i + 1);
	  break;
	}

      else if (newtest == false_rtx)
	{
	  /* Build a new COND without this test.  */
	  newexp = rtx_alloc (COND);
	  XVEC (newexp, 0) = rtvec_alloc (XVECLEN (exp, 0) - 2);
	  for (j = 0; j < i; j++)
	    XVECEXP (newexp, 0, j) = XVECEXP (exp, 0, j);

	  for (j = i; j < XVECLEN (newexp, 0); j++)
	    XVECEXP (newexp, 0, j) = XVECEXP (exp, 0, j + 2);

	  XEXP (newexp, 1) = XEXP (exp, 1);
	  break;
	}

      else if (newtest != XVECEXP (exp, 0, i))
	{
	  newexp = rtx_alloc (COND);
	  XVEC (newexp, 0) = rtvec_alloc (XVECLEN (exp, 0));
	  for (j = 0; j < XVECLEN (exp, 0); j++)
	    XVECEXP (newexp, 0, j) = XVECEXP (exp, 0, j);
	  XEXP (newexp, 1) = XEXP (exp, 1);

	  XVECEXP (newexp, 0, i) = newtest;
	  break;
	}

      /* See if this value may need simplification.  */
      if (GET_CODE (XVECEXP (exp, 0, i + 1)) == COND)
	{
	  value = simplify_cond (XVECEXP (exp, 0, i + 1),
				 insn_code, insn_index);
	  if (value != XVECEXP (exp, 0, i + 1))
	    {
	      newexp = rtx_alloc (COND);
	      XVEC (newexp, 0) = rtvec_alloc (XVECLEN (exp, 0));
	      for (j = 0; j < XVECLEN (exp, 0); j++)
		XVECEXP (newexp, 0, j) = XVECEXP (exp, 0, j);
	      XEXP (newexp, 1) = XEXP (exp, 1);

	      XVECEXP (newexp, 0, i + 1) = value;
	      break;
	    }
	}

      /* If this is the last condition in a COND and our value is the same
	 as the default value, our test isn't needed.  */
      if (i == XVECLEN (exp, 0) - 2
	  && rtx_equal_p (XVECEXP (exp, 0, i + 1), XEXP (exp, 1)))
	{
	  newexp = rtx_alloc (COND);
	  XVEC (newexp, 0) = rtvec_alloc (XVECLEN (exp, 0) - 2);
	  for (j = 0; j < i; j++)
	    XVECEXP (newexp, 0, j) = XVECEXP (exp, 0, j);
	  XEXP (newexp, 1) = XEXP (exp, 1);
	  break;
	}

      /* If this value and the value for the next test are the same, merge the
         tests.  */
      else if (i != XVECLEN (exp, 0) - 2
	       && rtx_equal_p (XVECEXP (exp, 0, i + 1),
			       XVECEXP (exp, 0, i + 3)))
	{
	  newexp = rtx_alloc (COND);
	  XVEC (newexp, 0) = rtvec_alloc (XVECLEN (exp, 0) - 2);
	  for (j = 0; j < i; j++)
	    XVECEXP (newexp, 0, j) = XVECEXP (exp, 0, j);

	  XVECEXP (newexp, 0, j)
	    = insert_right_side (IOR, XVECEXP (exp, 0, i),
				 XVECEXP (exp, 0, i + 2),
				 insn_code, insn_index);
	  XVECEXP (newexp, 0, j + 1) = XVECEXP (exp, 0, i + 1);

	  for (j = i + 2; j < XVECLEN (newexp, 0); j++)
	    XVECEXP (newexp, 0, j) = XVECEXP (exp, 0, j + 2);

	  XEXP (newexp, 1) = XEXP (exp, 1);
	  break;
	}
    }

  /* See if default value needs simplification.  */
  if (GET_CODE (XEXP (exp, 1)) == COND)
    {
      value = simplify_cond (XEXP (exp, 1), insn_code, insn_index);
      if (value != XEXP (exp, 1))
	{
	  newexp = rtx_alloc (COND);
	  XVEC (newexp, 0) = rtvec_alloc (XVECLEN (exp, 0));
	  for (j = 0; j < XVECLEN (exp, 0); j++)
	    XVECEXP (newexp, 0, j) = XVECEXP (exp, 0, j);
	  XEXP (newexp, 1) = value;
	}
    }
  
  if (exp == newexp)
    return exp;
  else if (XVECLEN (newexp, 0) == 1)
    return XVECEXP (newexp, 0, 0);
  else
    return simplify_cond (newexp, insn_code, insn_index);
}

/* Remove an insn entry from an attribute value.  */

static void
remove_insn_ent (av, ie)
     struct attr_value *av;
     struct insn_ent *ie;
{
  struct insn_ent *previe;

  if (av->first_insn == ie)
    av->first_insn = ie->next;
  else
    {
      for (previe = av->first_insn; previe->next != ie; previe = previe->next)
	;
      previe->next = ie->next;
    }

  av->num_insns--;
  if (ie->insn_code == -1)
    av->has_asm_insn = 0;
}

/* Insert an insn entry in an attribute value list.  */

static void
insert_insn_ent (av, ie)
     struct attr_value *av;
     struct insn_ent *ie;
{
  ie->next = av->first_insn;
  av->first_insn = ie;
  av->num_insns++;
  if (ie->insn_code == -1)
    av->has_asm_insn = 1;
}

/* This is a utility routine to take an expression that is a tree of either
   AND or IOR expressions and insert a new term.  The new term will be
   inserted at the right side of the first node whose code does not match
   the root.  A new node will be created with the root's code.  Its left
   side will be the old right side and its right side will be the new
   term.

   If the `term' is itself a tree, all its leaves will be inserted.  */

static rtx
insert_right_side (code, exp, term, insn_code, insn_index)
     RTX_CODE code;
     rtx exp;
     rtx term;
     int insn_code, insn_index;
{
  rtx newexp;

  if (GET_CODE (term) == code)
    {
      exp = insert_right_side (code, exp, XEXP (term, 0),
			       insn_code, insn_index);
      exp = insert_right_side (code, exp, XEXP (term, 1),
			       insn_code, insn_index);

      return exp;
    }

  if (GET_CODE (exp) == code)
    {
      /* Make a copy of this expression and call recursively.  */
      newexp = attr_rtx (code, XEXP (exp, 0),
			 insert_right_side (code, XEXP (exp, 1),
					    term, insn_code, insn_index));
    }
  else
    {
      /* Insert the new term.  */
      newexp = attr_rtx (code, exp, term);
      }

  return SIMPLIFY_TEST_EXP (newexp, insn_code, insn_index);
}

/* If we have an expression which AND's a bunch of
	(not (eq_attrq "alternative" "n"))
   terms, we may have covered all or all but one of the possible alternatives.
   If so, we can optimize.  Similarly for IOR's of EQ_ATTR.

   This routine is passed an expression and either AND or IOR.  It returns a
   bitmask indicating which alternatives are present.  */

static int
compute_alternative_mask (exp, code)
     rtx exp;
     RTX_CODE code;
{
  if (GET_CODE (exp) == code)
    return compute_alternative_mask (XEXP (exp, 0), code)
	   | compute_alternative_mask (XEXP (exp, 1), code);

  else if (code == AND && GET_CODE (exp) == NOT
	   && GET_CODE (XEXP (exp, 0)) == EQ_ATTR
	   && XSTR (XEXP (exp, 0), 0) == alternative_name)
    return 1 << atoi (XSTR (XEXP (exp, 0), 1));

  else if (code == IOR && GET_CODE (exp) == EQ_ATTR
	   && XSTR (exp, 0) == alternative_name)
    return 1 << atoi (XSTR (exp, 1));

  else
    return 0;
}

/* Given I, a single-bit mask, return RTX to compare the `alternative'
   attribute with the value represented by that bit.  */

static rtx
make_alternative_compare (mask)
     int mask;
{
  rtx newexp;
  int i;
  char *alternative;

  /* Find the bit.  */
  for (i = 0; (mask & (1 << i)) == 0; i++)
    ;

  alternative = attr_printf (3, "%d", i);

  newexp = attr_rtx (EQ_ATTR, alternative_name, alternative);
  RTX_UNCHANGING_P (newexp) = 1;

  return newexp;
}

/* If we are processing an (eq_attr "attr" "value") test, we find the value
   of "attr" for this insn code.  From that value, we can compute a test
   showing when the EQ_ATTR will be true.  This routine performs that
   computation.  If a test condition involves an address, we leave the EQ_ATTR
   intact because addresses are only valid for the `length' attribute.  */

static rtx
evaluate_eq_attr (exp, value, insn_code, insn_index)
     rtx exp;
     rtx value;
     int insn_code, insn_index;
{
  rtx orexp, andexp;
  rtx right;
  rtx newexp;
  int i;

  if (GET_CODE (value) == CONST_STRING)
    {
      if (! strcmp (XSTR (value, 0), XSTR (exp, 1)))
	newexp = true_rtx;
      else
	newexp = false_rtx;
    }
  else if (GET_CODE (value) == COND)
    {
      /* We construct an IOR of all the cases for which the requested attribute
	 value is present.  Since we start with FALSE, if it is not present,
	 FALSE will be returned.

	 Each case is the AND of the NOT's of the previous conditions with the
	 current condition; in the default case the current condition is TRUE. 

	 For each possible COND value, call ourselves recursively.

	 The extra TRUE and FALSE expressions will be eliminated by another
	 call to the simplification routine. */

      orexp = false_rtx;
      andexp = true_rtx;

      for (i = 0; i < XVECLEN (value, 0); i += 2)
	{
	  right = insert_right_side (AND, andexp,
				     XVECEXP (value, 0, i),
				     insn_code, insn_index);
	  right = insert_right_side (AND, right,
			evaluate_eq_attr (exp, XVECEXP (value, 0, i + 1),
					   insn_code, insn_index),
				     insn_code, insn_index);
	  orexp = insert_right_side (IOR, orexp, right,
				     insn_code, insn_index);

	  /* Add this condition into the AND expression.  */
	  newexp = attr_rtx (NOT, XVECEXP (value, 0, i));
	  andexp = insert_right_side (AND, andexp, newexp,
				      insn_code, insn_index);
	}

      /* Handle the default case.  */
      right = insert_right_side (AND, andexp,
				 evaluate_eq_attr (exp, XEXP (value, 1),
						    insn_code, insn_index),
				 insn_code, insn_index);
      newexp = insert_right_side (IOR, orexp, right, insn_code, insn_index);
    }
  else
    abort ();

  /* If uses an address, must return original expression.  */

  address_used = 0;
  walk_attr_value (newexp);

  if (address_used)
    return exp;
  else
    return newexp;
}

/* This routine is called when an AND of a term with a tree of AND's is
   encountered.  If the term or its complement is present in the tree, it
   can be replaced with TRUE or FALSE, respectively.

   Note that (eq_attr "att" "v1") and (eq_attr "att" "v2") cannot both
   be true and hence are complementary.  

   There is one special case:  If we see
	(and (not (eq_attr "att" "v1"))
	     (eq_attr "att" "v2"))
   this can be replaced by (eq_attr "att" "v2").  To do this we need to
   replace the term, not anything in the AND tree.  So we pass a pointer to
   the term.  */

static rtx
simplify_and_tree (exp, pterm, insn_code, insn_index)
     rtx exp;
     rtx *pterm;
     int insn_code, insn_index;
{
  rtx left, right;
  rtx newexp;
  rtx temp;
  int left_eliminates_term, right_eliminates_term;

  if (GET_CODE (exp) == AND)
    {
      left = simplify_and_tree (XEXP (exp, 0), pterm,  insn_code, insn_index);
      right = simplify_and_tree (XEXP (exp, 1), pterm, insn_code, insn_index);
      if (left != XEXP (exp, 0) || right != XEXP (exp, 1))
	{
	  newexp = attr_rtx (GET_CODE (exp), left, right);

	  exp = SIMPLIFY_TEST_EXP (newexp, insn_code, insn_index);
	}
    }

  else if (GET_CODE (exp) == IOR)
    {
      /* For the IOR case, we do the same as above, except that we can
         only eliminate `term' if both sides of the IOR would do so.  */
      temp = *pterm;
      left = simplify_and_tree (XEXP (exp, 0), &temp,  insn_code, insn_index);
      left_eliminates_term = (temp == true_rtx);

      temp = *pterm;
      right = simplify_and_tree (XEXP (exp, 1), &temp, insn_code, insn_index);
      right_eliminates_term = (temp == true_rtx);

      if (left_eliminates_term && right_eliminates_term)
	*pterm = true_rtx;

      if (left != XEXP (exp, 0) || right != XEXP (exp, 1))
	{
	  newexp = attr_rtx (GET_CODE (exp), left, right);

	  exp = SIMPLIFY_TEST_EXP (newexp, insn_code, insn_index);
	}
    }

  /* Check for simplifications.  Do some extra checking here since this
     routine is called so many times.  */

  if (exp == *pterm)
    return true_rtx;

  else if (GET_CODE (exp) == NOT && XEXP (exp, 0) == *pterm)
    return false_rtx;

  else if (GET_CODE (*pterm) == NOT && exp == XEXP (*pterm, 0))
    return false_rtx;

  else if (GET_CODE (exp) == EQ_ATTR && GET_CODE (*pterm) == EQ_ATTR)
    {
      if (XSTR (exp, 0) != XSTR (*pterm, 0))
	return exp;

      if (! strcmp (XSTR (exp, 1), XSTR (*pterm, 1)))
	return true_rtx;
      else
	return false_rtx;
    }

  else if (GET_CODE (*pterm) == EQ_ATTR && GET_CODE (exp) == NOT
	   && GET_CODE (XEXP (exp, 0)) == EQ_ATTR)
    {
      if (XSTR (*pterm, 0) != XSTR (XEXP (exp, 0), 0))
	return exp;

      if (! strcmp (XSTR (*pterm, 1), XSTR (XEXP (exp, 0), 1)))
	return false_rtx;
      else
	return true_rtx;
    }

  else if (GET_CODE (exp) == EQ_ATTR && GET_CODE (*pterm) == NOT
	   && GET_CODE (XEXP (*pterm, 0)) == EQ_ATTR)
    {
      if (XSTR (exp, 0) != XSTR (XEXP (*pterm, 0), 0))
	return exp;

      if (! strcmp (XSTR (exp, 1), XSTR (XEXP (*pterm, 0), 1)))
	return false_rtx;
      else
	*pterm = true_rtx;
    }

  else if (GET_CODE (exp) == NOT && GET_CODE (*pterm) == NOT)
    {
      if (rtx_equal_p (XEXP (exp, 0), XEXP (*pterm, 0)))
	return true_rtx;
    }

  else if (GET_CODE (exp) == NOT)
    {
      if (rtx_equal_p (XEXP (exp, 0), *pterm))
	return false_rtx;
    }

  else if (GET_CODE (*pterm) == NOT)
    {
      if (rtx_equal_p (XEXP (*pterm, 0), exp))
	return false_rtx;
    }

  else if (rtx_equal_p (exp, *pterm))
    return true_rtx;

  return exp;
}

/* Similiar to `simplify_and_tree', but for IOR trees.  */

static rtx
simplify_or_tree (exp, pterm, insn_code, insn_index)
     rtx exp;
     rtx *pterm;
     int insn_code, insn_index;
{
  rtx left, right;
  rtx newexp;
  rtx temp;
  int left_eliminates_term, right_eliminates_term;

  if (GET_CODE (exp) == IOR)
    {
      left = simplify_or_tree (XEXP (exp, 0), pterm,  insn_code, insn_index);
      right = simplify_or_tree (XEXP (exp, 1), pterm, insn_code, insn_index);
      if (left != XEXP (exp, 0) || right != XEXP (exp, 1))
	{
	  newexp = attr_rtx (GET_CODE (exp), left, right);

	  exp = SIMPLIFY_TEST_EXP (newexp, insn_code, insn_index);
	}
    }

  else if (GET_CODE (exp) == AND)
    {
      /* For the AND case, we do the same as above, except that we can
         only eliminate `term' if both sides of the AND would do so.  */
      temp = *pterm;
      left = simplify_or_tree (XEXP (exp, 0), &temp,  insn_code, insn_index);
      left_eliminates_term = (temp == false_rtx);

      temp = *pterm;
      right = simplify_or_tree (XEXP (exp, 1), &temp, insn_code, insn_index);
      right_eliminates_term = (temp == false_rtx);

      if (left_eliminates_term && right_eliminates_term)
	*pterm = false_rtx;

      if (left != XEXP (exp, 0) || right != XEXP (exp, 1))
	{
	  newexp = attr_rtx (GET_CODE (exp), left, right);

	  exp = SIMPLIFY_TEST_EXP (newexp, insn_code, insn_index);
	}
    }

  if (rtx_equal_p (exp, *pterm))
    return false_rtx;

  else if (GET_CODE (exp) == NOT && rtx_equal_p (XEXP (exp, 0), *pterm))
    return true_rtx;

  else if (GET_CODE (*pterm) == NOT && rtx_equal_p (XEXP (*pterm, 0), exp))
    return true_rtx;

  else if (GET_CODE (*pterm) == EQ_ATTR && GET_CODE (exp) == NOT
	   && GET_CODE (XEXP (exp, 0)) == EQ_ATTR
	   && XSTR (*pterm, 0) == XSTR (XEXP (exp, 0), 0))
    *pterm = false_rtx;

  else if (GET_CODE (exp) == EQ_ATTR && GET_CODE (*pterm) == NOT
	   && GET_CODE (XEXP (*pterm, 0)) == EQ_ATTR
	   && XSTR (exp, 0) == XSTR (XEXP (*pterm, 0), 0))
    return false_rtx;

  return exp;
}

/* Given an expression, see if it can be simplified for a particular insn
   code based on the values of other attributes being tested.  This can
   eliminate nested get_attr_... calls.

   Note that if an endless recursion is specified in the patterns, the 
   optimization will loop.  However, it will do so in precisely the cases where
   an infinite recursion loop could occur during compilation.  It's better that
   it occurs here!  */

static rtx
simplify_test_exp (exp, insn_code, insn_index)
     rtx exp;
     int insn_code, insn_index;
{
  rtx left, right;
  struct attr_desc *attr;
  struct attr_value *av;
  struct insn_ent *ie;
  int i;
  rtx newexp = exp;

  switch (GET_CODE (exp))
    {
    case AND:
      left = SIMPLIFY_TEST_EXP (XEXP (exp, 0), insn_code, insn_index);
      right = SIMPLIFY_TEST_EXP (XEXP (exp, 1), insn_code, insn_index);

      /* If either side is an IOR and we have (eq_attr "alternative" ..")
	 present on both sides, apply the distributive law since this will
	 yield simplications.  */
      if ((GET_CODE (left) == IOR || GET_CODE (right) == IOR)
	  && compute_alternative_mask (left, IOR)
	  && compute_alternative_mask (right, IOR))
	{
	  if (GET_CODE (left) == IOR)
	    {
	      rtx tem = left;
	      left = right;
	      right = tem;
	    }

	  newexp = attr_rtx (IOR,
			     attr_rtx (AND, left, XEXP (right, 0)),
			     attr_rtx (AND, left, XEXP (right, 1)));

	  return SIMPLIFY_TEST_EXP (newexp, insn_code, insn_index);
	}

      /* Try with the term on both sides.  */
      right = simplify_and_tree (right, &left, insn_code, insn_index);
      if (left == XEXP (exp, 0) && right == XEXP (exp, 1))
	left = simplify_and_tree (left, &right, insn_code, insn_index);

      if (left == false_rtx || right == false_rtx)
	return false_rtx;
      else if (left == true_rtx)
	return right;
      else if (right == true_rtx)
	return left;

      /* See if all or all but one of the insn's alternatives are specified
	 in this tree.  Optimize if so.  */

      else if (insn_code >= 0
	       && (GET_CODE (left) == AND
		   || (GET_CODE (left) == NOT
		       && GET_CODE (XEXP (left, 0)) == EQ_ATTR
		       && XSTR (XEXP (left, 0), 0) == alternative_name)
		   || GET_CODE (right) == AND
		   || (GET_CODE (right) == NOT
		       && GET_CODE (XEXP (right, 0)) == EQ_ATTR
		       && XSTR (XEXP (right, 0), 0) == alternative_name)))
	{
	  i = compute_alternative_mask (exp, AND);
	  if (i & ~insn_alternatives[insn_code])
	    fatal ("Illegal alternative specified for pattern number %d",
		   insn_index);

	  /* If all alternatives are excluded, this is false. */
	  i ^= insn_alternatives[insn_code];
	  if (i == 0)
	    return false_rtx;
	  else if ((i & (i - 1)) == 0 && insn_alternatives[insn_code] > 1)
	    {
	      /* If just one excluded, AND a comparison with that one to the
		 front of the tree.  The others will be eliminated by
		 optimization.  We do not want to do this if the insn has one
		 alternative and we have tested none of them!  */
	      left = make_alternative_compare (i);
	      right = simplify_and_tree (exp, &left, insn_code, insn_index);
	      newexp = attr_rtx (AND, left, right);

	      return SIMPLIFY_TEST_EXP (newexp, insn_code, insn_index);
	    }
	}

      if (left != XEXP (exp, 0) || right != XEXP (exp, 1))
	{
	  newexp = attr_rtx (AND, left, right);
	  return SIMPLIFY_TEST_EXP (newexp, insn_code, insn_index);
	}
      break;

    case IOR:
      left = SIMPLIFY_TEST_EXP (XEXP (exp, 0), insn_code, insn_index);
      right = SIMPLIFY_TEST_EXP (XEXP (exp, 1), insn_code, insn_index);

      right = simplify_or_tree (right, &left, insn_code, insn_index);
      if (left == XEXP (exp, 0) && right == XEXP (exp, 1))
	left = simplify_or_tree (left, &right, insn_code, insn_index);

      if (right == true_rtx || left == true_rtx)
	return true_rtx;
      else if (left == false_rtx)
	return right;
      else if (right == false_rtx)
	return left;

      /* Test for simple cases where the distributive law is useful.  I.e.,
	    convert (ior (and (x) (y))
			 (and (x) (z)))
	    to      (and (x)
			 (ior (y) (z)))
       */

      else if (GET_CODE (left) == AND && GET_CODE (right) == AND
	  && rtx_equal_p (XEXP (left, 0), XEXP (right, 0)))
	{
	  newexp = attr_rtx (IOR, XEXP (left, 1), XEXP (right, 1));

	  left = XEXP (left, 0);
	  right = newexp;
	  newexp = attr_rtx (AND, left, right);
	  return SIMPLIFY_TEST_EXP (newexp, insn_code, insn_index);
	}

      /* See if all or all but one of the insn's alternatives are specified
	 in this tree.  Optimize if so.  */

      else if (insn_code >= 0
	  && (GET_CODE (left) == IOR
	      || (GET_CODE (left) == EQ_ATTR
		  && XSTR (left, 0) == alternative_name)
	      || GET_CODE (right) == IOR
	      || (GET_CODE (right) == EQ_ATTR
		  && XSTR (right, 0) == alternative_name)))
	{
	  i = compute_alternative_mask (exp, IOR);
	  if (i & ~insn_alternatives[insn_code])
	    fatal ("Illegal alternative specified for pattern number %d",
		   insn_index);

	  /* If all alternatives are included, this is true. */
	  i ^= insn_alternatives[insn_code];
	  if (i == 0)
	    return true_rtx;
	  else if ((i & (i - 1)) == 0 && insn_alternatives[insn_code] > 1)
	    {
	      /* If just one excluded, IOR a comparison with that one to the
		 front of the tree.  The others will be eliminated by
		 optimization.  We do not want to do this if the insn has one
		 alternative and we have tested none of them!  */
	      left = make_alternative_compare (i);
	      right = simplify_and_tree (exp, &left, insn_code, insn_index);
	      newexp = attr_rtx (IOR, attr_rtx (NOT, left), right);

	      return SIMPLIFY_TEST_EXP (newexp, insn_code, insn_index);
	    }
	}

      if (left != XEXP (exp, 0) || right != XEXP (exp, 1))
	{
	  newexp = attr_rtx (IOR, left, right);
	  return SIMPLIFY_TEST_EXP (newexp, insn_code, insn_index);
	}
      break;

    case NOT:
      left = SIMPLIFY_TEST_EXP (XEXP (exp, 0), insn_code, insn_index);
      if (GET_CODE (left) == NOT)
	return XEXP (left, 0);

      if (left == false_rtx)
	return true_rtx;
      else if (left == true_rtx)
	return false_rtx;

      /* Try to apply De`Morgan's laws.  */
      else if (GET_CODE (left) == IOR)
	{
	  newexp = attr_rtx (AND,
			     attr_rtx (NOT, XEXP (left, 0)),
			     attr_rtx (NOT, XEXP (left, 1)));

	  newexp = SIMPLIFY_TEST_EXP (newexp, insn_code, insn_index);
	}
      else if (GET_CODE (left) == AND)
	{
	  newexp = attr_rtx (IOR,
			     attr_rtx (NOT, XEXP (left, 0)),
			     attr_rtx (NOT, XEXP (left, 1)));

	  newexp = SIMPLIFY_TEST_EXP (newexp, insn_code, insn_index);
	}
      else if (left != XEXP (exp, 0))
	{
	  newexp = attr_rtx (NOT, left);
	}
      break;

    case EQ_ATTR:
      /* Look at the value for this insn code in the specified attribute.
	 We normally can replace this comparison with the condition that
	 would give this insn the values being tested for.   */
      if (XSTR (exp, 0) != alternative_name
	  && (attr = find_attr (XSTR (exp, 0), 0)) != NULL)
	for (av = attr->first_value; av; av = av->next)
	  for (ie = av->first_insn; ie; ie = ie->next)
	    if (ie->insn_code == insn_code)
	      return evaluate_eq_attr (exp, av->value, insn_code, insn_index);
    }

  /* We have already simplified this expression.  Simplifying it again
     won't buy anything unless we weren't given a valid insn code
     to process (i.e., we are canonicalizing something.).  */
  if (insn_code != -2)
    RTX_UNCHANGING_P (newexp) = 1;

  return newexp;
}

/* Optimize the attribute lists by seeing if we can determine conditional
   values from the known values of other attributes.  This will save subroutine
   calls during the compilation.  */

static void
optimize_attrs ()
{
  struct attr_desc *attr;
  struct attr_value *av;
  struct insn_ent *ie, *nextie;
  rtx newexp;
  int something_changed = 1;

  /* Loop until nothing changes for one iteration.  */
  while (something_changed)
    {
      something_changed = 0;
      for (attr = attrs; attr; attr = attr->next)
	for (av = attr->first_value; av; av = av->next)
	    for (ie = av->first_insn; ie; ie = nextie)
	      {
		nextie = ie->next;
		if (GET_CODE (av->value) != COND)
		  continue;

		newexp = simplify_cond (av->value, ie->insn_code,
					ie->insn_index);
		if (newexp != av->value)
		  {
		    remove_insn_ent (av, ie);
		    insert_insn_ent (get_attr_value (newexp, attr,
						     ie->insn_code), ie);
		    something_changed = 1;
		  }
	      }
    }
}

/* Create table entries for DEFINE_ATTR.  */

static void
gen_attr (exp)
     rtx exp;
{
  struct attr_desc *attr;
  struct attr_value *av;
  char *name_ptr;
  char *p;

  /* Make a new attribute structure.  Check for duplicate by looking at
     attr->default_val, since it is initialized by this routine.  */
  attr = find_attr (XSTR (exp, 0), 1);
  if (attr->default_val)
    fatal ("Duplicate definition for `%s' attribute", attr->name);

  if (*XSTR (exp, 1) == '\0')
      attr->is_numeric = 1;
  else
    {
      name_ptr = XSTR (exp, 1);
      while ((p = next_comma_elt (&name_ptr)) != NULL)
	{
	  av = (struct attr_value *) xmalloc (sizeof (struct attr_value));
	  av->value = attr_rtx (CONST_STRING, p);
	  av->next = attr->first_value;
	  attr->first_value = av;
	  av->first_insn = NULL;
	  av->num_insns = 0;
	  av->has_asm_insn = 0;
	}
    }

  if (GET_CODE (XEXP (exp, 2)) == CONST)
    {
      attr->is_const = 1;
      if (attr->is_numeric)
	fatal ("Constant attributes may not take numeric values");
      /* Get rid of the CONST node.  It is allowed only at top-level.  */
      XEXP (exp, 2) = XEXP (XEXP (exp, 2), 0);
    }

  if (! strcmp (attr->name, "length") && ! attr->is_numeric)
    fatal ("`length' attribute must take numeric values");

  /* Set up the default value. */
  check_attr_value (XEXP (exp, 2), attr);
  attr->default_val = get_attr_value (XEXP (exp, 2), attr, -2);
}

/* Given a pattern for DEFINE_PEEPHOLE or DEFINE_INSN, return the number of
   alternatives in the constraints.  Assume all MATCH_OPERANDs have the same
   number of alternatives as this should be checked elsewhere.  */

static int
count_alternatives (exp)
     rtx exp;
{
  int i, j, n;
  char *fmt;
  
  if (GET_CODE (exp) == MATCH_OPERAND)
    return n_comma_elts (XSTR (exp, 2));

  for (i = 0, fmt = GET_RTX_FORMAT (GET_CODE (exp));
       i < GET_RTX_LENGTH (GET_CODE (exp)); i++)
    switch (*fmt++)
      {
      case 'e':
      case 'u':
	n = count_alternatives (XEXP (exp, i));
	if (n)
	  return n;
	break;

      case 'E':
      case 'V':
	if (XVEC (exp, i) != NULL)
	  for (j = 0; j < XVECLEN (exp, i); j++)
	    {
	      n = count_alternatives (XVECEXP (exp, i, j));
	      if (n)
		return n;
	    }
      }

  return 0;
}

/* Returns non-zero if the given expression contains an EQ_ATTR with the
   `alternative' attribute.  */

static int
compares_alternatives_p (exp)
     rtx exp;
{
  int i, j;
  char *fmt;

  if (GET_CODE (exp) == EQ_ATTR && XSTR (exp, 0) == alternative_name)
    return 1;

  for (i = 0, fmt = GET_RTX_FORMAT (GET_CODE (exp));
       i < GET_RTX_LENGTH (GET_CODE (exp)); i++)
    switch (*fmt++)
      {
      case 'e':
      case 'u':
	if (compares_alternatives_p (XEXP (exp, i)))
	  return 1;
	break;

      case 'E':
	for (j = 0; j < XVECLEN (exp, i); j++)
	  if (compares_alternatives_p (XVECEXP (exp, i, j)))
	    return 1;
	break;
      }

  return 0;
}

/* Returns non-zero is INNER is contained in EXP.  */

static int
contained_in_p (inner, exp)
     rtx inner;
     rtx exp;
{
  int i, j;
  char *fmt;

  if (rtx_equal_p (inner, exp))
    return 1;

  for (i = 0, fmt = GET_RTX_FORMAT (GET_CODE (exp));
       i < GET_RTX_LENGTH (GET_CODE (exp)); i++)
    switch (*fmt++)
      {
      case 'e':
      case 'u':
	if (contained_in_p (inner, XEXP (exp, i)))
	  return 1;
	break;

      case 'E':
	for (j = 0; j < XVECLEN (exp, i); j++)
	  if (contained_in_p (inner, XVECEXP (exp, i, j)))
	    return 1;
	break;
      }

  return 0;
}
	
/* Process DEFINE_PEEPHOLE, DEFINE_INSN, and DEFINE_ASM_ATTRIBUTES.  */

static void
gen_insn (exp)
     rtx exp;
{
  struct insn_def *id;

  id = (struct insn_def *) xmalloc (sizeof (struct insn_def));
  id->next = defs;
  defs = id;
  id->def = exp;

  switch (GET_CODE (exp))
    {
    case DEFINE_INSN:
      id->insn_code = insn_code_number++;
      id->insn_index = insn_index_number++;
      id->num_alternatives = count_alternatives (exp);
      if (id->num_alternatives == 0)
	id->num_alternatives = 1;
      id->vec_idx = 4;
      break;

    case DEFINE_PEEPHOLE:
      id->insn_code = insn_code_number++;
      id->insn_index = insn_index_number++;
      id->num_alternatives = count_alternatives (exp);
      if (id->num_alternatives == 0)
	id->num_alternatives = 1;
      id->vec_idx = 3;
      break;

    case DEFINE_ASM_ATTRIBUTES:
      id->insn_code = -1;
      id->insn_index = -1;
      id->num_alternatives = 1;
      id->vec_idx = 0;
      got_define_asm_attributes = 1;
      break;
    }
}

/* Process a DEFINE_DELAY.  Validate the vector length, check if annul
   true or annul false is specified, and make a `struct delay_desc'.  */

static void
gen_delay (def)
     rtx def;
{
  struct delay_desc *delay;
  int i;

  if (XVECLEN (def, 1) % 3 != 0)
    fatal ("Number of elements in DEFINE_DELAY must be multiple of three.");

  for (i = 0; i < XVECLEN (def, 1); i += 3)
    {
      if (XVECEXP (def, 1, i + 1))
	have_annul_true = 1;
      if (XVECEXP (def, 1, i + 2))
	have_annul_false = 1;
    }
  
  delay = (struct delay_desc *) xmalloc (sizeof (struct delay_desc));
  delay->def = def;
  delay->num = ++num_delays;
  delay->next = delays;
  delays = delay;
}

/* Process a DEFINE_FUNCTION_UNIT.  

   This gives information about a function unit contained in the CPU.
   We fill in a `struct function_unit_op' and a `struct function_unit'
   with information used later by `expand_unit'.  */

static void
gen_unit (def)
     rtx def;
{
  struct function_unit *unit;
  struct function_unit_op *op;

  /* See if we have already seen this function unit.  If so, check that
     the multipicity and simultaneity values are the same.  If not, make
     a structure for this function unit.  */
  for (unit = units; unit; unit = unit->next)
    if (! strcmp (unit->name, XSTR (def, 0)))
      {
	if (unit->multiplicity != XINT (def, 1)
	    || unit->simultaneity != XINT (def, 2))
	  fatal ("Differing specifications given for `%s' function unit.",
		 unit->name);
	break;
      }

  if (unit == 0)
    {
      unit = (struct function_unit *) xmalloc (sizeof (struct function_unit));
      unit->name = XSTR (def, 0);
      unit->multiplicity = XINT (def, 1);
      unit->simultaneity = XINT (def, 2);
      unit->num = num_units++;
      unit->num_opclasses = 0;
      unit->condexp = false_rtx;
      unit->ops = 0;
      unit->next = units;
      units = unit;
    }

  /* Make a new operation class structure entry and initialize it.  */
  op = (struct function_unit_op *) xmalloc (sizeof (struct function_unit_op));
  op->condexp = XEXP (def, 3);
  op->num = unit->num_opclasses++;
  op->ready = XINT (def, 4);
  op->next = unit->ops;
  unit->ops = op;

  /* Set our busy expression based on whether or not an optional conflict
     vector was specified.  */
  if (XVEC (def, 6))
    {
      /* Compute the IOR of all the specified expressions.  */
      rtx orexp = false_rtx;
      int i;

      for (i = 0; i < XVECLEN (def, 6); i++)
	orexp = insert_right_side (IOR, orexp, XVECEXP (def, 6, i), -2);

      op->busyexp = attr_rtx (IF_THEN_ELSE, orexp,
			      make_numeric_value (XINT (def, 5)),
			      make_numeric_value (0));
    }
  else
    op->busyexp = make_numeric_value (XINT (def, 5));

  /* Merge our conditional into that of the function unit so we can determine
     which insns are used by the function unit.  */
  unit->condexp = insert_right_side (IOR, unit->condexp, op->condexp, -2);
}

/* Given a piece of RTX, print a C expression to test it's truth value.
   We use AND and IOR both for logical and bit-wise operations, so 
   interpret them as logical unless they are inside a comparison expression.
   The second operand of this function will be non-zero in that case.  */

static void
write_test_expr (exp, in_comparison)
     rtx exp;
     int in_comparison;
{
  int comparison_operator = 0;
  RTX_CODE code;
  struct attr_desc *attr;

  /* In order not to worry about operator precedence, surround our part of
     the expression with parentheses.  */

  printf ("(");
  code = GET_CODE (exp);
  switch (code)
    {
    /* Binary operators.  */
    case EQ: case NE:
    case GE: case GT: case GEU: case GTU:
    case LE: case LT: case LEU: case LTU:
      comparison_operator = 1;

    case PLUS:   case MINUS:  case MULT:     case DIV:      case MOD:
    case AND:    case IOR:    case XOR:
    case LSHIFT: case ASHIFT: case LSHIFTRT: case ASHIFTRT:
      write_test_expr (XEXP (exp, 0), in_comparison || comparison_operator);
      switch (code)
        {
	case EQ:
	  printf (" == ");
	  break;
	case NE:
	  printf (" != ");
	  break;
	case GE:
	  printf (" >= ");
	  break;
	case GT:
	  printf (" > ");
	  break;
	case GEU:
	  printf (" >= (unsigned) ");
	  break;
	case GTU:
	  printf (" > (unsigned) ");
	  break;
	case LE:
	  printf (" <= ");
	  break;
	case LT:
	  printf (" < ");
	  break;
	case LEU:
	  printf (" <= (unsigned) ");
	  break;
	case LTU:
	  printf (" < (unsigned) ");
	  break;
	case PLUS:
	  printf (" + ");
	  break;
	case MINUS:
	  printf (" - ");
	  break;
	case MULT:
	  printf (" * ");
	  break;
	case DIV:
	  printf (" / ");
	  break;
	case MOD:
	  printf (" %% ");
	  break;
	case AND:
	  if (in_comparison)
	    printf (" & ");
	  else
	    printf (" && ");
	  break;
	case IOR:
	  if (in_comparison)
	    printf (" | ");
	  else
	    printf (" || ");
	  break;
	case XOR:
	  printf (" ^ ");
	  break;
	case LSHIFT:
	case ASHIFT:
	  printf (" << ");
	  break;
	case LSHIFTRT:
	case ASHIFTRT:
	  printf (" >> ");
	  break;
        }

      write_test_expr (XEXP (exp, 1), in_comparison || comparison_operator);
      break;

    case NOT:
      /* Special-case (not (eq_attrq "alternative" "x")) */
      if (! in_comparison && GET_CODE (XEXP (exp, 0)) == EQ_ATTR
	  && XSTR (XEXP (exp, 0), 0) == alternative_name)
	{
	  printf ("which_alternative != %s", XSTR (XEXP (exp, 0), 1));
	  break;
	}

      /* Otherwise, fall through to normal unary operator.  */

    /* Unary operators.  */   
    case ABS:  case NEG:
      switch (code)
	{
	case NOT:
	  if (in_comparison)
	    printf ("~ ");
	  else
	    printf ("! ");
	  break;
	case ABS:
	  printf ("abs ");
	  break;
	case NEG:
	  printf ("-");
	  break;
	}

      write_test_expr (XEXP (exp, 0), in_comparison);
      break;

    /* Comparison test of an attribute with a value.  Most of these will
       have been removed by optimization.   Handle "alternative"
       specially and give error if EQ_ATTR present inside a comparison.  */
    case EQ_ATTR:
      if (in_comparison)
	fatal ("EQ_ATTR not valid inside comparison");

      if (XSTR (exp, 0) == alternative_name)
	{
	  printf ("which_alternative == %s", XSTR (exp, 1));
	  break;
	}

      attr = find_attr (XSTR (exp, 0), 0);
      if (! attr) abort ();
      printf ("get_attr_%s (insn) == ", attr->name);
      write_attr_valueq (attr, XSTR (exp, 1)); 
      break;

    /* See if an operand matches a predicate.  */
    case MATCH_OPERAND:
      /* If only a mode is given, just ensure the mode matches the operand.
	 If neither a mode nor predicate is given, error.  */
     if (XSTR (exp, 1) == NULL || *XSTR (exp, 1) == '\0')
	{
	  if (GET_MODE (exp) == VOIDmode)
	    fatal ("Null MATCH_OPERAND specified as test");
	  else
	    printf ("GET_MODE (operands[%d]) == %smode",
		    XINT (exp, 0), GET_MODE_NAME (GET_MODE (exp)));
	}
      else
	printf ("%s (operands[%d], %smode)",
		XSTR (exp, 1), XINT (exp, 0), GET_MODE_NAME (GET_MODE (exp)));
      break;

    /* Constant integer. */
    case CONST_INT:
      printf ("%d", XINT (exp, 0));
      break;

    /* A random C expression. */
    case SYMBOL_REF:
      printf ("%s", XSTR (exp, 0));
      break;

    /* The address of the branch target.  */
    case MATCH_DUP:
      printf ("insn_addresses[INSN_UID (JUMP_LABEL (insn))]");
      break;

    /* The address of the current insn.  It would be more consistent with
       other usage to make this the address of the NEXT insn, but this gets
       too confusing because of the ambiguity regarding the length of the
       current insn.  */
    case PC:
      printf ("insn_current_address");
      break;

    default:
      fatal ("bad RTX code `%s' in attribute calculation\n",
	     GET_RTX_NAME (code));
    }

  printf (")");
}

/* Given an attribute value, return the maximum CONST_STRING argument
   encountered.  It is assumed that they are all numeric.  */

static int
max_attr_value (exp)
     rtx exp;
{
  int current_max = 0;
  int n;
  int i;

  if (GET_CODE (exp) == CONST_STRING)
    return atoi (XSTR (exp, 0));

  else if (GET_CODE (exp) == COND)
    {
      for (i = 0; i < XVECLEN (exp, 0); i += 2)
	{
	  n = max_attr_value (XVECEXP (exp, 0, i + 1));
	  if (n > current_max)
	    current_max = n;
	}

      n = max_attr_value (XEXP (exp, 1));
      if (n > current_max)
	current_max = n;
    }

  else
    abort ();

  return current_max;
}

/* Scan an attribute value, possibly a conditional, and record what actions
   will be required to do any conditional tests in it.

   Specifically, set
	`must_extract'	  if we need to extract the insn operands
	`must_constrain'  if we must compute `which_alternative'
	`address_used'	  if an address expression was used
 */

static void
walk_attr_value (exp)
     rtx exp;
{
  register int i, j;
  register char *fmt;
  RTX_CODE code;

  if (exp == NULL)
    return;

  code = GET_CODE (exp);
  switch (code)
    {
    case SYMBOL_REF:
      if (! RTX_UNCHANGING_P (exp))
	/* Since this is an arbitrary expression, it can look at anything.
	   However, constant expressions do not depend on any particular
	   insn.  */
	must_extract = must_constrain = 1;
      return;

    case MATCH_OPERAND:
      must_extract = 1;
      return;

    case EQ_ATTR:
      if (XSTR (exp, 0) == alternative_name)
	must_extract = must_constrain = 1;
      return;

    case MATCH_DUP:
    case PC:
      address_used = 1;
      return;
    }

  for (i = 0, fmt = GET_RTX_FORMAT (code); i < GET_RTX_LENGTH (code); i++)
    switch (*fmt++)
      {
      case 'e':
      case 'u':
	walk_attr_value (XEXP (exp, i));
	break;

      case 'E':
	if (XVEC (exp, i) != NULL)
	  for (j = 0; j < XVECLEN (exp, i); j++)
	    walk_attr_value (XVECEXP (exp, i, j));
	break;
      }
}

/* Write out a function to obtain the attribute for a given INSN.  */

static void
write_attr_get (attr)
     struct attr_desc *attr;
{
  struct attr_value *av, *common_av;

  /* Find the most used attribute value.  Handle that as the `default' of the
     switch we will generate. */
  common_av = find_most_used (attr);

  /* Write out start of function, then all values with explicit `case' lines,
     then a `default', then the value with the most uses.  */
  if (attr->is_numeric)
    printf ("int\n");
  else
    printf ("enum attr_%s\n", attr->name);

  /* If the attribute name starts with a star, the remainder is the name of
     the subroutine to use, instead of `get_attr_...'.  */
  if (attr->name[0] == '*')
    printf ("%s (insn)\n", &attr->name[1]);
  else if (attr->is_const == 0)
    printf ("get_attr_%s (insn)\n", attr->name);
  else
    {
      printf ("get_attr_%s ()\n", attr->name);
      printf ("{\n");

      for (av = attr->first_value; av; av = av->next)
	if (av->num_insns != 0)
	  write_attr_set (attr, 2, av->value, "return", ";",
			  true_rtx, av->first_insn->insn_code,
			  av->first_insn->insn_index);

      printf ("}\n\n");
      return;
    }
  printf ("     rtx insn;\n");
  printf ("{\n");
  printf ("  switch (recog_memoized (insn))\n");
  printf ("    {\n");

  for (av = attr->first_value; av; av = av->next)
    if (av != common_av)
      write_attr_case (attr, av, 1, "return", ";", 4, true_rtx);

  write_attr_case (attr, common_av, 0, "return", ";", 4, true_rtx);
  printf ("    }\n}\n\n");
}

/* Given an AND tree of known true terms (because we are inside an `if' with
   that as the condition or are in an `else' clause) and an expression,
   replace any known true terms with TRUE.  Use `simplify_and_tree' to do
   the bulk of the work.  */

static rtx
eliminate_known_true (known_true, exp, insn_code, insn_index)
     rtx known_true;
     rtx exp;
     int insn_code, insn_index;
{
  rtx term;

  known_true = SIMPLIFY_TEST_EXP (known_true, insn_code, insn_index);

  if (GET_CODE (known_true) == AND)
    {
      exp = eliminate_known_true (XEXP (known_true, 0), exp,
				  insn_code, insn_index);
      exp = eliminate_known_true (XEXP (known_true, 1), exp,
				  insn_code, insn_index);
    }
  else
    {
      term = known_true;
      exp = simplify_and_tree (exp, &term, insn_code, insn_index);
    }

  return exp;
}

/* Write out a series of tests and assignment statements to perform tests and
   sets of an attribute value.  We are passed an indentation amount and prefix
   and suffix strings to write around each attribute value (e.g., "return"
   and ";").  */

static void
write_attr_set (attr, indent, value, prefix, suffix, known_true,
		insn_code, insn_index)
     struct attr_desc *attr;
     int indent;
     rtx value;
     char *prefix;
     char *suffix;
     rtx known_true;
     int insn_code, insn_index;
{
  if (GET_CODE (value) == CONST_STRING)
    {
      write_indent (indent);
      printf ("%s ", prefix);
      write_attr_value (attr, value);
      printf ("%s\n", suffix);
    }
  else if (GET_CODE (value) == COND)
    {
      /* Assume the default value will be the default of the COND unless we
	 find an always true expression.  */
      rtx default_val = XEXP (value, 1);
      rtx our_known_true = known_true;
      rtx newexp;
      int first_if = 1;
      int i;

      for (i = 0; i < XVECLEN (value, 0); i += 2)
	{
	  rtx testexp;
	  rtx inner_true;

	  testexp = eliminate_known_true (our_known_true,
					  XVECEXP (value, 0, i),
					  insn_code, insn_index);
	  newexp = attr_rtx (NOT, testexp);
	  newexp  = insert_right_side (AND, our_known_true, newexp,
				       insn_code, insn_index);

	  /* If the test expression is always true or if the next `known_true'
	     expression is always false, this is the last case, so break
	     out and let this value be the `else' case.  */
	  if (testexp == true_rtx || newexp == false_rtx)
	    {
	      default_val = XVECEXP (value, 0, i + 1);
	      break;
	    }

	  /* Compute the expression to pass to our recursive call as being
	     known true.  */
	  inner_true = insert_right_side (AND, our_known_true,
					  testexp, insn_code, insn_index);

	  /* If this is always false, skip it.  */
	  if (inner_true == false_rtx)
	    continue;

	  write_indent (indent);
	  printf ("%sif ", first_if ? "" : "else ");
	  first_if = 0;
	  write_test_expr (testexp, 0);
	  printf ("\n");
	  write_indent (indent + 2);
	  printf ("{\n");

	  write_attr_set (attr, indent + 4,  
			  XVECEXP (value, 0, i + 1), prefix, suffix,
			  inner_true, insn_code, insn_index);
	  write_indent (indent + 2);
	  printf ("}\n");
	  our_known_true = newexp;
	}

      if (! first_if)
	{
	  write_indent (indent);
	  printf ("else\n");
	  write_indent (indent + 2);
	  printf ("{\n");
	}

      write_attr_set (attr, first_if ? indent : indent + 4, default_val,
		      prefix, suffix, our_known_true, insn_code, insn_index);

      if (! first_if)
	{
	  write_indent (indent + 2);
	  printf ("}\n");
	}
    }
  else
    abort ();
}

/* Write out the computation for one attribute value.  */

static void
write_attr_case (attr, av, write_case_lines, prefix, suffix, indent, known_true)
     struct attr_desc *attr;
     struct attr_value *av;
     int write_case_lines;
     char *prefix, *suffix;
     int indent;
     rtx known_true;
{
  struct insn_ent *ie;

  if (av->num_insns == 0)
    return;

  if (av->has_asm_insn)
    {
      write_indent (indent);
      printf ("case -1:\n");
      write_indent (indent + 2);
      printf ("if (GET_CODE (PATTERN (insn)) != ASM_INPUT\n");
      write_indent (indent + 2);
      printf ("    && asm_noperands (PATTERN (insn)) < 0)\n");
      write_indent (indent + 2);
      printf ("  fatal_insn_not_found (insn);\n");
    }

  if (write_case_lines)
    {
      for (ie = av->first_insn; ie; ie = ie->next)
	if (ie->insn_code != -1)
	  {
	    write_indent (indent);
	    printf ("case %d:\n", ie->insn_code);
	  }
    }
  else
    {
      write_indent (indent);
      printf ("default:\n");
    }

  /* See what we have to do to handle output this value.  */
  must_extract = must_constrain = address_used = 0;
  walk_attr_value (av->value);

  if (must_extract)
    {
      write_indent (indent + 2);
      printf ("insn_extract (insn);\n");
    }

  if (must_constrain)
    {
#ifdef REGISTER_CONSTRAINTS
      write_indent (indent + 2);
      printf ("if (! constrain_operands (INSN_CODE (insn), reload_completed))\n");
      write_indent (indent + 2);
      printf ("  fatal_insn_not_found (insn);\n");
#endif
    }

  write_attr_set (attr, indent + 2, av->value, prefix, suffix,
		  known_true, av->first_insn->insn_code,
		  av->first_insn->insn_index);

  if (strncmp (prefix, "return", 6))
    {
      write_indent (indent + 2);
      printf ("break;\n");
    }
  printf ("\n");
}

/* Utilities to write names in various forms.  */

static void
write_attr_valueq (attr, s)
     struct attr_desc *attr;
     char *s;
{
  if (attr->is_numeric)
    printf ("%s", s);
  else
    {
      write_upcase (attr->name);
      printf ("_");
      write_upcase (s);
    }
}

static void
write_attr_value (attr, value)
     struct attr_desc *attr;
     rtx value;
{
  if (GET_CODE (value) != CONST_STRING)
    abort ();

  write_attr_valueq (attr, XSTR (value, 0));
}

static void
write_upcase (str)
     char *str;
{
  while (*str)
    if (*str < 'a' || *str > 'z')
      printf ("%c", *str++);
    else
      printf ("%c", *str++ - 'a' + 'A');
}

static void
write_indent (indent)
     int indent;
{
  for (; indent > 8; indent -= 8)
    printf ("\t");

  for (; indent; indent--)
    printf (" ");
}

/* Write a subroutine that is given an insn that requires a delay slot, a
   delay slot ordinal, and a candidate insn.  It returns non-zero if the
   candidate can be placed in the specified delay slot of the insn.

   We can write as many as three subroutines.  `eligible_for_delay'
   handles normal delay slots, `eligible_for_annul_true' indicates that
   the specified insn can be annulled if the branch is true, and likewise
   for `eligible_for_annul_false'.

   KIND is a string distingushing these three cases ("delay", "annul_true",
   or "annul_false").  */

static void
write_eligible_delay (kind)
     char *kind;
{
  struct delay_desc *delay;
  int max_slots;
  char str[50];
  struct attr_desc *attr;
  struct attr_value *av, *common_av;
  int i;

  /* Compute the maximum number of delay slots required.  We use the delay
     ordinal times this number plus one, plus the slot number as an index into
     the appropriate predicate to test.  */

  for (delay = delays, max_slots = 0; delay; delay = delay->next)
    if (XVECLEN (delay->def, 1) / 3 > max_slots)
      max_slots = XVECLEN (delay->def, 1) / 3;

  /* Write function prelude.  */

  printf ("int\n");
  printf ("eligible_for_%s (delay_insn, slot, candidate_insn)\n", kind);
  printf ("     rtx delay_insn;\n");
  printf ("     int slot;\n");
  printf ("     rtx candidate_insn;\n");
  printf ("{\n");
  printf ("  rtx insn;\n");
  printf ("\n");
  printf ("  if (slot >= %d)\n", max_slots);
  printf ("    abort ();\n");
  printf ("\n");

  /* If more than one delay type, find out which type the delay insn is.  */

  if (num_delays > 1)
    {
      attr = find_attr ("*delay_type", 0);
      if (! attr) abort ();
      common_av = find_most_used (attr);

      printf ("  insn = delay_insn;\n");
      printf ("  switch (recog_memoized (insn))\n");
      printf ("    {\n");

      sprintf (str, " * %d;\n      break;", max_slots);
      for (av = attr->first_value; av; av = av->next)
	if (av != common_av)
	  write_attr_case (attr, av, 1, "slot +=", str, 4, true_rtx);

      write_attr_case (attr, common_av, 0, "slot +=", str, 4, true_rtx);
      printf ("    }\n\n");

      /* Ensure matched.  Otherwise, shouldn't have been called.  */
      printf ("  if (slot < %d)\n", max_slots);
      printf ("    abort ();\n\n");
    }

  /* If just one type of delay slot, write simple switch.  */
  if (num_delays == 1 && max_slots == 1)
    {
      printf ("  insn = candidate_insn;\n");
      printf ("  switch (recog_memoized (insn))\n");
      printf ("    {\n");

      attr = find_attr ("*delay_1_0", 0);
      if (! attr) abort ();
      common_av = find_most_used (attr);

      for (av = attr->first_value; av; av = av->next)
	if (av != common_av)
	  write_attr_case (attr, av, 1, "return", ";", 4, true_rtx);

      write_attr_case (attr, common_av, 0, "return", ";", 4, true_rtx);
      printf ("    }\n");
    }

  else
    {
      /* Write a nested CASE.  The first indicates which condition we need to
	 test, and the inner CASE tests the condition.  */
      printf ("  insn = candidate_insn;\n");
      printf ("  switch (slot)\n");
      printf ("    {\n");

      for (delay = delays; delay; delay = delay->next)
	for (i = 0; i < XVECLEN (delay->def, 1); i += 3)
	  {
	    printf ("    case %d:\n",
		    (i / 3) + (num_delays == 1 ? 0 : delay->num * max_slots));
	    printf ("      switch (recog_memoized (insn))\n");
	    printf ("\t{\n");

	    sprintf (str, "*%s_%d_%d", kind, delay->num, i / 3);
	    attr = find_attr (str, 0);
	    if (! attr) abort ();
	    common_av = find_most_used (attr);

	    for (av = attr->first_value; av; av = av->next)
	      if (av != common_av)
		write_attr_case (attr, av, 1, "return", ";", 8, true_rtx);

	    write_attr_case (attr, common_av, 0, "return", ";", 8, true_rtx);
	    printf ("      }\n");
	  }

      printf ("    default:\n");
      printf ("      abort ();\n");     
      printf ("    }\n");
    }

  printf ("}\n\n");
}

/* Write routines to compute conflict cost for function units.  Then write a
   table describing the available function units.  */

static void
write_function_unit_info ()
{
  struct function_unit *unit;
  struct attr_desc *case_attr, *attr;
  struct attr_value *av, *common_av;
  rtx value;
  char *str;
  int using_case;
  int i;

  /* Write out conflict routines for function units.  Don't bother writing
     one if there is only one busy value.  */

  for (unit = units; unit; unit = unit->next)
    {
      /* See if only one case exists and if there is a constant value for
	 that case.  If so, we don't need a function.  */
      str = (char *) alloca (strlen (unit->name) + 10);
      sprintf (str, "*%s_cases", unit->name);
      attr = find_attr (str, 0);
      if (! attr) abort ();
      value = find_single_value (attr);
      if (value && GET_CODE (value) == CONST_STRING)
	{
	  sprintf (str, "*%s_case_%s", unit->name, XSTR (value, 0));
	  attr = find_attr (str, 0);
	  if (! attr) abort ();
	  value = find_single_value (attr);
	  if (value && GET_CODE (value) == CONST_STRING)
	    {
	      unit->needs_conflict_function = 0;
	      unit->default_cost = value;
	      continue;
	    }
	}

      /* The function first computes the case from the candidate insn.  */
      unit->needs_conflict_function = 1;
      unit->default_cost = make_numeric_value (0);

      printf ("static int\n");
      printf ("%s_unit_conflict_cost (executing_insn, candidate_insn)\n",
	      unit->name);
      printf ("     rtx executing_insn;\n");
      printf ("     rtx candidate_insn;\n");
      printf ("{\n");
      printf ("  rtx insn;\n");
      printf ("  int casenum;\n\n");
      printf ("  insn = candidate_insn;\n");
      printf ("  switch (recog_memoized (insn))\n");
      printf ("    {\n");

      /* Write the `switch' statement to get the case value.  */
      sprintf (str, "*%s_cases", unit->name);
      case_attr = find_attr (str, 0);
      if (! case_attr) abort ();
      common_av = find_most_used (case_attr);

      for (av = case_attr->first_value; av; av = av->next)
	if (av != common_av)
	  write_attr_case (case_attr, av, 1,
			   "casenum =", ";", 4, unit->condexp);

      write_attr_case (case_attr, common_av, 0,
		       "casenum =", ";", 4, unit->condexp);
      printf ("    }\n\n");

      /* Now write an outer switch statement on each case.  Then write
	 the tests on the executing function within each.  */
      printf ("  insn = executing_insn;\n");
      printf ("  switch (casenum)\n");
      printf ("    {\n");

      for (i = 0; i < unit->num_opclasses; i++)
	{
	  /* Ensure using this case.  */
	  using_case = 0;
	  for (av = case_attr->first_value; av; av = av->next)
	    if (av->num_insns
		&& contained_in_p (make_numeric_value (i), av->value))
	      using_case = 1;

	  if (! using_case)
	    continue;

	  printf ("    case %d:\n", i);
	  sprintf (str, "*%s_case_%d", unit->name, i);
	  attr = find_attr (str, 0);
	  if (! attr) abort ();

	  /* If single value, just write it.  */
	  value = find_single_value (attr);
	  if (value)
	    write_attr_set (attr, 6, value, "return", ";\n", true_rtx, -2);
	  else
	    {
	      common_av = find_most_used (attr);
	      printf ("      switch (recog_memoized (insn))\n");
	      printf ("\t{\n");

	      for (av = attr->first_value; av; av = av->next)
		if (av != common_av)
		  write_attr_case (attr, av, 1,
				   "return", ";", 8, unit->condexp);

	      write_attr_case (attr, common_av, 0,
			       "return", ";", 8, unit->condexp);
	      printf ("      }\n\n");
	    }
	}

      printf ("    }\n}\n\n");
    }

  /* Now that all functions have been written, write the table describing
     the function units.   The name is included for documenation purposes
     only.  */

  printf ("struct function_unit_desc function_units[] = {\n");

  for (unit = units; unit; unit = unit->next)
    {
      printf ("  {\"%s\", %d, %d, %d, %s, %s_unit_ready_cost, ",
	      unit->name, 1 << unit->num, unit->multiplicity,
	      unit->simultaneity, XSTR (unit->default_cost, 0), unit->name);

      if (unit->needs_conflict_function)
	printf ("%s_unit_conflict_cost", unit->name);
      else
	printf ("0");

      printf ("}, \n");
    }

  printf ("};\n\n");
}

/* This page contains miscellaneous utility routines.  */

/* Given a string, return the number of comma-separated elements in it.
   Return 0 for the null string.  */

static int
n_comma_elts (s)
     char *s;
{
  int n;

  if (*s == '\0')
    return 0;

  for (n = 1; *s; s++)
    if (*s == ',')
      n++;

  return n;
}

/* Given a pointer to a (char *), return a malloc'ed string containing the
   next comma-separated element.  Advance the pointer to after the string
   scanned, or the end-of-string.  Return NULL if at end of string.  */

static char *
next_comma_elt (pstr)
     char **pstr;
{
  char *out_str;
  char *p;

  if (**pstr == '\0')
    return NULL;

  /* Find end of string to compute length.  */
  for (p = *pstr; *p != ',' && *p != '\0'; p++)
    ;

  out_str = attr_string (*pstr, p - *pstr);
  *pstr = p;

  if (**pstr == ',')
    (*pstr)++;

  return out_str;
}

/* Return a `struct attr_desc' pointer for a given named attribute.  If CREATE
   is non-zero, build a new attribute, if one does not exist.  */

static struct attr_desc *
find_attr (name, create)
     char *name;
     int create;
{
  struct attr_desc *attr;
  char *new_name;

  /* Before we resort to using `strcmp', see if the string address matches
     anywhere.  In most cases, it should have been canonicalized to do so.  */
  if (name == alternative_name)
    return NULL;

  for (attr = attrs; attr; attr = attr->next)
    if (name == attr->name)
      return attr;

  /* Otherwise, do it the slow way.  */
  for (attr = attrs; attr; attr = attr->next)
    if (! strcmp (name, attr->name))
      return attr;

  if (! create)
    return NULL;

  new_name = (char *) xmalloc (strlen (name) + 1);
  strcpy (new_name, name);

  attr = (struct attr_desc *) xmalloc (sizeof (struct attr_desc));
  attr->name = new_name;
  attr->first_value = attr->default_val = NULL;
  attr->is_numeric = attr->is_const = attr->is_special = 0;
  attr->next = attrs;
  attrs = attr;

  return attr;
}

/* Create internal attribute with the given default value.  */

static void
make_internal_attr (name, value, special)
     char *name;
     rtx value;
     int special;
{
  struct attr_desc *attr;

  attr = find_attr (name, 1);
  if (attr->default_val)
    abort ();

  attr->is_numeric = 1;
  attr->is_const = 0;
  attr->is_special = special;
  attr->default_val = get_attr_value (value, attr, -2);
}

/* Find the most used value of an attribute.  */

static struct attr_value *
find_most_used (attr)
     struct attr_desc *attr;
{
  struct attr_value *av;
  struct attr_value *most_used;
  int nuses;

  most_used = NULL;
  nuses = -1;

  for (av = attr->first_value; av; av = av->next)
    if (av->num_insns > nuses)
      nuses = av->num_insns, most_used = av;

  return most_used;
}

/* If an attribute only has a single value used, return it.  Otherwise
   return NULL.  */

static rtx
find_single_value (attr)
     struct attr_desc *attr;
{
  struct attr_value *av;
  rtx unique_value;

  unique_value = NULL;
  for (av = attr->first_value; av; av = av->next)
    if (av->num_insns)
      {
	if (unique_value)
	  return NULL;
	else
	  unique_value = av->value;
      }

  return unique_value;
}

/* Return (attr_value "n") */

static rtx
make_numeric_value (n)
     int n;
{
  static rtx int_values[20];
  rtx exp;
  char *p;

  if (n < 0)
    abort ();

  if (n < 20 && int_values[n])
    return int_values[n];

  p = attr_printf ((n < 1000 ? 4 : HOST_BITS_PER_INT * 3 / 10 + 3), "%d", n);
  exp = attr_rtx (CONST_STRING, p);

  if (n < 20)
    int_values[n] = exp;

  return exp;
}

char *
xrealloc (ptr, size)
     char *ptr;
     unsigned size;
{
  char *result = (char *) realloc (ptr, size);
  if (!result)
    fatal ("virtual memory exhausted");
  return result;
}

char *
xmalloc (size)
     unsigned size;
{
  register char *val = (char *) malloc (size);

  if (val == 0)
    fatal ("virtual memory exhausted");
  return val;
}

static void
fatal (s, a1, a2)
     char *s;
{
  fprintf (stderr, "genattrtab: ");
  fprintf (stderr, s, a1, a2);
  fprintf (stderr, "\n");
  exit (FATAL_EXIT_CODE);
}

/* More 'friendly' abort that prints the line and file.
   config.h can #define abort fancy_abort if you like that sort of thing.  */

void
fancy_abort ()
{
  fatal ("Internal gcc abort.");
}

int
main (argc, argv)
     int argc;
     char **argv;
{
  rtx desc;
  FILE *infile;
  extern rtx read_rtx ();
  register int c;
  struct attr_desc *attr;
  struct attr_value *av;
  struct insn_def *id;
  rtx tem;

  obstack_init (rtl_obstack);

  if (argc <= 1)
    fatal ("No input file name.");

  infile = fopen (argv[1], "r");
  if (infile == 0)
    {
      perror (argv[1]);
      exit (FATAL_EXIT_CODE);
    }

  init_rtl ();

  /* Set up true and false rtx's */
  true_rtx = attr_rtx (CONST_INT, 1);
  false_rtx = attr_rtx (CONST_INT, 0);
  RTX_UNCHANGING_P (true_rtx) = RTX_UNCHANGING_P (false_rtx) = 1;

  printf ("/* Generated automatically by the program `genattrtab'\n\
from the machine description file `md'.  */\n\n");

  /* Read the machine description.  */

  while (1)
    {
      c = read_skip_spaces (infile);
      if (c == EOF)
	break;
      ungetc (c, infile);

      desc = read_rtx (infile);
      if (GET_CODE (desc) == DEFINE_INSN
	  || GET_CODE (desc) == DEFINE_PEEPHOLE
	  || GET_CODE (desc) == DEFINE_ASM_ATTRIBUTES)
	gen_insn (desc);

      else if (GET_CODE (desc) == DEFINE_EXPAND)
	insn_code_number++, insn_index_number++;

      else if (GET_CODE (desc) == DEFINE_SPLIT)
	insn_code_number++, insn_index_number++;

      else if (GET_CODE (desc) == DEFINE_ATTR)
	{
	  gen_attr (desc);
	  insn_index_number++;
	}

      else if (GET_CODE (desc) == DEFINE_DELAY)
	{
	  gen_delay (desc);
	  insn_index_number++;
	}

      else if (GET_CODE (desc) == DEFINE_FUNCTION_UNIT)
	{
	  gen_unit (desc);
	  insn_index_number++;
	}
    }

  /* If we didn't have a DEFINE_ASM_ATTRIBUTES, make a null one.  */
  if (! got_define_asm_attributes)
    {
      tem = rtx_alloc (DEFINE_ASM_ATTRIBUTES);
      XVEC (tem, 0) = rtvec_alloc (0);
      gen_insn (tem);
    }

  /* Expand DEFINE_DELAY information into new attribute.  */
  if (num_delays)
    expand_delays ();

  /* Expand DEFINE_FUNCTION_UNIT information into new attributes.  */
  if (num_units)
    expand_units ();

  printf ("#include \"config.h\"\n");
  printf ("#include \"rtl.h\"\n");
  printf ("#include \"insn-config.h\"\n");
  printf ("#include \"recog.h\"\n");
  printf ("#include \"regs.h\"\n");
  printf ("#include \"real.h\"\n");
  printf ("#include \"output.h\"\n");
  printf ("#include \"insn-attr.h\"\n");
  printf ("\n");  
  printf ("#define operands recog_operand\n\n");

  /* Make `insn_alternatives'.  */
  insn_alternatives = (int *) xmalloc (insn_code_number * sizeof (int));
  for (id = defs; id; id = id->next)
    if (id->insn_code >= 0)
      insn_alternatives[id->insn_code] = (1 << id->num_alternatives) - 1;

  /* Prepare to write out attribute subroutines by checking everything stored
     away and building the attribute cases.  */

  check_defs ();
  for (attr = attrs; attr; attr = attr->next)
    {
      check_attr_value (attr->default_val->value, attr);
      fill_attr (attr);
    }

  /* Construct extra attributes for `length'.  */
  make_length_attrs ();

  /* Perform any possible optimizations to speed up compilation. */
  optimize_attrs ();

  /* Now write out all the `gen_attr_...' routines.  Do these before the
     special routines (specifically before write_function_unit_info), so
     that they get defined before they are used.  */

  for (attr = attrs; attr; attr = attr->next)
    {
      if (! attr->is_special)
	write_attr_get (attr);
    }

  /* Write out delay eligibility information, if DEFINE_DELAY present.
     (The function to compute the number of delay slots will be written
     below.)  */
  if (num_delays)
    {
      write_eligible_delay ("delay");
      if (have_annul_true)
	write_eligible_delay ("annul_true");
      if (have_annul_false)
	write_eligible_delay ("annul_false");
    }

  /* Write out information about function units.  */
  if (num_units)
    write_function_unit_info ();

  fflush (stdout);
  exit (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
  /* NOTREACHED */
  return 0;
}
