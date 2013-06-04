/* Support routines for the various generation passes.
   Copyright (C) 2000-2013 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "bconfig.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "obstack.h"
#include "errors.h"
#include "hashtab.h"
#include "read-md.h"
#include "gensupport.h"

#define MAX_OPERANDS 40

static rtx operand_data[MAX_OPERANDS];
static rtx match_operand_entries_in_pattern[MAX_OPERANDS];
static char used_operands_numbers[MAX_OPERANDS];


/* In case some macros used by files we include need it, define this here.  */
int target_flags;

int insn_elision = 1;

static struct obstack obstack;
struct obstack *rtl_obstack = &obstack;

/* Counter for patterns that generate code: define_insn, define_expand,
   define_split, define_peephole, and define_peephole2.  See read_md_rtx().
   Any define_insn_and_splits are already in separate queues so that the
   insn and the splitter get a unique number also.  */
static int sequence_num;

static int predicable_default;
static const char *predicable_true;
static const char *predicable_false;

static const char *subst_true = "yes";
static const char *subst_false = "no";

static htab_t condition_table;

/* We initially queue all patterns, process the define_insn,
   define_cond_exec and define_subst patterns, then return
   them one at a time.  */

struct queue_elem
{
  rtx data;
  const char *filename;
  int lineno;
  struct queue_elem *next;
  /* In a DEFINE_INSN that came from a DEFINE_INSN_AND_SPLIT, SPLIT
     points to the generated DEFINE_SPLIT.  */
  struct queue_elem *split;
};

#define MNEMONIC_ATTR_NAME "mnemonic"
#define MNEMONIC_HTAB_SIZE 1024

static struct queue_elem *define_attr_queue;
static struct queue_elem **define_attr_tail = &define_attr_queue;
static struct queue_elem *define_pred_queue;
static struct queue_elem **define_pred_tail = &define_pred_queue;
static struct queue_elem *define_insn_queue;
static struct queue_elem **define_insn_tail = &define_insn_queue;
static struct queue_elem *define_cond_exec_queue;
static struct queue_elem **define_cond_exec_tail = &define_cond_exec_queue;
static struct queue_elem *define_subst_queue;
static struct queue_elem **define_subst_tail = &define_subst_queue;
static struct queue_elem *other_queue;
static struct queue_elem **other_tail = &other_queue;
static struct queue_elem *define_subst_attr_queue;
static struct queue_elem **define_subst_attr_tail = &define_subst_attr_queue;

static struct queue_elem *queue_pattern (rtx, struct queue_elem ***,
					 const char *, int);

static void remove_constraints (rtx);
static void process_rtx (rtx, int);

static int is_predicable (struct queue_elem *);
static void identify_predicable_attribute (void);
static int n_alternatives (const char *);
static void collect_insn_data (rtx, int *, int *);
static rtx alter_predicate_for_insn (rtx, int, int, int);
static const char *alter_test_for_insn (struct queue_elem *,
					struct queue_elem *);
static char *shift_output_template (char *, const char *, int);
static const char *alter_output_for_insn (struct queue_elem *,
					  struct queue_elem *,
					  int, int);
static void process_one_cond_exec (struct queue_elem *);
static void process_define_cond_exec (void);
static void init_predicate_table (void);
static void record_insn_name (int, const char *);

static bool has_subst_attribute (struct queue_elem *, struct queue_elem *);
static bool subst_pattern_match (rtx, rtx, int);
static int get_alternatives_number (rtx, int *, int);
static const char * alter_output_for_subst_insn (rtx, int);
static void alter_attrs_for_subst_insn (struct queue_elem *, int);
static void process_substs_on_one_elem (struct queue_elem *,
					struct queue_elem *);
static rtx subst_dup (rtx, int, int);
static void process_define_subst (void);

static const char * duplicate_alternatives (const char *, int);
static const char * duplicate_each_alternative (const char * str, int n_dup);

typedef const char * (*constraints_handler_t) (const char *, int);
static rtx alter_constraints (rtx, int, constraints_handler_t);
static rtx adjust_operands_numbers (rtx);
static rtx replace_duplicating_operands_in_pattern (rtx);

/* Make a version of gen_rtx_CONST_INT so that GEN_INT can be used in
   the gensupport programs.  */

rtx
gen_rtx_CONST_INT (enum machine_mode ARG_UNUSED (mode),
		   HOST_WIDE_INT arg)
{
  rtx rt = rtx_alloc (CONST_INT);

  XWINT (rt, 0) = arg;
  return rt;
}

/* Predicate handling.

   We construct from the machine description a table mapping each
   predicate to a list of the rtl codes it can possibly match.  The
   function 'maybe_both_true' uses it to deduce that there are no
   expressions that can be matches by certain pairs of tree nodes.
   Also, if a predicate can match only one code, we can hardwire that
   code into the node testing the predicate.

   Some predicates are flagged as special.  validate_pattern will not
   warn about modeless match_operand expressions if they have a
   special predicate.  Predicates that allow only constants are also
   treated as special, for this purpose.

   validate_pattern will warn about predicates that allow non-lvalues
   when they appear in destination operands.

   Calculating the set of rtx codes that can possibly be accepted by a
   predicate expression EXP requires a three-state logic: any given
   subexpression may definitively accept a code C (Y), definitively
   reject a code C (N), or may have an indeterminate effect (I).  N
   and I is N; Y or I is Y; Y and I, N or I are both I.  Here are full
   truth tables.

     a b  a&b  a|b
     Y Y   Y    Y
     N Y   N    Y
     N N   N    N
     I Y   I    Y
     I N   N    I
     I I   I    I

   We represent Y with 1, N with 0, I with 2.  If any code is left in
   an I state by the complete expression, we must assume that that
   code can be accepted.  */

#define N 0
#define Y 1
#define I 2

#define TRISTATE_AND(a,b)			\
  ((a) == I ? ((b) == N ? N : I) :		\
   (b) == I ? ((a) == N ? N : I) :		\
   (a) && (b))

#define TRISTATE_OR(a,b)			\
  ((a) == I ? ((b) == Y ? Y : I) :		\
   (b) == I ? ((a) == Y ? Y : I) :		\
   (a) || (b))

#define TRISTATE_NOT(a)				\
  ((a) == I ? I : !(a))

/* 0 means no warning about that code yet, 1 means warned.  */
static char did_you_mean_codes[NUM_RTX_CODE];

/* Recursively calculate the set of rtx codes accepted by the
   predicate expression EXP, writing the result to CODES.  LINENO is
   the line number on which the directive containing EXP appeared.  */

static void
compute_predicate_codes (rtx exp, int lineno, char codes[NUM_RTX_CODE])
{
  char op0_codes[NUM_RTX_CODE];
  char op1_codes[NUM_RTX_CODE];
  char op2_codes[NUM_RTX_CODE];
  int i;

  switch (GET_CODE (exp))
    {
    case AND:
      compute_predicate_codes (XEXP (exp, 0), lineno, op0_codes);
      compute_predicate_codes (XEXP (exp, 1), lineno, op1_codes);
      for (i = 0; i < NUM_RTX_CODE; i++)
	codes[i] = TRISTATE_AND (op0_codes[i], op1_codes[i]);
      break;

    case IOR:
      compute_predicate_codes (XEXP (exp, 0), lineno, op0_codes);
      compute_predicate_codes (XEXP (exp, 1), lineno, op1_codes);
      for (i = 0; i < NUM_RTX_CODE; i++)
	codes[i] = TRISTATE_OR (op0_codes[i], op1_codes[i]);
      break;
    case NOT:
      compute_predicate_codes (XEXP (exp, 0), lineno, op0_codes);
      for (i = 0; i < NUM_RTX_CODE; i++)
	codes[i] = TRISTATE_NOT (op0_codes[i]);
      break;

    case IF_THEN_ELSE:
      /* a ? b : c  accepts the same codes as (a & b) | (!a & c).  */
      compute_predicate_codes (XEXP (exp, 0), lineno, op0_codes);
      compute_predicate_codes (XEXP (exp, 1), lineno, op1_codes);
      compute_predicate_codes (XEXP (exp, 2), lineno, op2_codes);
      for (i = 0; i < NUM_RTX_CODE; i++)
	codes[i] = TRISTATE_OR (TRISTATE_AND (op0_codes[i], op1_codes[i]),
				TRISTATE_AND (TRISTATE_NOT (op0_codes[i]),
					      op2_codes[i]));
      break;

    case MATCH_CODE:
      /* MATCH_CODE allows a specified list of codes.  However, if it
	 does not apply to the top level of the expression, it does not
	 constrain the set of codes for the top level.  */
      if (XSTR (exp, 1)[0] != '\0')
	{
	  memset (codes, Y, NUM_RTX_CODE);
	  break;
	}

      memset (codes, N, NUM_RTX_CODE);
      {
	const char *next_code = XSTR (exp, 0);
	const char *code;

	if (*next_code == '\0')
	  {
	    error_with_line (lineno, "empty match_code expression");
	    break;
	  }

	while ((code = scan_comma_elt (&next_code)) != 0)
	  {
	    size_t n = next_code - code;
	    int found_it = 0;

	    for (i = 0; i < NUM_RTX_CODE; i++)
	      if (!strncmp (code, GET_RTX_NAME (i), n)
		  && GET_RTX_NAME (i)[n] == '\0')
		{
		  codes[i] = Y;
		  found_it = 1;
		  break;
		}
	    if (!found_it)
	      {
		error_with_line (lineno,
				 "match_code \"%.*s\" matches nothing",
				 (int) n, code);
		for (i = 0; i < NUM_RTX_CODE; i++)
		  if (!strncasecmp (code, GET_RTX_NAME (i), n)
		      && GET_RTX_NAME (i)[n] == '\0'
		      && !did_you_mean_codes[i])
		    {
		      did_you_mean_codes[i] = 1;
		      message_with_line (lineno, "(did you mean \"%s\"?)",
					 GET_RTX_NAME (i));
		    }
	      }
	  }
      }
      break;

    case MATCH_OPERAND:
      /* MATCH_OPERAND disallows the set of codes that the named predicate
	 disallows, and is indeterminate for the codes that it does allow.  */
      {
	struct pred_data *p = lookup_predicate (XSTR (exp, 1));
	if (!p)
	  {
	    error_with_line (lineno, "reference to unknown predicate '%s'",
			     XSTR (exp, 1));
	    break;
	  }
	for (i = 0; i < NUM_RTX_CODE; i++)
	  codes[i] = p->codes[i] ? I : N;
      }
      break;


    case MATCH_TEST:
      /* (match_test WHATEVER) is completely indeterminate.  */
      memset (codes, I, NUM_RTX_CODE);
      break;

    default:
      error_with_line (lineno,
		       "'%s' cannot be used in a define_predicate expression",
		       GET_RTX_NAME (GET_CODE (exp)));
      memset (codes, I, NUM_RTX_CODE);
      break;
    }
}

#undef TRISTATE_OR
#undef TRISTATE_AND
#undef TRISTATE_NOT

/* Return true if NAME is a valid predicate name.  */

static bool
valid_predicate_name_p (const char *name)
{
  const char *p;

  if (!ISALPHA (name[0]) && name[0] != '_')
    return false;
  for (p = name + 1; *p; p++)
    if (!ISALNUM (*p) && *p != '_')
      return false;
  return true;
}

/* Process define_predicate directive DESC, which appears on line number
   LINENO.  Compute the set of codes that can be matched, and record this
   as a known predicate.  */

static void
process_define_predicate (rtx desc, int lineno)
{
  struct pred_data *pred;
  char codes[NUM_RTX_CODE];
  int i;

  if (!valid_predicate_name_p (XSTR (desc, 0)))
    {
      error_with_line (lineno,
		       "%s: predicate name must be a valid C function name",
		       XSTR (desc, 0));
      return;
    }

  pred = XCNEW (struct pred_data);
  pred->name = XSTR (desc, 0);
  pred->exp = XEXP (desc, 1);
  pred->c_block = XSTR (desc, 2);
  if (GET_CODE (desc) == DEFINE_SPECIAL_PREDICATE)
    pred->special = true;

  compute_predicate_codes (XEXP (desc, 1), lineno, codes);

  for (i = 0; i < NUM_RTX_CODE; i++)
    if (codes[i] != N)
      add_predicate_code (pred, (enum rtx_code) i);

  add_predicate (pred);
}
#undef I
#undef N
#undef Y

/* Queue PATTERN on LIST_TAIL.  Return the address of the new queue
   element.  */

static struct queue_elem *
queue_pattern (rtx pattern, struct queue_elem ***list_tail,
	       const char *filename, int lineno)
{
  struct queue_elem *e = XNEW(struct queue_elem);
  e->data = pattern;
  e->filename = filename;
  e->lineno = lineno;
  e->next = NULL;
  e->split = NULL;
  **list_tail = e;
  *list_tail = &e->next;
  return e;
}

/* Remove element ELEM from QUEUE.  */
static void
remove_from_queue (struct queue_elem *elem, struct queue_elem **queue)
{
  struct queue_elem *prev, *e;
  prev = NULL;
  for (e = *queue; e ; e = e->next)
    {
      if (e == elem)
	break;
      prev = e;
    }
  if (e == NULL)
    return;

  if (prev)
    prev->next = elem->next;
  else
    *queue = elem->next;
}

/* Build a define_attr for an binary attribute with name NAME and
   possible values "yes" and "no", and queue it.  */
static void
add_define_attr (const char *name)
{
  struct queue_elem *e = XNEW(struct queue_elem);
  rtx t1 = rtx_alloc (DEFINE_ATTR);
  XSTR (t1, 0) = name;
  XSTR (t1, 1) = "no,yes";
  XEXP (t1, 2) = rtx_alloc (CONST_STRING);
  XSTR (XEXP (t1, 2), 0) = "yes";
  e->data = t1;
  e->filename = "built-in";
  e->lineno = -1;
  e->next = define_attr_queue;
  define_attr_queue = e;

}

/* Recursively remove constraints from an rtx.  */

static void
remove_constraints (rtx part)
{
  int i, j;
  const char *format_ptr;

  if (part == 0)
    return;

  if (GET_CODE (part) == MATCH_OPERAND)
    XSTR (part, 2) = "";
  else if (GET_CODE (part) == MATCH_SCRATCH)
    XSTR (part, 1) = "";

  format_ptr = GET_RTX_FORMAT (GET_CODE (part));

  for (i = 0; i < GET_RTX_LENGTH (GET_CODE (part)); i++)
    switch (*format_ptr++)
      {
      case 'e':
      case 'u':
	remove_constraints (XEXP (part, i));
	break;
      case 'E':
	if (XVEC (part, i) != NULL)
	  for (j = 0; j < XVECLEN (part, i); j++)
	    remove_constraints (XVECEXP (part, i, j));
	break;
      }
}

/* Process a top level rtx in some way, queuing as appropriate.  */

static void
process_rtx (rtx desc, int lineno)
{
  switch (GET_CODE (desc))
    {
    case DEFINE_INSN:
      queue_pattern (desc, &define_insn_tail, read_md_filename, lineno);
      break;

    case DEFINE_COND_EXEC:
      queue_pattern (desc, &define_cond_exec_tail, read_md_filename, lineno);
      break;

    case DEFINE_SUBST:
      queue_pattern (desc, &define_subst_tail, read_md_filename, lineno);
      break;

    case DEFINE_SUBST_ATTR:
      queue_pattern (desc, &define_subst_attr_tail, read_md_filename, lineno);
      break;

    case DEFINE_ATTR:
    case DEFINE_ENUM_ATTR:
      queue_pattern (desc, &define_attr_tail, read_md_filename, lineno);
      break;

    case DEFINE_PREDICATE:
    case DEFINE_SPECIAL_PREDICATE:
      process_define_predicate (desc, lineno);
      /* Fall through.  */

    case DEFINE_CONSTRAINT:
    case DEFINE_REGISTER_CONSTRAINT:
    case DEFINE_MEMORY_CONSTRAINT:
    case DEFINE_ADDRESS_CONSTRAINT:
      queue_pattern (desc, &define_pred_tail, read_md_filename, lineno);
      break;

    case DEFINE_INSN_AND_SPLIT:
      {
	const char *split_cond;
	rtx split;
	rtvec attr;
	int i;
	struct queue_elem *insn_elem;
	struct queue_elem *split_elem;

	/* Create a split with values from the insn_and_split.  */
	split = rtx_alloc (DEFINE_SPLIT);

	i = XVECLEN (desc, 1);
	XVEC (split, 0) = rtvec_alloc (i);
	while (--i >= 0)
	  {
	    XVECEXP (split, 0, i) = copy_rtx (XVECEXP (desc, 1, i));
	    remove_constraints (XVECEXP (split, 0, i));
	  }

	/* If the split condition starts with "&&", append it to the
	   insn condition to create the new split condition.  */
	split_cond = XSTR (desc, 4);
	if (split_cond[0] == '&' && split_cond[1] == '&')
	  {
	    copy_md_ptr_loc (split_cond + 2, split_cond);
	    split_cond = join_c_conditions (XSTR (desc, 2), split_cond + 2);
	  }
	XSTR (split, 1) = split_cond;
	XVEC (split, 2) = XVEC (desc, 5);
	XSTR (split, 3) = XSTR (desc, 6);

	/* Fix up the DEFINE_INSN.  */
	attr = XVEC (desc, 7);
	PUT_CODE (desc, DEFINE_INSN);
	XVEC (desc, 4) = attr;

	/* Queue them.  */
	insn_elem
	  = queue_pattern (desc, &define_insn_tail, read_md_filename,
			   lineno);
	split_elem
	  = queue_pattern (split, &other_tail, read_md_filename, lineno);
	insn_elem->split = split_elem;
	break;
      }

    default:
      queue_pattern (desc, &other_tail, read_md_filename, lineno);
      break;
    }
}

/* Return true if attribute PREDICABLE is true for ELEM, which holds
   a DEFINE_INSN.  */

static int
is_predicable (struct queue_elem *elem)
{
  rtvec vec = XVEC (elem->data, 4);
  const char *value;
  int i;

  if (! vec)
    return predicable_default;

  for (i = GET_NUM_ELEM (vec) - 1; i >= 0; --i)
    {
      rtx sub = RTVEC_ELT (vec, i);
      switch (GET_CODE (sub))
	{
	case SET_ATTR:
	  if (strcmp (XSTR (sub, 0), "predicable") == 0)
	    {
	      value = XSTR (sub, 1);
	      goto found;
	    }
	  break;

	case SET_ATTR_ALTERNATIVE:
	  if (strcmp (XSTR (sub, 0), "predicable") == 0)
	    {
	      error_with_line (elem->lineno,
			       "multiple alternatives for `predicable'");
	      return 0;
	    }
	  break;

	case SET:
	  if (GET_CODE (SET_DEST (sub)) != ATTR
	      || strcmp (XSTR (SET_DEST (sub), 0), "predicable") != 0)
	    break;
	  sub = SET_SRC (sub);
	  if (GET_CODE (sub) == CONST_STRING)
	    {
	      value = XSTR (sub, 0);
	      goto found;
	    }

	  /* ??? It would be possible to handle this if we really tried.
	     It's not easy though, and I'm not going to bother until it
	     really proves necessary.  */
	  error_with_line (elem->lineno,
			   "non-constant value for `predicable'");
	  return 0;

	default:
	  gcc_unreachable ();
	}
    }

  return predicable_default;

 found:
  /* Find out which value we're looking at.  Multiple alternatives means at
     least one is predicable.  */
  if (strchr (value, ',') != NULL)
    return 1;
  if (strcmp (value, predicable_true) == 0)
    return 1;
  if (strcmp (value, predicable_false) == 0)
    return 0;

  error_with_line (elem->lineno,
		   "unknown value `%s' for `predicable' attribute", value);
  return 0;
}

/* Find attribute SUBST in ELEM and assign NEW_VALUE to it.  */
static void
change_subst_attribute (struct queue_elem *elem,
			struct queue_elem *subst_elem,
			const char *new_value)
{
  rtvec attrs_vec = XVEC (elem->data, 4);
  const char *subst_name = XSTR (subst_elem->data, 0);
  int i;

  if (! attrs_vec)
    return;

  for (i = GET_NUM_ELEM (attrs_vec) - 1; i >= 0; --i)
    {
      rtx cur_attr = RTVEC_ELT (attrs_vec, i);
      if (GET_CODE (cur_attr) != SET_ATTR)
	continue;
      if (strcmp (XSTR (cur_attr, 0), subst_name) == 0)
	{
	  XSTR (cur_attr, 1) = new_value;
	  return;
	}
    }
}

/* Return true if ELEM has the attribute with the name of DEFINE_SUBST
   represented by SUBST_ELEM and this attribute has value SUBST_TRUE.
   DEFINE_SUBST isn't applied to patterns without such attribute.  In other
   words, we suppose the default value of the attribute to be 'no' since it is
   always generated automaticaly in read-rtl.c.  */
static bool
has_subst_attribute (struct queue_elem *elem, struct queue_elem *subst_elem)
{
  rtvec attrs_vec = XVEC (elem->data, 4);
  const char *value, *subst_name = XSTR (subst_elem->data, 0);
  int i;

  if (! attrs_vec)
    return false;

  for (i = GET_NUM_ELEM (attrs_vec) - 1; i >= 0; --i)
    {
      rtx cur_attr = RTVEC_ELT (attrs_vec, i);
      switch (GET_CODE (cur_attr))
	{
	case SET_ATTR:
	  if (strcmp (XSTR (cur_attr, 0), subst_name) == 0)
	    {
	      value = XSTR (cur_attr, 1);
	      goto found;
	    }
	  break;

	case SET:
	  if (GET_CODE (SET_DEST (cur_attr)) != ATTR
	      || strcmp (XSTR (SET_DEST (cur_attr), 0), subst_name) != 0)
	    break;
	  cur_attr = SET_SRC (cur_attr);
	  if (GET_CODE (cur_attr) == CONST_STRING)
	    {
	      value = XSTR (cur_attr, 0);
	      goto found;
	    }

	  /* Only (set_attr "subst" "yes/no") and
		  (set (attr "subst" (const_string "yes/no")))
	     are currently allowed.  */
	  error_with_line (elem->lineno,
			   "unsupported value for `%s'", subst_name);
	  return false;

	case SET_ATTR_ALTERNATIVE:
	  error_with_line (elem->lineno,
			   "%s: `set_attr_alternative' is unsupported by "
			   "`define_subst'",
			   XSTR (elem->data, 0));
	  return false;


	default:
	  gcc_unreachable ();
	}
    }

  return false;

 found:
  if (strcmp (value, subst_true) == 0)
    return true;
  if (strcmp (value, subst_false) == 0)
    return false;

  error_with_line (elem->lineno,
		   "unknown value `%s' for `%s' attribute", value, subst_name);
  return false;
}

/* Compare RTL-template of original define_insn X to input RTL-template of
   define_subst PT.  Return 1 if the templates match, 0 otherwise.
   During the comparison, the routine also fills global_array OPERAND_DATA.  */
static bool
subst_pattern_match (rtx x, rtx pt, int lineno)
{
  RTX_CODE code, code_pt;
  int i, j, len;
  const char *fmt, *pred_name;

  code = GET_CODE (x);
  code_pt = GET_CODE (pt);

  if (code_pt == MATCH_OPERAND)
    {
      /* MATCH_DUP, and MATCH_OP_DUP don't have a specified mode, so we
	 always accept them.  */
      if (GET_MODE (pt) != VOIDmode && GET_MODE (x) != GET_MODE (pt)
	  && (code != MATCH_DUP && code != MATCH_OP_DUP))
	return false; /* Modes don't match.  */

      if (code == MATCH_OPERAND)
	{
	  pred_name = XSTR (pt, 1);
	  if (pred_name[0] != 0)
	    {
	      const struct pred_data *pred_pt = lookup_predicate (pred_name);
	      if (!pred_pt || pred_pt != lookup_predicate (XSTR (x, 1)))
		return false; /* Predicates don't match.  */
	    }
	}

      gcc_assert (XINT (pt, 0) >= 0 && XINT (pt, 0) < MAX_OPERANDS);
      operand_data[XINT (pt, 0)] = x;
      return true;
    }

  if (code_pt == MATCH_OPERATOR)
    {
      int x_vecexp_pos = -1;

      /* Compare modes.  */
      if (GET_MODE (pt) != VOIDmode && GET_MODE (x) != GET_MODE (pt))
	return false;

      /* In case X is also match_operator, compare predicates.  */
      if (code == MATCH_OPERATOR)
	{
	  pred_name = XSTR (pt, 1);
	  if (pred_name[0] != 0)
	    {
	      const struct pred_data *pred_pt = lookup_predicate (pred_name);
	      if (!pred_pt || pred_pt != lookup_predicate (XSTR (x, 1)))
		return false;
	    }
	}

      /* Compare operands.
	 MATCH_OPERATOR in input template could match in original template
	 either 1) MATCH_OPERAND, 2) UNSPEC, 3) ordinary operation (like PLUS).
	 In the first case operands are at (XVECEXP (x, 2, j)), in the second
	 - at (XVECEXP (x, 0, j)), in the last one - (XEXP (x, j)).
	 X_VECEXP_POS variable shows, where to look for these operands.  */
      if (code == UNSPEC
	  || code == UNSPEC_VOLATILE)
	x_vecexp_pos = 0;
      else if (code == MATCH_OPERATOR)
	x_vecexp_pos = 2;
      else
	x_vecexp_pos = -1;

      /* MATCH_OPERATOR or UNSPEC case.  */
      if (x_vecexp_pos >= 0)
	{
	  /* Compare operands number in X and PT.  */
	  if (XVECLEN (x, x_vecexp_pos) != XVECLEN (pt, 2))
	    return false;
	  for (j = 0; j < XVECLEN (pt, 2); j++)
	    if (!subst_pattern_match (XVECEXP (x, x_vecexp_pos, j),
				      XVECEXP (pt, 2, j), lineno))
	      return false;
	}

      /* Ordinary operator.  */
      else
	{
	  /* Compare operands number in X and PT.
	     We count operands differently for X and PT since we compare
	     an operator (with operands directly in RTX) and MATCH_OPERATOR
	     (that has a vector with operands).  */
	  if (GET_RTX_LENGTH (code) != XVECLEN (pt, 2))
	    return false;
	  for (j = 0; j < XVECLEN (pt, 2); j++)
	    if (!subst_pattern_match (XEXP (x, j), XVECEXP (pt, 2, j), lineno))
	      return false;
	}

      /* Store the operand to OPERAND_DATA array.  */
      gcc_assert (XINT (pt, 0) >= 0 && XINT (pt, 0) < MAX_OPERANDS);
      operand_data[XINT (pt, 0)] = x;
      return true;
    }

  if (code_pt == MATCH_PAR_DUP
      || code_pt == MATCH_DUP
      || code_pt == MATCH_OP_DUP
      || code_pt == MATCH_SCRATCH
      || code_pt == MATCH_PARALLEL)
    {
      /* Currently interface for these constructions isn't defined -
	 probably they aren't needed in input template of define_subst at all.
	 So, for now their usage in define_subst is forbidden.  */
      error_with_line (lineno, "%s cannot be used in define_subst",
		       GET_RTX_NAME (code_pt));
    }

  gcc_assert (code != MATCH_PAR_DUP
      && code_pt != MATCH_DUP
      && code_pt != MATCH_OP_DUP
      && code_pt != MATCH_SCRATCH
      && code_pt != MATCH_PARALLEL
      && code_pt != MATCH_OPERAND
      && code_pt != MATCH_OPERATOR);
  /* If PT is none of the handled above, then we match only expressions with
     the same code in X.  */
  if (code != code_pt)
    return false;

  fmt = GET_RTX_FORMAT (code_pt);
  len = GET_RTX_LENGTH (code_pt);

  for (i = 0; i < len; i++)
    {
      if (fmt[i] == '0')
	break;

      switch (fmt[i])
	{
	case 'i': case 'w': case 's':
	  continue;

	case 'e': case 'u':
	  if (!subst_pattern_match (XEXP (x, i), XEXP (pt, i), lineno))
	    return false;
	  break;
	case 'E':
	  {
	    if (XVECLEN (x, i) != XVECLEN (pt, i))
	      return false;
	    for (j = 0; j < XVECLEN (pt, i); j++)
	      if (!subst_pattern_match (XVECEXP (x, i, j), XVECEXP (pt, i, j),
					lineno))
		return false;
	    break;
	  }
	default:
	  gcc_unreachable ();
	}
    }

  return true;
}

/* Examine the attribute "predicable"; discover its boolean values
   and its default.  */

static void
identify_predicable_attribute (void)
{
  struct queue_elem *elem;
  char *p_true, *p_false;
  const char *value;

  /* Look for the DEFINE_ATTR for `predicable', which must exist.  */
  for (elem = define_attr_queue; elem ; elem = elem->next)
    if (strcmp (XSTR (elem->data, 0), "predicable") == 0)
      goto found;

  error_with_line (define_cond_exec_queue->lineno,
		   "attribute `predicable' not defined");
  return;

 found:
  value = XSTR (elem->data, 1);
  p_false = xstrdup (value);
  p_true = strchr (p_false, ',');
  if (p_true == NULL || strchr (++p_true, ',') != NULL)
    {
      error_with_line (elem->lineno, "attribute `predicable' is not a boolean");
      free (p_false);
      return;
    }
  p_true[-1] = '\0';

  predicable_true = p_true;
  predicable_false = p_false;

  switch (GET_CODE (XEXP (elem->data, 2)))
    {
    case CONST_STRING:
      value = XSTR (XEXP (elem->data, 2), 0);
      break;

    case CONST:
      error_with_line (elem->lineno, "attribute `predicable' cannot be const");
      free (p_false);
      return;

    default:
      error_with_line (elem->lineno,
		       "attribute `predicable' must have a constant default");
      free (p_false);
      return;
    }

  if (strcmp (value, p_true) == 0)
    predicable_default = 1;
  else if (strcmp (value, p_false) == 0)
    predicable_default = 0;
  else
    {
      error_with_line (elem->lineno,
		       "unknown value `%s' for `predicable' attribute", value);
      free (p_false);
    }
}

/* Return the number of alternatives in constraint S.  */

static int
n_alternatives (const char *s)
{
  int n = 1;

  if (s)
    while (*s)
      n += (*s++ == ',');

  return n;
}

/* The routine scans rtl PATTERN, find match_operand in it and counts
   number of alternatives.  If PATTERN contains several match_operands
   with different number of alternatives, error is emitted, and the
   routine returns 0.  If all match_operands in PATTERN have the same
   number of alternatives, it's stored in N_ALT, and the routine returns 1.
   Argument LINENO is used in when the error is emitted.  */
static int
get_alternatives_number (rtx pattern, int *n_alt, int lineno)
{
  const char *fmt;
  enum rtx_code code;
  int i, j, len;

  if (!n_alt)
    return 0;

  code = GET_CODE (pattern);
  switch (code)
    {
    case MATCH_OPERAND:
      i = n_alternatives (XSTR (pattern, 2));
      /* n_alternatives returns 1 if constraint string is empty -
	 here we fix it up.  */
      if (!*(XSTR (pattern, 2)))
	i = 0;
      if (*n_alt <= 0)
	*n_alt = i;

      else if (i && i != *n_alt)
	{
	  error_with_line (lineno,
			   "wrong number of alternatives in operand %d",
			   XINT (pattern, 0));
	  return 0;
	}

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);
  for (i = 0; i < len; i++)
    {
      switch (fmt[i])
	{
	case 'e': case 'u':
	  if (!get_alternatives_number (XEXP (pattern, i), n_alt, lineno))
		return 0;
	  break;

	case 'V':
	  if (XVEC (pattern, i) == NULL)
	    break;

	case 'E':
	  for (j = XVECLEN (pattern, i) - 1; j >= 0; --j)
	    if (!get_alternatives_number (XVECEXP (pattern, i, j),
					  n_alt, lineno))
		return 0;
	  break;

	case 'i': case 'w': case '0': case 's': case 'S': case 'T':
	  break;

	default:
	  gcc_unreachable ();
	}
    }
    return 1;
}

/* Determine how many alternatives there are in INSN, and how many
   operands.  */

static void
collect_insn_data (rtx pattern, int *palt, int *pmax)
{
  const char *fmt;
  enum rtx_code code;
  int i, j, len;

  code = GET_CODE (pattern);
  switch (code)
    {
    case MATCH_OPERAND:
      i = n_alternatives (XSTR (pattern, 2));
      *palt = (i > *palt ? i : *palt);
      /* Fall through.  */

    case MATCH_OPERATOR:
    case MATCH_SCRATCH:
    case MATCH_PARALLEL:
      i = XINT (pattern, 0);
      if (i > *pmax)
	*pmax = i;
      break;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);
  for (i = 0; i < len; i++)
    {
      switch (fmt[i])
	{
	case 'e': case 'u':
	  collect_insn_data (XEXP (pattern, i), palt, pmax);
	  break;

	case 'V':
	  if (XVEC (pattern, i) == NULL)
	    break;
	  /* Fall through.  */
	case 'E':
	  for (j = XVECLEN (pattern, i) - 1; j >= 0; --j)
	    collect_insn_data (XVECEXP (pattern, i, j), palt, pmax);
	  break;

	case 'i': case 'w': case '0': case 's': case 'S': case 'T':
	  break;

	default:
	  gcc_unreachable ();
	}
    }
}

static rtx
alter_predicate_for_insn (rtx pattern, int alt, int max_op, int lineno)
{
  const char *fmt;
  enum rtx_code code;
  int i, j, len;

  code = GET_CODE (pattern);
  switch (code)
    {
    case MATCH_OPERAND:
      {
	const char *c = XSTR (pattern, 2);

	if (n_alternatives (c) != 1)
	  {
	    error_with_line (lineno, "too many alternatives for operand %d",
			     XINT (pattern, 0));
	    return NULL;
	  }

	/* Replicate C as needed to fill out ALT alternatives.  */
	if (c && *c && alt > 1)
	  {
	    size_t c_len = strlen (c);
	    size_t len = alt * (c_len + 1);
	    char *new_c = XNEWVEC (char, len);

	    memcpy (new_c, c, c_len);
	    for (i = 1; i < alt; ++i)
	      {
		new_c[i * (c_len + 1) - 1] = ',';
		memcpy (&new_c[i * (c_len + 1)], c, c_len);
	      }
	    new_c[len - 1] = '\0';
	    XSTR (pattern, 2) = new_c;
	  }
      }
      /* Fall through.  */

    case MATCH_OPERATOR:
    case MATCH_SCRATCH:
    case MATCH_PARALLEL:
      XINT (pattern, 0) += max_op;
      break;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);
  for (i = 0; i < len; i++)
    {
      rtx r;

      switch (fmt[i])
	{
	case 'e': case 'u':
	  r = alter_predicate_for_insn (XEXP (pattern, i), alt,
					max_op, lineno);
	  if (r == NULL)
	    return r;
	  break;

	case 'E':
	  for (j = XVECLEN (pattern, i) - 1; j >= 0; --j)
	    {
	      r = alter_predicate_for_insn (XVECEXP (pattern, i, j),
					    alt, max_op, lineno);
	      if (r == NULL)
		return r;
	    }
	  break;

	case 'i': case 'w': case '0': case 's':
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  return pattern;
}

/* Duplicate constraints in PATTERN.  If pattern is from original
   rtl-template, we need to duplicate each alternative - for that we
   need to use duplicate_each_alternative () as a functor ALTER.
   If pattern is from output-pattern of define_subst, we need to
   duplicate constraints in another way - with duplicate_alternatives ().
   N_DUP is multiplication factor.  */
static rtx
alter_constraints (rtx pattern, int n_dup, constraints_handler_t alter)
{
  const char *fmt;
  enum rtx_code code;
  int i, j, len;

  code = GET_CODE (pattern);
  switch (code)
    {
    case MATCH_OPERAND:
      XSTR (pattern, 2) = alter (XSTR (pattern, 2), n_dup);
      break;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);
  for (i = 0; i < len; i++)
    {
      rtx r;

      switch (fmt[i])
	{
	case 'e': case 'u':
	  r = alter_constraints (XEXP (pattern, i), n_dup, alter);
	  if (r == NULL)
	    return r;
	  break;

	case 'E':
	  for (j = XVECLEN (pattern, i) - 1; j >= 0; --j)
	    {
	      r = alter_constraints (XVECEXP (pattern, i, j), n_dup, alter);
	      if (r == NULL)
		return r;
	    }
	  break;

	case 'i': case 'w': case '0': case 's':
	  break;

	default:
	  break;
	}
    }

  return pattern;
}

static const char *
alter_test_for_insn (struct queue_elem *ce_elem,
		     struct queue_elem *insn_elem)
{
  return join_c_conditions (XSTR (ce_elem->data, 1),
			    XSTR (insn_elem->data, 2));
}

/* Modify VAL, which is an attribute expression for the "enabled" attribute,
   to take "ce_enabled" into account.  Return the new expression.  */
static rtx
modify_attr_enabled_ce (rtx val)
{
  rtx eq_attr, str;
  rtx ite;
  eq_attr = rtx_alloc (EQ_ATTR);
  ite = rtx_alloc (IF_THEN_ELSE);
  str = rtx_alloc (CONST_STRING);

  XSTR (eq_attr, 0) = "ce_enabled";
  XSTR (eq_attr, 1) = "yes";
  XSTR (str, 0) = "no";
  XEXP (ite, 0) = eq_attr;
  XEXP (ite, 1) = val;
  XEXP (ite, 2) = str;

  return ite;
}

/* Alter the attribute vector of INSN, which is a COND_EXEC variant created
   from a define_insn pattern.  We must modify the "predicable" attribute
   to be named "ce_enabled", and also change any "enabled" attribute that's
   present so that it takes ce_enabled into account.
   We rely on the fact that INSN was created with copy_rtx, and modify data
   in-place.  */

static void
alter_attrs_for_insn (rtx insn)
{
  static bool global_changes_made = false;
  rtvec vec = XVEC (insn, 4);
  rtvec new_vec;
  rtx val, set;
  int num_elem;
  int predicable_idx = -1;
  int enabled_idx = -1;
  int i;

  if (! vec)
    return;

  num_elem = GET_NUM_ELEM (vec);
  for (i = num_elem - 1; i >= 0; --i)
    {
      rtx sub = RTVEC_ELT (vec, i);
      switch (GET_CODE (sub))
	{
	case SET_ATTR:
	  if (strcmp (XSTR (sub, 0), "predicable") == 0)
	    {
	      predicable_idx = i;
	      XSTR (sub, 0) = "ce_enabled";
	    }
	  else if (strcmp (XSTR (sub, 0), "enabled") == 0)
	    {
	      enabled_idx = i;
	      XSTR (sub, 0) = "nonce_enabled";
	    }
	  break;

	case SET_ATTR_ALTERNATIVE:
	  if (strcmp (XSTR (sub, 0), "predicable") == 0)
	    /* We already give an error elsewhere.  */
	    return;
	  else if (strcmp (XSTR (sub, 0), "enabled") == 0)
	    {
	      enabled_idx = i;
	      XSTR (sub, 0) = "nonce_enabled";
	    }
	  break;

	case SET:
	  if (GET_CODE (SET_DEST (sub)) != ATTR)
	    break;
	  if (strcmp (XSTR (SET_DEST (sub), 0), "predicable") == 0)
	    {
	      sub = SET_SRC (sub);
	      if (GET_CODE (sub) == CONST_STRING)
		{
		  predicable_idx = i;
		  XSTR (sub, 0) = "ce_enabled";
		}
	      else
		/* We already give an error elsewhere.  */
		return;
	      break;
	    }
	  if (strcmp (XSTR (SET_DEST (sub), 0), "enabled") == 0)
	    {
	      enabled_idx = i;
	      XSTR (SET_DEST (sub), 0) = "nonce_enabled";
	    }
	  break;

	default:
	  gcc_unreachable ();
	}
    }
  if (predicable_idx == -1)
    return;

  if (!global_changes_made)
    {
      struct queue_elem *elem;

      global_changes_made = true;
      add_define_attr ("ce_enabled");
      add_define_attr ("nonce_enabled");

      for (elem = define_attr_queue; elem ; elem = elem->next)
	if (strcmp (XSTR (elem->data, 0), "enabled") == 0)
	  {
	    XEXP (elem->data, 2)
	      = modify_attr_enabled_ce (XEXP (elem->data, 2));
	  }
    }
  if (enabled_idx == -1)
    return;

  new_vec = rtvec_alloc (num_elem + 1);
  for (i = 0; i < num_elem; i++)
    RTVEC_ELT (new_vec, i) = RTVEC_ELT (vec, i);
  val = rtx_alloc (IF_THEN_ELSE);
  XEXP (val, 0) = rtx_alloc (EQ_ATTR);
  XEXP (val, 1) = rtx_alloc (CONST_STRING);
  XEXP (val, 2) = rtx_alloc (CONST_STRING);
  XSTR (XEXP (val, 0), 0) = "nonce_enabled";
  XSTR (XEXP (val, 0), 1) = "yes";
  XSTR (XEXP (val, 1), 0) = "yes";
  XSTR (XEXP (val, 2), 0) = "no";
  set = rtx_alloc (SET);
  SET_DEST (set) = rtx_alloc (ATTR);
  XSTR (SET_DEST (set), 0) = "enabled";
  SET_SRC (set) = modify_attr_enabled_ce (val);
  RTVEC_ELT (new_vec, i) = set;
  XVEC (insn, 4) = new_vec;
}

/* As number of constraints is changed after define_subst, we need to
   process attributes as well - we need to duplicate them the same way
   that we duplicated constraints in original pattern
   ELEM is a queue element, containing our rtl-template,
   N_DUP - multiplication factor.  */
static void
alter_attrs_for_subst_insn (struct queue_elem * elem, int n_dup)
{
  rtvec vec = XVEC (elem->data, 4);
  int num_elem;
  int i;

  if (n_dup < 2 || ! vec)
    return;

  num_elem = GET_NUM_ELEM (vec);
  for (i = num_elem - 1; i >= 0; --i)
    {
      rtx sub = RTVEC_ELT (vec, i);
      switch (GET_CODE (sub))
	{
	case SET_ATTR:
	  if (strchr (XSTR (sub, 1), ',') != NULL)
	    XSTR (sub, 1) = duplicate_alternatives (XSTR (sub, 1), n_dup);
	    break;

	case SET_ATTR_ALTERNATIVE:
	case SET:
	  error_with_line (elem->lineno,
			   "%s: `define_subst' does not support attributes "
			   "assigned by `set' and `set_attr_alternative'",
			   XSTR (elem->data, 0));
	  return;

	default:
	  gcc_unreachable ();
	}
    }
}

/* Adjust all of the operand numbers in SRC to match the shift they'll
   get from an operand displacement of DISP.  Return a pointer after the
   adjusted string.  */

static char *
shift_output_template (char *dest, const char *src, int disp)
{
  while (*src)
    {
      char c = *src++;
      *dest++ = c;
      if (c == '%')
	{
	  c = *src++;
	  if (ISDIGIT ((unsigned char) c))
	    c += disp;
	  else if (ISALPHA (c))
	    {
	      *dest++ = c;
	      c = *src++ + disp;
	    }
	  *dest++ = c;
	}
    }

  return dest;
}

static const char *
alter_output_for_insn (struct queue_elem *ce_elem,
		       struct queue_elem *insn_elem,
		       int alt, int max_op)
{
  const char *ce_out, *insn_out;
  char *result, *p;
  size_t len, ce_len, insn_len;

  /* ??? Could coordinate with genoutput to not duplicate code here.  */

  ce_out = XSTR (ce_elem->data, 2);
  insn_out = XTMPL (insn_elem->data, 3);
  if (!ce_out || *ce_out == '\0')
    return insn_out;

  ce_len = strlen (ce_out);
  insn_len = strlen (insn_out);

  if (*insn_out == '*')
    /* You must take care of the predicate yourself.  */
    return insn_out;

  if (*insn_out == '@')
    {
      len = (ce_len + 1) * alt + insn_len + 1;
      p = result = XNEWVEC (char, len);

      do
	{
	  do
	    *p++ = *insn_out++;
	  while (ISSPACE ((unsigned char) *insn_out));

	  if (*insn_out != '#')
	    {
	      p = shift_output_template (p, ce_out, max_op);
	      *p++ = ' ';
	    }

	  do
	    *p++ = *insn_out++;
	  while (*insn_out && *insn_out != '\n');
	}
      while (*insn_out);
      *p = '\0';
    }
  else
    {
      len = ce_len + 1 + insn_len + 1;
      result = XNEWVEC (char, len);

      p = shift_output_template (result, ce_out, max_op);
      *p++ = ' ';
      memcpy (p, insn_out, insn_len + 1);
    }

  return result;
}

/* From string STR "a,b,c" produce "a,b,c,a,b,c,a,b,c", i.e. original
   string, duplicated N_DUP times.  */

static const char *
duplicate_alternatives (const char * str, int n_dup)
{
  int i, len, new_len;
  char *result, *sp;
  const char *cp;

  if (n_dup < 2)
    return str;

  while (ISSPACE (*str))
    str++;

  if (*str == '\0')
    return str;

  cp = str;
  len = strlen (str);
  new_len = (len + 1) * n_dup;

  sp = result = XNEWVEC (char, new_len);

  /* Global modifier characters mustn't be duplicated: skip if found.  */
  if (*cp == '=' || *cp == '+' || *cp == '%')
    {
      *sp++ = *cp++;
      len--;
    }

  /* Copy original constraints N_DUP times.  */
  for (i = 0; i < n_dup; i++, sp += len+1)
    {
      memcpy (sp, cp, len);
      *(sp+len) = (i == n_dup - 1) ? '\0' : ',';
    }

  return result;
}

/* From string STR "a,b,c" produce "a,a,a,b,b,b,c,c,c", i.e. string where
   each alternative from the original string is duplicated N_DUP times.  */
static const char *
duplicate_each_alternative (const char * str, int n_dup)
{
  int i, len, new_len;
  char *result, *sp, *ep, *cp;

  if (n_dup < 2)
    return str;

  while (ISSPACE (*str))
    str++;

  if (*str == '\0')
    return str;

  cp = xstrdup (str);

  new_len = (strlen (cp) + 1) * n_dup;

  sp = result = XNEWVEC (char, new_len);

  /* Global modifier characters mustn't be duplicated: skip if found.  */
  if (*cp == '=' || *cp == '+' || *cp == '%')
      *sp++ = *cp++;

  do
    {
      if ((ep = strchr (cp, ',')) != NULL)
	*ep++ = '\0';
      len = strlen (cp);

      /* Copy a constraint N_DUP times.  */
      for (i = 0; i < n_dup; i++, sp += len + 1)
	{
	  memcpy (sp, cp, len);
	  *(sp+len) = (ep == NULL && i == n_dup - 1) ? '\0' : ',';
	}

      cp = ep;
    }
  while (cp != NULL);

  return result;
}

/* Alter the output of INSN whose pattern was modified by
   DEFINE_SUBST.  We must replicate output strings according
   to the new number of alternatives ALT in substituted pattern.
   If ALT equals 1, output has one alternative or defined by C
   code, then output is returned without any changes.  */

static const char *
alter_output_for_subst_insn (rtx insn, int alt)
{
  const char *insn_out, *sp ;
  char *old_out, *new_out, *cp;
  int i, j, new_len;

  insn_out = XTMPL (insn, 3);

  if (alt < 2 || *insn_out == '*' || *insn_out != '@')
    return insn_out;

  old_out = XNEWVEC (char, strlen (insn_out)),
  sp = insn_out;

  while (ISSPACE (*sp) || *sp == '@')
    sp++;

  for (i = 0; *sp;)
    old_out[i++] = *sp++;

  new_len = alt * (i + 1) + 1;

  new_out = XNEWVEC (char, new_len);
  new_out[0] = '@';

  for (j = 0, cp = new_out + 1; j < alt; j++, cp += i + 1)
    {
      memcpy (cp, old_out, i);
      *(cp+i) = (j == alt - 1) ? '\0' : '\n';
    }

  return new_out;
}

/* Replicate insns as appropriate for the given DEFINE_COND_EXEC.  */

static void
process_one_cond_exec (struct queue_elem *ce_elem)
{
  struct queue_elem *insn_elem;
  for (insn_elem = define_insn_queue; insn_elem ; insn_elem = insn_elem->next)
    {
      int alternatives, max_operand;
      rtx pred, insn, pattern, split;
      char *new_name;
      int i;

      if (! is_predicable (insn_elem))
	continue;

      alternatives = 1;
      max_operand = -1;
      collect_insn_data (insn_elem->data, &alternatives, &max_operand);
      max_operand += 1;

      if (XVECLEN (ce_elem->data, 0) != 1)
	{
	  error_with_line (ce_elem->lineno, "too many patterns in predicate");
	  return;
	}

      pred = copy_rtx (XVECEXP (ce_elem->data, 0, 0));
      pred = alter_predicate_for_insn (pred, alternatives, max_operand,
				       ce_elem->lineno);
      if (pred == NULL)
	return;

      /* Construct a new pattern for the new insn.  */
      insn = copy_rtx (insn_elem->data);
      new_name = XNEWVAR (char, strlen XSTR (insn_elem->data, 0) + 4);
      sprintf (new_name, "*p %s", XSTR (insn_elem->data, 0));
      XSTR (insn, 0) = new_name;
      pattern = rtx_alloc (COND_EXEC);
      XEXP (pattern, 0) = pred;
      if (XVECLEN (insn, 1) == 1)
	{
	  XEXP (pattern, 1) = XVECEXP (insn, 1, 0);
	  XVECEXP (insn, 1, 0) = pattern;
	  PUT_NUM_ELEM (XVEC (insn, 1), 1);
	}
      else
	{
	  XEXP (pattern, 1) = rtx_alloc (PARALLEL);
	  XVEC (XEXP (pattern, 1), 0) = XVEC (insn, 1);
	  XVEC (insn, 1) = rtvec_alloc (1);
	  XVECEXP (insn, 1, 0) = pattern;
	}

       if (XVEC (ce_elem->data, 3) != NULL)
	{
	  rtvec attributes = rtvec_alloc (XVECLEN (insn, 4)
	                                  + XVECLEN (ce_elem->data, 3));
	  int i = 0;
	  int j = 0;
	  for (i = 0; i < XVECLEN (insn, 4); i++)
	    RTVEC_ELT (attributes, i) = XVECEXP (insn, 4, i);

	  for (j = 0; j < XVECLEN (ce_elem->data, 3); j++, i++)
	    RTVEC_ELT (attributes, i) = XVECEXP (ce_elem->data, 3, j);

	  XVEC (insn, 4) = attributes;
	}

      XSTR (insn, 2) = alter_test_for_insn (ce_elem, insn_elem);
      XTMPL (insn, 3) = alter_output_for_insn (ce_elem, insn_elem,
					      alternatives, max_operand);
      alter_attrs_for_insn (insn);

      /* Put the new pattern on the `other' list so that it
	 (a) is not reprocessed by other define_cond_exec patterns
	 (b) appears after all normal define_insn patterns.

	 ??? B is debatable.  If one has normal insns that match
	 cond_exec patterns, they will be preferred over these
	 generated patterns.  Whether this matters in practice, or if
	 it's a good thing, or whether we should thread these new
	 patterns into the define_insn chain just after their generator
	 is something we'll have to experiment with.  */

      queue_pattern (insn, &other_tail, insn_elem->filename,
		     insn_elem->lineno);

      if (!insn_elem->split)
	continue;

      /* If the original insn came from a define_insn_and_split,
	 generate a new split to handle the predicated insn.  */
      split = copy_rtx (insn_elem->split->data);
      /* Predicate the pattern matched by the split.  */
      pattern = rtx_alloc (COND_EXEC);
      XEXP (pattern, 0) = pred;
      if (XVECLEN (split, 0) == 1)
	{
	  XEXP (pattern, 1) = XVECEXP (split, 0, 0);
	  XVECEXP (split, 0, 0) = pattern;
	  PUT_NUM_ELEM (XVEC (split, 0), 1);
	}
      else
	{
	  XEXP (pattern, 1) = rtx_alloc (PARALLEL);
	  XVEC (XEXP (pattern, 1), 0) = XVEC (split, 0);
	  XVEC (split, 0) = rtvec_alloc (1);
	  XVECEXP (split, 0, 0) = pattern;
	}
      /* Predicate all of the insns generated by the split.  */
      for (i = 0; i < XVECLEN (split, 2); i++)
	{
	  pattern = rtx_alloc (COND_EXEC);
	  XEXP (pattern, 0) = pred;
	  XEXP (pattern, 1) = XVECEXP (split, 2, i);
	  XVECEXP (split, 2, i) = pattern;
	}
      /* Add the new split to the queue.  */
      queue_pattern (split, &other_tail, read_md_filename,
		     insn_elem->split->lineno);
    }
}

/* Try to apply define_substs to the given ELEM.
   Only define_substs, specified via attributes would be applied.
   If attribute, requiring define_subst, is set, but no define_subst
   was applied, ELEM would be deleted.  */

static void
process_substs_on_one_elem (struct queue_elem *elem,
			    struct queue_elem *queue)
{
  struct queue_elem *subst_elem;
  int i, j, patterns_match;

  for (subst_elem = define_subst_queue;
       subst_elem; subst_elem = subst_elem->next)
    {
      int alternatives, alternatives_subst;
      rtx subst_pattern;
      rtvec subst_pattern_vec;

      if (!has_subst_attribute (elem, subst_elem))
	continue;

      /* Compare original rtl-pattern from define_insn with input
	 pattern from define_subst.
	 Also, check if numbers of alternatives are the same in all
	 match_operands.  */
      if (XVECLEN (elem->data, 1) != XVECLEN (subst_elem->data, 1))
	continue;
      patterns_match = 1;
      alternatives = -1;
      alternatives_subst = -1;
      for (j = 0; j < XVECLEN (elem->data, 1); j++)
	{
	  if (!subst_pattern_match (XVECEXP (elem->data, 1, j),
				    XVECEXP (subst_elem->data, 1, j),
				    subst_elem->lineno))
	    {
	      patterns_match = 0;
	      break;
	    }

	  if (!get_alternatives_number (XVECEXP (elem->data, 1, j),
					&alternatives, subst_elem->lineno))
	    {
	      patterns_match = 0;
	      break;
	    }
	}

      /* Check if numbers of alternatives are the same in all
	 match_operands in output template of define_subst.  */
      for (j = 0; j < XVECLEN (subst_elem->data, 3); j++)
	{
	  if (!get_alternatives_number (XVECEXP (subst_elem->data, 3, j),
					&alternatives_subst,
					subst_elem->lineno))
	    {
	      patterns_match = 0;
	      break;
	    }
	}

      if (!patterns_match)
	continue;

      /* Clear array in which we save occupied indexes of operands.  */
      memset (used_operands_numbers, 0, sizeof (used_operands_numbers));

      /* Create a pattern, based on the output one from define_subst.  */
      subst_pattern_vec = rtvec_alloc (XVECLEN (subst_elem->data, 3));
      for (j = 0; j < XVECLEN (subst_elem->data, 3); j++)
	{
	  subst_pattern = copy_rtx (XVECEXP (subst_elem->data, 3, j));

	  /* Duplicate constraints in substitute-pattern.  */
	  subst_pattern = alter_constraints (subst_pattern, alternatives,
					     duplicate_each_alternative);

	  subst_pattern = adjust_operands_numbers (subst_pattern);

	  /* Substitute match_dup and match_op_dup in the new pattern and
	     duplicate constraints.  */
	  subst_pattern = subst_dup (subst_pattern, alternatives,
				     alternatives_subst);

	  replace_duplicating_operands_in_pattern (subst_pattern);

	  /* We don't need any constraints in DEFINE_EXPAND.  */
	  if (GET_CODE (elem->data) == DEFINE_EXPAND)
	    remove_constraints (subst_pattern);

	  RTVEC_ELT (subst_pattern_vec, j) = subst_pattern;
	}
      XVEC (elem->data, 1) = subst_pattern_vec;

      for (i = 0; i < MAX_OPERANDS; i++)
	  match_operand_entries_in_pattern[i] = NULL;

      if (GET_CODE (elem->data) == DEFINE_INSN)
	{
	  XTMPL (elem->data, 3) =
	    alter_output_for_subst_insn (elem->data, alternatives_subst);
	  alter_attrs_for_subst_insn (elem, alternatives_subst);
	}

      /* Recalculate condition, joining conditions from original and
	 DEFINE_SUBST input patterns.  */
      XSTR (elem->data, 2) = join_c_conditions (XSTR (subst_elem->data, 2),
						XSTR (elem->data, 2));
      /* Mark that subst was applied by changing attribute from "yes"
	 to "no".  */
      change_subst_attribute (elem, subst_elem, subst_false);
    }

  /* If ELEM contains a subst attribute with value "yes", then we
     expected that a subst would be applied, but it wasn't - so,
     we need to remove that elementto avoid duplicating.  */
  for (subst_elem = define_subst_queue;
       subst_elem; subst_elem = subst_elem->next)
    {
      if (has_subst_attribute (elem, subst_elem))
	{
	  remove_from_queue (elem, &queue);
	  return;
	}
    }
}

/* This is a subroutine of mark_operands_used_in_match_dup.
   This routine is marks all MATCH_OPERANDs inside PATTERN as occupied.  */
static void
mark_operands_from_match_dup (rtx pattern)
{
  const char *fmt;
  int i, j, len, opno;

  if (GET_CODE (pattern) == MATCH_OPERAND
      || GET_CODE (pattern) == MATCH_OPERATOR
      || GET_CODE (pattern) == MATCH_PARALLEL)
    {
      opno = XINT (pattern, 0);
      gcc_assert (opno >= 0 && opno < MAX_OPERANDS);
      used_operands_numbers [opno] = 1;
    }
  fmt = GET_RTX_FORMAT (GET_CODE (pattern));
  len = GET_RTX_LENGTH (GET_CODE (pattern));
  for (i = 0; i < len; i++)
    {
      switch (fmt[i])
	{
	case 'e': case 'u':
	  mark_operands_from_match_dup (XEXP (pattern, i));
	  break;
	case 'E':
	  for (j = XVECLEN (pattern, i) - 1; j >= 0; --j)
	    mark_operands_from_match_dup (XVECEXP (pattern, i, j));
	  break;
	}
    }
}

/* This is a subroutine of adjust_operands_numbers.
   It goes through all expressions in PATTERN and when MATCH_DUP is
   met, all MATCH_OPERANDs inside it is marked as occupied.  The
   process of marking is done by routin mark_operands_from_match_dup.  */
static void
mark_operands_used_in_match_dup (rtx pattern)
{
  const char *fmt;
  int i, j, len, opno;

  if (GET_CODE (pattern) == MATCH_DUP)
    {
      opno = XINT (pattern, 0);
      gcc_assert (opno >= 0 && opno < MAX_OPERANDS);
      mark_operands_from_match_dup (operand_data[opno]);
      return;
    }
  fmt = GET_RTX_FORMAT (GET_CODE (pattern));
  len = GET_RTX_LENGTH (GET_CODE (pattern));
  for (i = 0; i < len; i++)
    {
      switch (fmt[i])
	{
	case 'e': case 'u':
	  mark_operands_used_in_match_dup (XEXP (pattern, i));
	  break;
	case 'E':
	  for (j = XVECLEN (pattern, i) - 1; j >= 0; --j)
	    mark_operands_used_in_match_dup (XVECEXP (pattern, i, j));
	  break;
	}
    }
}

/* This is subroutine of renumerate_operands_in_pattern.
   It finds first not-occupied operand-index.  */
static int
find_first_unused_number_of_operand ()
{
  int i;
  for (i = 0; i < MAX_OPERANDS; i++)
    if (!used_operands_numbers[i])
      return i;
  return MAX_OPERANDS;
}

/* This is subroutine of adjust_operands_numbers.
   It visits all expressions in PATTERN and assigns not-occupied
   operand indexes to MATCH_OPERANDs and MATCH_OPERATORs of this
   PATTERN.  */
static void
renumerate_operands_in_pattern (rtx pattern)
{
  const char *fmt;
  enum rtx_code code;
  int i, j, len, new_opno;
  code = GET_CODE (pattern);

  if (code == MATCH_OPERAND
      || code == MATCH_OPERATOR)
    {
      new_opno = find_first_unused_number_of_operand ();
      gcc_assert (new_opno >= 0 && new_opno < MAX_OPERANDS);
      XINT (pattern, 0) = new_opno;
      used_operands_numbers [new_opno] = 1;
    }

  fmt = GET_RTX_FORMAT (GET_CODE (pattern));
  len = GET_RTX_LENGTH (GET_CODE (pattern));
  for (i = 0; i < len; i++)
    {
      switch (fmt[i])
	{
	case 'e': case 'u':
	  renumerate_operands_in_pattern (XEXP (pattern, i));
	  break;
	case 'E':
	  for (j = XVECLEN (pattern, i) - 1; j >= 0; --j)
	    renumerate_operands_in_pattern (XVECEXP (pattern, i, j));
	  break;
	}
    }
}

/* If output pattern of define_subst contains MATCH_DUP, then this
   expression would be replaced with the pattern, matched with
   MATCH_OPERAND from input pattern.  This pattern could contain any
   number of MATCH_OPERANDs, MATCH_OPERATORs etc., so it's possible
   that a MATCH_OPERAND from output_pattern (if any) would have the
   same number, as MATCH_OPERAND from copied pattern.  To avoid such
   indexes overlapping, we assign new indexes to MATCH_OPERANDs,
   laying in the output pattern outside of MATCH_DUPs.  */
static rtx
adjust_operands_numbers (rtx pattern)
{
  mark_operands_used_in_match_dup (pattern);

  renumerate_operands_in_pattern (pattern);

  return pattern;
}

/* Generate RTL expression
   (match_dup OPNO)
   */
static rtx
generate_match_dup (int opno)
{
  rtx return_rtx = rtx_alloc (MATCH_DUP);
  PUT_CODE (return_rtx, MATCH_DUP);
  XINT (return_rtx, 0) = opno;
  return return_rtx;
}

/* This routine checks all match_operands in PATTERN and if some of
   have the same index, it replaces all of them except the first one  to
   match_dup.
   Usually, match_operands with the same indexes are forbidden, but
   after define_subst copy an RTL-expression from original template,
   indexes of existed and just-copied match_operands could coincide.
   To fix it, we replace one of them with match_dup.  */
static rtx
replace_duplicating_operands_in_pattern (rtx pattern)
{
  const char *fmt;
  int i, j, len, opno;
  rtx mdup;

  if (GET_CODE (pattern) == MATCH_OPERAND)
    {
      opno = XINT (pattern, 0);
      gcc_assert (opno >= 0 && opno < MAX_OPERANDS);
      if (match_operand_entries_in_pattern[opno] == NULL)
	{
	  match_operand_entries_in_pattern[opno] = pattern;
	  return NULL;
	}
      else
	{
	  /* Compare predicates before replacing with match_dup.  */
	  if (strcmp (XSTR (pattern, 1),
		      XSTR (match_operand_entries_in_pattern[opno], 1)))
	    {
	      error ("duplicated match_operands with different predicates were"
		     " found.");
	      return NULL;
	    }
	  return generate_match_dup (opno);
	}
    }
  fmt = GET_RTX_FORMAT (GET_CODE (pattern));
  len = GET_RTX_LENGTH (GET_CODE (pattern));
  for (i = 0; i < len; i++)
    {
      switch (fmt[i])
	{
	case 'e': case 'u':
	  mdup = replace_duplicating_operands_in_pattern (XEXP (pattern, i));
	  if (mdup)
	    XEXP (pattern, i) = mdup;
	  break;
	case 'E':
	  for (j = XVECLEN (pattern, i) - 1; j >= 0; --j)
	    {
	      mdup =
		replace_duplicating_operands_in_pattern (XVECEXP
							 (pattern, i, j));
	      if (mdup)
		XVECEXP (pattern, i, j) = mdup;
	    }
	  break;
	}
    }
  return NULL;
}

/* The routine modifies given input PATTERN of define_subst, replacing
   MATCH_DUP and MATCH_OP_DUP with operands from define_insn original
   pattern, whose operands are stored in OPERAND_DATA array.
   It also duplicates constraints in operands - constraints from
   define_insn operands are duplicated N_SUBST_ALT times, constraints
   from define_subst operands are duplicated N_ALT times.
   After the duplication, returned output rtl-pattern contains every
   combination of input constraints Vs constraints from define_subst
   output.  */
static rtx
subst_dup (rtx pattern, int n_alt, int n_subst_alt)
{
  const char *fmt;
  enum rtx_code code;
  int i, j, len, opno;

  code = GET_CODE (pattern);
  switch (code)
    {
    case MATCH_DUP:
    case MATCH_OP_DUP:
      opno = XINT (pattern, 0);

      gcc_assert (opno >= 0 && opno < MAX_OPERANDS);

      if (operand_data[opno])
	{
	  pattern = copy_rtx (operand_data[opno]);

	  /* Duplicate constraints.  */
	  pattern = alter_constraints (pattern, n_subst_alt,
				       duplicate_alternatives);
	}
      break;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (GET_CODE (pattern));
  len = GET_RTX_LENGTH (GET_CODE (pattern));
  for (i = 0; i < len; i++)
    {
      switch (fmt[i])
	{
	case 'e': case 'u':
	  if (code != MATCH_DUP && code != MATCH_OP_DUP)
	    XEXP (pattern, i) = subst_dup (XEXP (pattern, i),
					   n_alt, n_subst_alt);
	  break;
	case 'V':
	  if (XVEC (pattern, i) == NULL)
	    break;
	case 'E':
	  for (j = XVECLEN (pattern, i) - 1; j >= 0; --j)
	    if (code != MATCH_DUP && code != MATCH_OP_DUP)
	      XVECEXP (pattern, i, j) = subst_dup (XVECEXP (pattern, i, j),
						   n_alt, n_subst_alt);
	  break;

	case 'i': case 'w': case '0': case 's': case 'S': case 'T':
	  break;

	default:
	  gcc_unreachable ();
	}
    }
  return pattern;
}

/* If we have any DEFINE_COND_EXEC patterns, expand the DEFINE_INSN
   patterns appropriately.  */

static void
process_define_cond_exec (void)
{
  struct queue_elem *elem;

  identify_predicable_attribute ();
  if (have_error)
    return;

  for (elem = define_cond_exec_queue; elem ; elem = elem->next)
    process_one_cond_exec (elem);
}

/* If we have any DEFINE_SUBST patterns, expand DEFINE_INSN and
   DEFINE_EXPAND patterns appropriately.  */

static void
process_define_subst (void)
{
  struct queue_elem *elem, *elem_attr;

  /* Check if each define_subst has corresponding define_subst_attr.  */
  for (elem = define_subst_queue; elem ; elem = elem->next)
    {
      for (elem_attr = define_subst_attr_queue;
	   elem_attr;
	   elem_attr = elem_attr->next)
	if (strcmp (XSTR (elem->data, 0), XSTR (elem_attr->data, 1)) == 0)
	    goto found;

	error_with_line (elem->lineno,
			 "%s: `define_subst' must have at least one "
			 "corresponding `define_subst_attr'",
			 XSTR (elem->data, 0));
	return;
      found:
	continue;
    }

  for (elem = define_insn_queue; elem ; elem = elem->next)
    process_substs_on_one_elem (elem, define_insn_queue);
  for (elem = other_queue; elem ; elem = elem->next)
    {
      if (GET_CODE (elem->data) != DEFINE_EXPAND)
	continue;
      process_substs_on_one_elem (elem, other_queue);
    }
}

/* A read_md_files callback for reading an rtx.  */

static void
rtx_handle_directive (int lineno, const char *rtx_name)
{
  rtx queue, x;

  if (read_rtx (rtx_name, &queue))
    for (x = queue; x; x = XEXP (x, 1))
      process_rtx (XEXP (x, 0), lineno);
}

/* Comparison function for the mnemonic hash table.  */

static int
htab_eq_string (const void *s1, const void *s2)
{
  return strcmp ((const char*)s1, (const char*)s2) == 0;
}

/* Add mnemonic STR with length LEN to the mnemonic hash table
   MNEMONIC_HTAB.  A trailing zero end character is appendend to STR
   and a permanent heap copy of STR is created.  */

static void
add_mnemonic_string (htab_t mnemonic_htab, const char *str, int len)
{
  char *new_str;
  void **slot;
  char *str_zero = (char*)alloca (len + 1);

  memcpy (str_zero, str, len);
  str_zero[len] = '\0';

  slot = htab_find_slot (mnemonic_htab, str_zero, INSERT);

  if (*slot)
    return;

  /* Not found; create a permanent copy and add it to the hash table.  */
  new_str = XNEWVAR (char, len + 1);
  memcpy (new_str, str_zero, len + 1);
  *slot = new_str;
}

/* Scan INSN for mnemonic strings and add them to the mnemonic hash
   table in MNEMONIC_HTAB.

   The mnemonics cannot be found if they are emitted using C code.

   If a mnemonic string contains ';' or a newline the string assumed
   to consist of more than a single instruction.  The attribute value
   will then be set to the user defined default value.  */

static void
gen_mnemonic_setattr (htab_t mnemonic_htab, rtx insn)
{
  const char *template_code, *cp;
  int i;
  int vec_len;
  rtx set_attr;
  char *attr_name;
  rtvec new_vec;

  template_code = XTMPL (insn, 3);

  /* Skip patterns which use C code to emit the template.  */
  if (template_code[0] == '*')
    return;

  if (template_code[0] == '@')
    cp = &template_code[1];
  else
    cp = &template_code[0];

  for (i = 0; *cp; )
    {
      const char *ep, *sp;
      int size = 0;

      while (ISSPACE (*cp))
	cp++;

      for (ep = sp = cp; !IS_VSPACE (*ep) && *ep != '\0'; ++ep)
	if (!ISSPACE (*ep))
	  sp = ep + 1;

      if (i > 0)
	obstack_1grow (&string_obstack, ',');

      while (cp < sp && ((*cp >= '0' && *cp <= '9')
			 || (*cp >= 'a' && *cp <= 'z')))

	{
	  obstack_1grow (&string_obstack, *cp);
	  cp++;
	  size++;
	}

      while (cp < sp)
	{
	  if (*cp == ';' || (*cp == '\\' && cp[1] == 'n'))
	    {
	      /* Don't set a value if there are more than one
		 instruction in the string.  */
	      obstack_next_free (&string_obstack) =
		obstack_next_free (&string_obstack) - size;
	      size = 0;

	      cp = sp;
	      break;
	    }
	  cp++;
	}
      if (size == 0)
	obstack_1grow (&string_obstack, '*');
      else
	add_mnemonic_string (mnemonic_htab,
			     obstack_next_free (&string_obstack) - size,
			     size);
      i++;
    }

  /* An insn definition might emit an empty string.  */
  if (obstack_object_size (&string_obstack) == 0)
    return;

  obstack_1grow (&string_obstack, '\0');

  set_attr = rtx_alloc (SET_ATTR);
  XSTR (set_attr, 1) = XOBFINISH (&string_obstack, char *);
  attr_name = XNEWVAR (char, strlen (MNEMONIC_ATTR_NAME) + 1);
  strcpy (attr_name, MNEMONIC_ATTR_NAME);
  XSTR (set_attr, 0) = attr_name;

  if (!XVEC (insn, 4))
    vec_len = 0;
  else
    vec_len = XVECLEN (insn, 4);

  new_vec = rtvec_alloc (vec_len + 1);
  for (i = 0; i < vec_len; i++)
    RTVEC_ELT (new_vec, i) = XVECEXP (insn, 4, i);
  RTVEC_ELT (new_vec, vec_len) = set_attr;
  XVEC (insn, 4) = new_vec;
}

/* This function is called for the elements in the mnemonic hashtable
   and generates a comma separated list of the mnemonics.  */

static int
mnemonic_htab_callback (void **slot, void *info ATTRIBUTE_UNUSED)
{
  obstack_grow (&string_obstack, (char*)*slot, strlen ((char*)*slot));
  obstack_1grow (&string_obstack, ',');
  return 1;
}

/* Generate (set_attr "mnemonic" "..") RTXs and append them to every
   insn definition in case the back end requests it by defining the
   mnemonic attribute.  The values for the attribute will be extracted
   from the output patterns of the insn definitions as far as
   possible.  */

static void
gen_mnemonic_attr (void)
{
  struct queue_elem *elem;
  rtx mnemonic_attr = NULL;
  htab_t mnemonic_htab;
  const char *str, *p;
  int i;

  if (have_error)
    return;

  /* Look for the DEFINE_ATTR for `mnemonic'.  */
  for (elem = define_attr_queue; elem != *define_attr_tail; elem = elem->next)
    if (GET_CODE (elem->data) == DEFINE_ATTR
	&& strcmp (XSTR (elem->data, 0), MNEMONIC_ATTR_NAME) == 0)
      {
	mnemonic_attr = elem->data;
	break;
      }

  /* A (define_attr "mnemonic" "...") indicates that the back-end
     wants a mnemonic attribute to be generated.  */
  if (!mnemonic_attr)
    return;

  mnemonic_htab = htab_create_alloc (MNEMONIC_HTAB_SIZE, htab_hash_string,
				     htab_eq_string, 0, xcalloc, free);

  for (elem = define_insn_queue; elem; elem = elem->next)
    {
      rtx insn = elem->data;
      bool found = false;

      /* Check if the insn definition already has
	 (set_attr "mnemonic" ...).  */
      if (XVEC (insn, 4))
 	for (i = 0; i < XVECLEN (insn, 4); i++)
	  if (strcmp (XSTR (XVECEXP (insn, 4, i), 0), MNEMONIC_ATTR_NAME) == 0)
	    {
	      found = true;
	      break;
	    }

      if (!found)
	gen_mnemonic_setattr (mnemonic_htab, insn);
    }

  /* Add the user defined values to the hash table.  */
  str = XSTR (mnemonic_attr, 1);
  while ((p = scan_comma_elt (&str)) != NULL)
    add_mnemonic_string (mnemonic_htab, p, str - p);

  htab_traverse (mnemonic_htab, mnemonic_htab_callback, NULL);

  /* Replace the last ',' with the zero end character.  */
  *((char *)obstack_next_free (&string_obstack) - 1) = '\0';
  XSTR (mnemonic_attr, 1) = XOBFINISH (&string_obstack, char *);
}

/* Check if there are DEFINE_ATTRs with the same name.  */
static void
check_define_attr_duplicates ()
{
  struct queue_elem *elem;
  htab_t attr_htab;
  char * attr_name;
  void **slot;

  attr_htab = htab_create (500, htab_hash_string, htab_eq_string, NULL);

  for (elem = define_attr_queue; elem; elem = elem->next)
    {
      attr_name = xstrdup (XSTR (elem->data, 0));

      slot = htab_find_slot (attr_htab, attr_name, INSERT);

      /* Duplicate.  */
      if (*slot)
	{
	  error_with_line (elem->lineno, "redefinition of attribute '%s'",
			   attr_name);
	  htab_delete (attr_htab);
	  return;
	}

      *slot = attr_name;
    }

  htab_delete (attr_htab);
}

/* The entry point for initializing the reader.  */

bool
init_rtx_reader_args_cb (int argc, char **argv,
			 bool (*parse_opt) (const char *))
{
  /* Prepare to read input.  */
  condition_table = htab_create (500, hash_c_test, cmp_c_test, NULL);
  init_predicate_table ();
  obstack_init (rtl_obstack);

  /* Start at 1, to make 0 available for CODE_FOR_nothing.  */
  sequence_num = 1;

  read_md_files (argc, argv, parse_opt, rtx_handle_directive);

  if (define_attr_queue != NULL)
    check_define_attr_duplicates ();

  /* Process define_cond_exec patterns.  */
  if (define_cond_exec_queue != NULL)
    process_define_cond_exec ();

  /* Process define_subst patterns.  */
  if (define_subst_queue != NULL)
    process_define_subst ();

  if (define_attr_queue != NULL)
    gen_mnemonic_attr ();

  return !have_error;
}

/* Programs that don't have their own options can use this entry point
   instead.  */
bool
init_rtx_reader_args (int argc, char **argv)
{
  return init_rtx_reader_args_cb (argc, argv, 0);
}

/* The entry point for reading a single rtx from an md file.  Return
   the rtx, or NULL if the md file has been fully processed.
   Return the line where the rtx was found in LINENO.
   Return the number of code generating rtx'en read since the start
   of the md file in SEQNR.  */

rtx
read_md_rtx (int *lineno, int *seqnr)
{
  struct queue_elem **queue, *elem;
  rtx desc;

 discard:

  /* Read all patterns from a given queue before moving on to the next.  */
  if (define_attr_queue != NULL)
    queue = &define_attr_queue;
  else if (define_pred_queue != NULL)
    queue = &define_pred_queue;
  else if (define_insn_queue != NULL)
    queue = &define_insn_queue;
  else if (other_queue != NULL)
    queue = &other_queue;
  else
    return NULL_RTX;

  elem = *queue;
  *queue = elem->next;
  desc = elem->data;
  read_md_filename = elem->filename;
  *lineno = elem->lineno;
  *seqnr = sequence_num;

  free (elem);

  /* Discard insn patterns which we know can never match (because
     their C test is provably always false).  If insn_elision is
     false, our caller needs to see all the patterns.  Note that the
     elided patterns are never counted by the sequence numbering; it
     is the caller's responsibility, when insn_elision is false, not
     to use elided pattern numbers for anything.  */
  switch (GET_CODE (desc))
    {
    case DEFINE_INSN:
    case DEFINE_EXPAND:
    case DEFINE_SUBST:
      if (maybe_eval_c_test (XSTR (desc, 2)) != 0)
	sequence_num++;
      else if (insn_elision)
	goto discard;

      /* *seqnr is used here so the name table will match caller's
	 idea of insn numbering, whether or not elision is active.  */
      record_insn_name (*seqnr, XSTR (desc, 0));
      break;

    case DEFINE_SPLIT:
    case DEFINE_PEEPHOLE:
    case DEFINE_PEEPHOLE2:
      if (maybe_eval_c_test (XSTR (desc, 1)) != 0)
	sequence_num++;
      else if (insn_elision)
	    goto discard;
      break;

    default:
      break;
    }

  return desc;
}

/* Helper functions for insn elision.  */

/* Compute a hash function of a c_test structure, which is keyed
   by its ->expr field.  */
hashval_t
hash_c_test (const void *x)
{
  const struct c_test *a = (const struct c_test *) x;
  const unsigned char *base, *s = (const unsigned char *) a->expr;
  hashval_t hash;
  unsigned char c;
  unsigned int len;

  base = s;
  hash = 0;

  while ((c = *s++) != '\0')
    {
      hash += c + (c << 17);
      hash ^= hash >> 2;
    }

  len = s - base;
  hash += len + (len << 17);
  hash ^= hash >> 2;

  return hash;
}

/* Compare two c_test expression structures.  */
int
cmp_c_test (const void *x, const void *y)
{
  const struct c_test *a = (const struct c_test *) x;
  const struct c_test *b = (const struct c_test *) y;

  return !strcmp (a->expr, b->expr);
}

/* Given a string representing a C test expression, look it up in the
   condition_table and report whether or not its value is known
   at compile time.  Returns a tristate: 1 for known true, 0 for
   known false, -1 for unknown.  */
int
maybe_eval_c_test (const char *expr)
{
  const struct c_test *test;
  struct c_test dummy;

  if (expr[0] == 0)
    return 1;

  dummy.expr = expr;
  test = (const struct c_test *)htab_find (condition_table, &dummy);
  if (!test)
    return -1;
  return test->value;
}

/* Record the C test expression EXPR in the condition_table, with
   value VAL.  Duplicates clobber previous entries.  */

void
add_c_test (const char *expr, int value)
{
  struct c_test *test;

  if (expr[0] == 0)
    return;

  test = XNEW (struct c_test);
  test->expr = expr;
  test->value = value;

  *(htab_find_slot (condition_table, test, INSERT)) = test;
}

/* For every C test, call CALLBACK with two arguments: a pointer to
   the condition structure and INFO.  Stops when CALLBACK returns zero.  */
void
traverse_c_tests (htab_trav callback, void *info)
{
  if (condition_table)
    htab_traverse (condition_table, callback, info);
}

/* Helper functions for define_predicate and define_special_predicate
   processing.  Shared between genrecog.c and genpreds.c.  */

static htab_t predicate_table;
struct pred_data *first_predicate;
static struct pred_data **last_predicate = &first_predicate;

static hashval_t
hash_struct_pred_data (const void *ptr)
{
  return htab_hash_string (((const struct pred_data *)ptr)->name);
}

static int
eq_struct_pred_data (const void *a, const void *b)
{
  return !strcmp (((const struct pred_data *)a)->name,
		  ((const struct pred_data *)b)->name);
}

struct pred_data *
lookup_predicate (const char *name)
{
  struct pred_data key;
  key.name = name;
  return (struct pred_data *) htab_find (predicate_table, &key);
}

/* Record that predicate PRED can accept CODE.  */

void
add_predicate_code (struct pred_data *pred, enum rtx_code code)
{
  if (!pred->codes[code])
    {
      pred->num_codes++;
      pred->codes[code] = true;

      if (GET_RTX_CLASS (code) != RTX_CONST_OBJ)
	pred->allows_non_const = true;

      if (code != REG
	  && code != SUBREG
	  && code != MEM
	  && code != CONCAT
	  && code != PARALLEL
	  && code != STRICT_LOW_PART
	  && code != SCRATCH)
	pred->allows_non_lvalue = true;

      if (pred->num_codes == 1)
	pred->singleton = code;
      else if (pred->num_codes == 2)
	pred->singleton = UNKNOWN;
    }
}

void
add_predicate (struct pred_data *pred)
{
  void **slot = htab_find_slot (predicate_table, pred, INSERT);
  if (*slot)
    {
      error ("duplicate predicate definition for '%s'", pred->name);
      return;
    }
  *slot = pred;
  *last_predicate = pred;
  last_predicate = &pred->next;
}

/* This array gives the initial content of the predicate table.  It
   has entries for all predicates defined in recog.c.  */

struct std_pred_table
{
  const char *name;
  bool special;
  bool allows_const_p;
  RTX_CODE codes[NUM_RTX_CODE];
};

static const struct std_pred_table std_preds[] = {
  {"general_operand", false, true, {SUBREG, REG, MEM}},
  {"address_operand", true, true, {SUBREG, REG, MEM, PLUS, MINUS, MULT}},
  {"register_operand", false, false, {SUBREG, REG}},
  {"pmode_register_operand", true, false, {SUBREG, REG}},
  {"scratch_operand", false, false, {SCRATCH, REG}},
  {"immediate_operand", false, true, {UNKNOWN}},
  {"const_int_operand", false, false, {CONST_INT}},
  {"const_double_operand", false, false, {CONST_INT, CONST_DOUBLE}},
  {"nonimmediate_operand", false, false, {SUBREG, REG, MEM}},
  {"nonmemory_operand", false, true, {SUBREG, REG}},
  {"push_operand", false, false, {MEM}},
  {"pop_operand", false, false, {MEM}},
  {"memory_operand", false, false, {SUBREG, MEM}},
  {"indirect_operand", false, false, {SUBREG, MEM}},
  {"ordered_comparison_operator", false, false, {EQ, NE,
						 LE, LT, GE, GT,
						 LEU, LTU, GEU, GTU}},
  {"comparison_operator", false, false, {EQ, NE,
					 LE, LT, GE, GT,
					 LEU, LTU, GEU, GTU,
					 UNORDERED, ORDERED,
					 UNEQ, UNGE, UNGT,
					 UNLE, UNLT, LTGT}}
};
#define NUM_KNOWN_STD_PREDS ARRAY_SIZE (std_preds)

/* Initialize the table of predicate definitions, starting with
   the information we have on generic predicates.  */

static void
init_predicate_table (void)
{
  size_t i, j;
  struct pred_data *pred;

  predicate_table = htab_create_alloc (37, hash_struct_pred_data,
				       eq_struct_pred_data, 0,
				       xcalloc, free);

  for (i = 0; i < NUM_KNOWN_STD_PREDS; i++)
    {
      pred = XCNEW (struct pred_data);
      pred->name = std_preds[i].name;
      pred->special = std_preds[i].special;

      for (j = 0; std_preds[i].codes[j] != 0; j++)
	add_predicate_code (pred, std_preds[i].codes[j]);

      if (std_preds[i].allows_const_p)
	for (j = 0; j < NUM_RTX_CODE; j++)
	  if (GET_RTX_CLASS (j) == RTX_CONST_OBJ)
	    add_predicate_code (pred, (enum rtx_code) j);

      add_predicate (pred);
    }
}

/* These functions allow linkage with print-rtl.c.  Also, some generators
   like to annotate their output with insn names.  */

/* Holds an array of names indexed by insn_code_number.  */
static char **insn_name_ptr = 0;
static int insn_name_ptr_size = 0;

const char *
get_insn_name (int code)
{
  if (code < insn_name_ptr_size)
    return insn_name_ptr[code];
  else
    return NULL;
}

static void
record_insn_name (int code, const char *name)
{
  static const char *last_real_name = "insn";
  static int last_real_code = 0;
  char *new_name;

  if (insn_name_ptr_size <= code)
    {
      int new_size;
      new_size = (insn_name_ptr_size ? insn_name_ptr_size * 2 : 512);
      insn_name_ptr = XRESIZEVEC (char *, insn_name_ptr, new_size);
      memset (insn_name_ptr + insn_name_ptr_size, 0,
	      sizeof(char *) * (new_size - insn_name_ptr_size));
      insn_name_ptr_size = new_size;
    }

  if (!name || name[0] == '\0')
    {
      new_name = XNEWVAR (char, strlen (last_real_name) + 10);
      sprintf (new_name, "%s+%d", last_real_name, code - last_real_code);
    }
  else
    {
      last_real_name = new_name = xstrdup (name);
      last_real_code = code;
    }

  insn_name_ptr[code] = new_name;
}

/* Make STATS describe the operands that appear in rtx X.  */

static void
get_pattern_stats_1 (struct pattern_stats *stats, rtx x)
{
  RTX_CODE code;
  int i;
  int len;
  const char *fmt;

  if (x == NULL_RTX)
    return;

  code = GET_CODE (x);
  switch (code)
    {
    case MATCH_OPERAND:
    case MATCH_OPERATOR:
    case MATCH_PARALLEL:
      stats->max_opno = MAX (stats->max_opno, XINT (x, 0));
      break;

    case MATCH_DUP:
    case MATCH_OP_DUP:
    case MATCH_PAR_DUP:
      stats->num_dups++;
      stats->max_dup_opno = MAX (stats->max_dup_opno, XINT (x, 0));
      break;

    case MATCH_SCRATCH:
      stats->max_scratch_opno = MAX (stats->max_scratch_opno, XINT (x, 0));
      break;

    default:
      break;
    }

  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);
  for (i = 0; i < len; i++)
    {
      if (fmt[i] == 'e' || fmt[i] == 'u')
	get_pattern_stats_1 (stats, XEXP (x, i));
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    get_pattern_stats_1 (stats, XVECEXP (x, i, j));
	}
    }
}

/* Make STATS describe the operands that appear in instruction pattern
   PATTERN.  */

void
get_pattern_stats (struct pattern_stats *stats, rtvec pattern)
{
  int i, len;

  stats->max_opno = -1;
  stats->max_dup_opno = -1;
  stats->max_scratch_opno = -1;
  stats->num_dups = 0;

  len = GET_NUM_ELEM (pattern);
  for (i = 0; i < len; i++)
    get_pattern_stats_1 (stats, RTVEC_ELT (pattern, i));

  stats->num_generator_args = stats->max_opno + 1;
  stats->num_insn_operands = MAX (stats->max_opno,
				  stats->max_scratch_opno) + 1;
  stats->num_operand_vars = MAX (stats->max_opno,
				  MAX (stats->max_dup_opno,
				       stats->max_scratch_opno)) + 1;
}
