/* Generate code from to output assembler insns as recognized from rtl.
   Copyright (C) 1987, 88, 92, 94-95, 97-99, 2000
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


/* This program reads the machine description for the compiler target machine
   and produces a file containing these things:

   1. An array of `struct insn_data', which is indexed by insn code number,
   which contains:

     a. `name' is the name for that pattern.  Nameless patterns are
     given a name.

     b. `output' hold either the output template, an array of output
     templates, or an output function.

     c. `genfun' is the function to generate a body for that pattern,
     given operands as arguments.

     d. `n_operands' is the number of distinct operands in the pattern
     for that insn,

     e. `n_dups' is the number of match_dup's that appear in the insn's
     pattern.  This says how many elements of `recog_data.dup_loc' are
     significant after an insn has been recognized.

     f. `n_alternatives' is the number of alternatives in the constraints
     of each pattern.

     g. `output_format' tells what type of thing `output' is.

     h. `operand' is the base of an array of operand data for the insn.

   2. An array of `struct insn_operand data', used by `operand' above.

     a. `predicate', an int-valued function, is the match_operand predicate
     for this operand.

     b. `constraint' is the constraint for this operand.  This exists
     only if register constraints appear in match_operand rtx's.

     c. `address_p' indicates that the operand appears within ADDRESS
     rtx's.  This exists only if there are *no* register constraints
     in the match_operand rtx's.

     d. `mode' is the machine mode that that operand is supposed to have.

     e. `strict_low', is nonzero for operands contained in a STRICT_LOW_PART.

     f. `eliminable', is nonzero for operands that are matched normally by
     MATCH_OPERAND; it is zero for operands that should not be changed during
     register elimination such as MATCH_OPERATORs.

  The code number of an insn is simply its position in the machine
  description; code numbers are assigned sequentially to entries in
  the description, starting with code number 0.

  Thus, the following entry in the machine description

    (define_insn "clrdf"
      [(set (match_operand:DF 0 "general_operand" "")
	    (const_int 0))]
      ""
      "clrd %0")

  assuming it is the 25th entry present, would cause
  insn_data[24].template to be "clrd %0", and
  insn_data[24].n_operands to be 1.  */

#include "hconfig.h"
#include "system.h"
#include "rtl.h"
#include "obstack.h"
#include "errors.h"

/* No instruction can have more operands than this.  Sorry for this
   arbitrary limit, but what machine will have an instruction with
   this many operands?  */

#define MAX_MAX_OPERANDS 40

static struct obstack obstack;
struct obstack *rtl_obstack = &obstack;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

static int n_occurrences PARAMS ((int, char *));

/* insns in the machine description are assigned sequential code numbers
   that are used by insn-recog.c (produced by genrecog) to communicate
   to insn-output.c (produced by this program).  */

static int next_code_number;

/* This counts all definitions in the md file,
   for the sake of error messages.  */

static int next_index_number;

/* This counts all operands used in the md file.  The first is null.  */

static int next_operand_number = 1;

/* Record in this chain all information about the operands we will output.  */

struct operand_data
{
  struct operand_data *next;
  int index;
  const char *predicate;
  const char *constraint;
  enum machine_mode mode;
  unsigned char n_alternatives;
  char address_p;
  char strict_low;
  char eliminable;
  char seen;
};

/* Begin with a null operand at index 0.  */

static struct operand_data null_operand =
{
  0, 0, "", "", VOIDmode, 0, 0, 0, 0, 0
};

static struct operand_data *odata = &null_operand;
static struct operand_data **odata_end = &null_operand.next;

/* Must match the constants in recog.h.  */

#define INSN_OUTPUT_FORMAT_NONE         0       /* abort */
#define INSN_OUTPUT_FORMAT_SINGLE       1       /* const char * */
#define INSN_OUTPUT_FORMAT_MULTI        2       /* const char * const * */
#define INSN_OUTPUT_FORMAT_FUNCTION     3       /* const char * (*)(...) */

/* Record in this chain all information that we will output,
   associated with the code number of the insn.  */

struct data
{
  struct data *next;
  const char *name;
  const char *template;
  int code_number;
  int index_number;
  int n_operands;		/* Number of operands this insn recognizes */
  int n_dups;			/* Number times match_dup appears in pattern */
  int n_alternatives;		/* Number of alternatives in each constraint */
  int operand_number;		/* Operand index in the big array.  */
  int output_format;		/* INSN_OUTPUT_FORMAT_*.  */
  struct operand_data operand[MAX_MAX_OPERANDS];
};

/* This variable points to the first link in the insn chain.  */

static struct data *idata, **idata_end = &idata;

static void output_prologue PARAMS ((void));
static void output_predicate_decls PARAMS ((void));
static void output_operand_data PARAMS ((void));
static void output_insn_data PARAMS ((void));
static void output_get_insn_name PARAMS ((void));
static void scan_operands PARAMS ((struct data *, rtx, int, int));
static int compare_operands PARAMS ((struct operand_data *,
				   struct operand_data *));
static void place_operands PARAMS ((struct data *));
static void process_template PARAMS ((struct data *, char *));
static void validate_insn_alternatives PARAMS ((struct data *));
static void gen_insn PARAMS ((rtx));
static void gen_peephole PARAMS ((rtx));
static void gen_expand PARAMS ((rtx));
static void gen_split PARAMS ((rtx));
static int n_occurrences PARAMS ((int, char *));

const char *
get_insn_name (index)
     int index;
{
  static char buf[100];

  struct data *i, *last_named = NULL;
  for (i = idata; i ; i = i->next)
    {
      if (i->index_number == index)
	return i->name;
      if (i->name)
	last_named = i;
    }

  if (last_named)
    sprintf(buf, "%s+%d", last_named->name, index - last_named->index_number);
  else
    sprintf(buf, "insn %d", index);

  return buf;
}

static void
output_prologue ()
{
  printf ("/* Generated automatically by the program `genoutput'\n\
from the machine description file `md'.  */\n\n");

  printf ("#define NO_MD_PROTOTYPES\n");
  printf ("#include \"config.h\"\n");
  printf ("#include \"system.h\"\n");
  printf ("#include \"flags.h\"\n");
  printf ("#include \"rtl.h\"\n");
  printf ("#include \"tm_p.h\"\n");
  printf ("#include \"function.h\"\n");
  printf ("#include \"regs.h\"\n");
  printf ("#include \"hard-reg-set.h\"\n");
  printf ("#include \"real.h\"\n");
  printf ("#include \"insn-config.h\"\n\n");
  printf ("#include \"conditions.h\"\n");
  printf ("#include \"insn-flags.h\"\n");
  printf ("#include \"insn-attr.h\"\n\n");
  printf ("#include \"insn-codes.h\"\n\n");
  printf ("#include \"recog.h\"\n\n");
  printf ("#include \"toplev.h\"\n");
  printf ("#include \"output.h\"\n");
}


/* We need to define all predicates used.  Keep a list of those we
   have defined so far.  There normally aren't very many predicates
   used, so a linked list should be fast enough.  */

static void
output_predicate_decls ()
{
  struct predicate { const char *name; struct predicate *next; } *predicates = 0;
  register struct operand_data *d;
  struct predicate *p;

  for (d = odata; d; d = d->next)
    if (d->predicate && d->predicate[0])
      {
	for (p = predicates; p; p = p->next)
	  if (strcmp (p->name, d->predicate) == 0)
	    break;

	if (p == 0)
	  {
	    printf ("extern int %s PARAMS ((rtx, enum machine_mode));\n",
		    d->predicate);
	    p = (struct predicate *) alloca (sizeof (struct predicate));
	    p->name = d->predicate;
	    p->next = predicates;
	    predicates = p;
	  }
      }

  printf ("\n\n");
}

static void
output_operand_data ()
{
  register struct operand_data *d;

  printf ("\nstatic const struct insn_operand_data operand_data[] = \n{\n");

  for (d = odata; d; d = d->next)
    {
      printf ("  {\n");

      printf ("    %s,\n",
	      d->predicate && d->predicate[0] ? d->predicate : "0");

      printf ("    \"%s\",\n", d->constraint ? d->constraint : "");

      printf ("    %smode,\n", GET_MODE_NAME (d->mode));

      printf ("    %d,\n", d->strict_low);

      printf ("    %d\n", d->eliminable);

      printf("  },\n");
    }
  printf("};\n\n\n");
}

static void
output_insn_data ()
{
  register struct data *d;
  int name_offset = 0;
  int next_name_offset;
  const char * last_name = 0;
  const char * next_name = 0;
  register struct data *n;

  for (n = idata, next_name_offset = 1; n; n = n->next, next_name_offset++)
    if (n->name)
      {
	next_name = n->name;
	break;
      }

  printf ("\nconst struct insn_data insn_data[] = \n{\n");

  for (d = idata; d; d = d->next)
    {
      printf ("  {\n");

      if (d->name)
	{
	  printf ("    \"%s\",\n", d->name);
	  name_offset = 0;
	  last_name = d->name;
	  next_name = 0;
	  for (n = d->next, next_name_offset = 1; n;
	       n = n->next, next_name_offset++)
	    {
	      if (n->name)
		{
		  next_name = n->name;
		  break;
		}
	    }
	}
      else
	{
	  name_offset++;
	  if (next_name && (last_name == 0
			    || name_offset > next_name_offset / 2))
	    printf ("    \"%s-%d\",\n", next_name,
		    next_name_offset - name_offset);
	  else
	    printf ("    \"%s+%d\",\n", last_name, name_offset);
	}

      switch (d->output_format)
	{
	case INSN_OUTPUT_FORMAT_NONE:
	  printf ("    0,\n");
	  break;
	case INSN_OUTPUT_FORMAT_SINGLE:
	  printf ("    \"%s\",\n", d->template);
	  break;
	case INSN_OUTPUT_FORMAT_MULTI:
	case INSN_OUTPUT_FORMAT_FUNCTION:
	  printf ("    (const PTR) output_%d,\n", d->code_number);
	  break;
	default:
	  abort ();
	}

      if (d->name && d->name[0] != '*')
	printf ("    gen_%s,\n", d->name);
      else
	printf ("    0,\n");

      printf ("    &operand_data[%d],\n", d->operand_number);
      printf ("    %d,\n", d->n_operands);
      printf ("    %d,\n", d->n_dups);
      printf ("    %d,\n", d->n_alternatives);
      printf ("    %d\n", d->output_format);

      printf("  },\n");
    }
  printf ("};\n\n\n");
}

static void
output_get_insn_name ()
{
  printf ("const char *\n");
  printf ("get_insn_name (code)\n");
  printf ("     int code;\n");
  printf ("{\n");
  printf ("  return insn_data[code].name;\n");
  printf ("}\n");
}


/* Stores in max_opno the largest operand number present in `part', if
   that is larger than the previous value of max_opno, and the rest of
   the operand data into `d->operand[i]'.

   THIS_ADDRESS_P is nonzero if the containing rtx was an ADDRESS.
   THIS_STRICT_LOW is nonzero if the containing rtx was a STRICT_LOW_PART.  */

static int max_opno;
static int num_dups;

static void
scan_operands (d, part, this_address_p, this_strict_low)
     struct data *d;
     rtx part;
     int this_address_p;
     int this_strict_low;
{
  register int i, j;
  register const char *format_ptr;
  int opno;

  if (part == 0)
    return;

  switch (GET_CODE (part))
    {
    case MATCH_OPERAND:
      opno = XINT (part, 0);
      if (opno > max_opno)
	max_opno = opno;
      if (max_opno >= MAX_MAX_OPERANDS)
	{
	  error ("Too many operands (%d) in definition %s.\n",
		 max_opno + 1, get_insn_name (next_index_number));
	  return;
	}
      if (d->operand[opno].seen)
	error ("Definition %s specified operand number %d more than once.\n",
	       get_insn_name (next_index_number), opno);
      d->operand[opno].seen = 1;
      d->operand[opno].mode = GET_MODE (part);
      d->operand[opno].strict_low = this_strict_low;
      d->operand[opno].predicate = XSTR (part, 1);
      d->operand[opno].constraint = XSTR (part, 2);
      if (XSTR (part, 2) != 0 && *XSTR (part, 2) != 0)
	d->operand[opno].n_alternatives
	  = n_occurrences (',', XSTR (part, 2)) + 1;
      d->operand[opno].address_p = this_address_p;
      d->operand[opno].eliminable = 1;
      return;

    case MATCH_SCRATCH:
      opno = XINT (part, 0);
      if (opno > max_opno)
	max_opno = opno;
      if (max_opno >= MAX_MAX_OPERANDS)
	{
	  error ("Too many operands (%d) in definition %s.\n",
		 max_opno + 1, get_insn_name (next_index_number));
	  return;
	}
      if (d->operand[opno].seen)
	error ("Definition %s specified operand number %d more than once.\n",
	       get_insn_name (next_index_number), opno);
      d->operand[opno].seen = 1;
      d->operand[opno].mode = GET_MODE (part);
      d->operand[opno].strict_low = 0;
      d->operand[opno].predicate = "scratch_operand";
      d->operand[opno].constraint = XSTR (part, 1);
      if (XSTR (part, 1) != 0 && *XSTR (part, 1) != 0)
	d->operand[opno].n_alternatives
	  = n_occurrences (',', XSTR (part, 1)) + 1;
      d->operand[opno].address_p = 0;
      d->operand[opno].eliminable = 0;
      return;

    case MATCH_OPERATOR:
    case MATCH_PARALLEL:
      opno = XINT (part, 0);
      if (opno > max_opno)
	max_opno = opno;
      if (max_opno >= MAX_MAX_OPERANDS)
	{
	  error ("Too many operands (%d) in definition %s.\n",
		 max_opno + 1, get_insn_name (next_index_number));
	  return;
	}
      if (d->operand[opno].seen)
	error ("Definition %s specified operand number %d more than once.\n",
	       get_insn_name (next_index_number), opno);
      d->operand[opno].seen = 1;
      d->operand[opno].mode = GET_MODE (part);
      d->operand[opno].strict_low = 0;
      d->operand[opno].predicate = XSTR (part, 1);
      d->operand[opno].constraint = 0;
      d->operand[opno].address_p = 0;
      d->operand[opno].eliminable = 0;
      for (i = 0; i < XVECLEN (part, 2); i++)
	scan_operands (d, XVECEXP (part, 2, i), 0, 0);
      return;

    case MATCH_DUP:
    case MATCH_OP_DUP:
    case MATCH_PAR_DUP:
      ++num_dups;
      return;

    case ADDRESS:
      scan_operands (d, XEXP (part, 0), 1, 0);
      return;

    case STRICT_LOW_PART:
      scan_operands (d, XEXP (part, 0), 0, 1);
      return;
      
    default:
      break;
    }

  format_ptr = GET_RTX_FORMAT (GET_CODE (part));

  for (i = 0; i < GET_RTX_LENGTH (GET_CODE (part)); i++)
    switch (*format_ptr++)
      {
      case 'e':
      case 'u':
	scan_operands (d, XEXP (part, i), 0, 0);
	break;
      case 'E':
	if (XVEC (part, i) != NULL)
	  for (j = 0; j < XVECLEN (part, i); j++)
	    scan_operands (d, XVECEXP (part, i, j), 0, 0);
	break;
      }
}

/* Compare two operands for content equality.  */

static int
compare_operands (d0, d1)
     struct operand_data *d0, *d1;
{
  const char *p0, *p1;

  p0 = d0->predicate;
  if (!p0)
    p0 = "";
  p1 = d1->predicate;
  if (!p1)
    p1 = "";
  if (strcmp (p0, p1) != 0)
    return 0;

  p0 = d0->constraint;
  if (!p0)
    p0 = "";
  p1 = d1->constraint;
  if (!p1)
    p1 = "";
  if (strcmp (p0, p1) != 0)
    return 0;

  if (d0->mode != d1->mode)
    return 0;

  if (d0->strict_low != d1->strict_low)
    return 0;

  if (d0->eliminable != d1->eliminable)
    return 0;

  return 1;
}

/* Scan the list of operands we've already committed to output and either
   find a subsequence that is the same, or allocate a new one at the end.  */

static void
place_operands (d)
     struct data *d;
{
  struct operand_data *od, *od2;
  int i;

  if (d->n_operands == 0)
    {
      d->operand_number = 0;
      return;
    }

  /* Brute force substring search.  */
  for (od = odata, i = 0; od; od = od->next, i = 0)
    if (compare_operands (od, &d->operand[0]))
      {
	od2 = od->next;
	i = 1;
	while (1)
	  {
	    if (i == d->n_operands)
	      goto full_match;
	    if (od2 == NULL)
	      goto partial_match;
	    if (! compare_operands (od2, &d->operand[i]))
	      break;
	    ++i, od2 = od2->next;
	  }
      }

  /* Either partial match at the end of the list, or no match.  In either
     case, we tack on what operands are remaining to the end of the list.  */
 partial_match:
  d->operand_number = next_operand_number - i;
  for (; i < d->n_operands; ++i)
    {
      od2 = &d->operand[i];
      *odata_end = od2;
      odata_end = &od2->next;
      od2->index = next_operand_number++;
    }
  *odata_end = NULL;
  return;

 full_match:
  d->operand_number = od->index;
  return;
}


/* Process an assembler template from a define_insn or a define_peephole.
   It is either the assembler code template, a list of assembler code
   templates, or C code to generate the assembler code template.  */

static void
process_template (d, template)
    struct data *d;
    char *template;
{
  register char *cp;
  register int i;

  /* Templates starting with * contain straight code to be run.  */
  if (template[0] == '*')
    {
      d->template = 0;
      d->output_format = INSN_OUTPUT_FORMAT_FUNCTION;

      printf ("\nstatic const char *output_%d PARAMS ((rtx *, rtx));\n",
	      d->code_number);
      puts ("\nstatic const char *");
      printf ("output_%d (operands, insn)\n", d->code_number);
      puts ("     rtx *operands ATTRIBUTE_UNUSED;");
      puts ("     rtx insn ATTRIBUTE_UNUSED;");
      puts ("{");

      puts (template + 1);
      puts ("}");
    }

  /* If the assembler code template starts with a @ it is a newline-separated
     list of assembler code templates, one for each alternative.  */
  else if (template[0] == '@')
    {
      d->template = 0;
      d->output_format = INSN_OUTPUT_FORMAT_MULTI;

      printf ("\nstatic const char * const output_%d[] = {\n", d->code_number);

      for (i = 0, cp = &template[1]; *cp; )
	{
	  while (*cp == '\n' || *cp == ' ' || *cp== '\t')
	    cp++;

	  printf ("  \"");
	  while (*cp != '\n' && *cp != '\0')
	    {
	      putchar (*cp);
	      cp++;
	    }

	  printf ("\",\n");
	  i++;
	}

      printf ("};\n");
    }
  else
    {
      d->template = template;
      d->output_format = INSN_OUTPUT_FORMAT_SINGLE;
    }
}

/* Check insn D for consistency in number of constraint alternatives.  */

static void
validate_insn_alternatives (d)
     struct data *d;
{
  register int n = 0, start;

  /* Make sure all the operands have the same number of alternatives
     in their constraints.  Let N be that number.  */
  for (start = 0; start < d->n_operands; start++)
    if (d->operand[start].n_alternatives > 0)
      {
	if (n == 0)
	  n = d->operand[start].n_alternatives;
	else if (n != d->operand[start].n_alternatives)
	  error ("wrong number of alternatives in operand %d of insn %s",
		 start, get_insn_name (d->index_number));
      }

  /* Record the insn's overall number of alternatives.  */
  d->n_alternatives = n;
}

/* Look at a define_insn just read.  Assign its code number.  Record
   on idata the template and the number of arguments.  If the insn has
   a hairy output action, output a function for now.  */

static void
gen_insn (insn)
     rtx insn;
{
  register struct data *d = (struct data *) xmalloc (sizeof (struct data));
  register int i;

  d->code_number = next_code_number++;
  d->index_number = next_index_number;
  if (XSTR (insn, 0)[0])
    d->name = XSTR (insn, 0);
  else
    d->name = 0;

  /* Build up the list in the same order as the insns are seen
     in the machine description.  */
  d->next = 0;
  *idata_end = d;
  idata_end = &d->next;

  max_opno = -1;
  num_dups = 0;
  memset (d->operand, 0, sizeof (d->operand));

  for (i = 0; i < XVECLEN (insn, 1); i++)
    scan_operands (d, XVECEXP (insn, 1, i), 0, 0);

  d->n_operands = max_opno + 1;
  d->n_dups = num_dups;

  validate_insn_alternatives (d);
  place_operands (d);
  process_template (d, XSTR (insn, 3));
}

/* Look at a define_peephole just read.  Assign its code number.
   Record on idata the template and the number of arguments.
   If the insn has a hairy output action, output it now.  */

static void
gen_peephole (peep)
     rtx peep;
{
  register struct data *d = (struct data *) xmalloc (sizeof (struct data));
  register int i;

  d->code_number = next_code_number++;
  d->index_number = next_index_number;
  d->name = 0;

  /* Build up the list in the same order as the insns are seen
     in the machine description.  */
  d->next = 0;
  *idata_end = d;
  idata_end = &d->next;

  max_opno = -1;
  num_dups = 0;
  memset (d->operand, 0, sizeof (d->operand));

  /* Get the number of operands by scanning all the patterns of the
     peephole optimizer.  But ignore all the rest of the information
     thus obtained.  */
  for (i = 0; i < XVECLEN (peep, 0); i++)
    scan_operands (d, XVECEXP (peep, 0, i), 0, 0);

  d->n_operands = max_opno + 1;
  d->n_dups = 0;

  validate_insn_alternatives (d);
  place_operands (d);
  process_template (d, XSTR (peep, 2));
}

/* Process a define_expand just read.  Assign its code number,
   only for the purposes of `insn_gen_function'.  */

static void
gen_expand (insn)
     rtx insn;
{
  register struct data *d = (struct data *) xmalloc (sizeof (struct data));
  register int i;

  d->code_number = next_code_number++;
  d->index_number = next_index_number;
  if (XSTR (insn, 0)[0])
    d->name = XSTR (insn, 0);
  else
    d->name = 0;

  /* Build up the list in the same order as the insns are seen
     in the machine description.  */
  d->next = 0;
  *idata_end = d;
  idata_end = &d->next;

  max_opno = -1;
  num_dups = 0;
  memset (d->operand, 0, sizeof (d->operand));

  /* Scan the operands to get the specified predicates and modes,
     since expand_binop needs to know them.  */

  if (XVEC (insn, 1))
    for (i = 0; i < XVECLEN (insn, 1); i++)
      scan_operands (d, XVECEXP (insn, 1, i), 0, 0);

  d->n_operands = max_opno + 1;
  d->n_dups = num_dups;
  d->template = 0;
  d->output_format = INSN_OUTPUT_FORMAT_NONE;

  validate_insn_alternatives (d);
  place_operands (d);
}

/* Process a define_split just read.  Assign its code number,
   only for reasons of consistency and to simplify genrecog.  */

static void
gen_split (split)
     rtx split;
{
  register struct data *d = (struct data *) xmalloc (sizeof (struct data));
  register int i;

  d->code_number = next_code_number++;
  d->index_number = next_index_number;
  d->name = 0;

  /* Build up the list in the same order as the insns are seen
     in the machine description.  */
  d->next = 0;
  *idata_end = d;
  idata_end = &d->next;

  max_opno = -1;
  num_dups = 0;
  memset (d->operand, 0, sizeof (d->operand));

  /* Get the number of operands by scanning all the patterns of the
     split patterns.  But ignore all the rest of the information thus
     obtained.  */
  for (i = 0; i < XVECLEN (split, 0); i++)
    scan_operands (d, XVECEXP (split, 0, i), 0, 0);

  d->n_operands = max_opno + 1;
  d->n_dups = 0;
  d->n_alternatives = 0;
  d->template = 0;
  d->output_format = INSN_OUTPUT_FORMAT_NONE;

  place_operands (d);
}

PTR
xmalloc (size)
  size_t size;
{
  register PTR val = (PTR) malloc (size);

  if (val == 0)
    fatal ("virtual memory exhausted");
  return val;
}

PTR
xrealloc (old, size)
  PTR old;
  size_t size;
{
  register PTR ptr;
  if (old)
    ptr = (PTR) realloc (old, size);
  else
    ptr = (PTR) malloc (size);
  if (!ptr)
    fatal ("virtual memory exhausted");
  return ptr;
}

extern int main PARAMS ((int, char **));

int
main (argc, argv)
     int argc;
     char **argv;
{
  rtx desc;
  FILE *infile;
  register int c;

  progname = "genoutput";
  obstack_init (rtl_obstack);

  if (argc <= 1)
    fatal ("No input file name.");

  infile = fopen (argv[1], "r");
  if (infile == 0)
    {
      perror (argv[1]);
      return (FATAL_EXIT_CODE);
    }
  read_rtx_filename = argv[1];

  output_prologue ();
  next_code_number = 0;
  next_index_number = 0;

  /* Read the machine description.  */

  while (1)
    {
      c = read_skip_spaces (infile);
      if (c == EOF)
	break;
      ungetc (c, infile);

      desc = read_rtx (infile);
      if (GET_CODE (desc) == DEFINE_INSN)
	gen_insn (desc);
      if (GET_CODE (desc) == DEFINE_PEEPHOLE)
	gen_peephole (desc);
      if (GET_CODE (desc) == DEFINE_EXPAND)
	gen_expand (desc);
      if (GET_CODE (desc) == DEFINE_SPLIT
 	  || GET_CODE (desc) == DEFINE_PEEPHOLE2)
	gen_split (desc);
      next_index_number++;
    }

  printf("\n\n");
  output_predicate_decls ();
  output_operand_data ();
  output_insn_data ();
  output_get_insn_name ();

  fflush (stdout);
  return (ferror (stdout) != 0 || have_error
	? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
}

static int
n_occurrences (c, s)
     int c;
     char *s;
{
  int n = 0;
  while (*s)
    n += (*s++ == c);
  return n;
}
