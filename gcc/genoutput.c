/* Generate code from to output assembler insns as recognized from rtl.
   Copyright (C) 1987-2019 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


/* This program reads the machine description for the compiler target machine
   and produces a file containing these things:

   1. An array of `struct insn_data_d', which is indexed by insn code number,
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

     b. `constraint' is the constraint for this operand.

     c. `address_p' indicates that the operand appears within ADDRESS
     rtx's.

     d. `mode' is the machine mode that that operand is supposed to have.

     e. `strict_low', is nonzero for operands contained in a STRICT_LOW_PART.

     f. `eliminable', is nonzero for operands that are matched normally by
     MATCH_OPERAND; it is zero for operands that should not be changed during
     register elimination such as MATCH_OPERATORs.

     g. `allows_mem', is true for operands that accept MEM rtxes.

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

#include "bconfig.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "errors.h"
#include "read-md.h"
#include "gensupport.h"

/* No instruction can have more operands than this.  Sorry for this
   arbitrary limit, but what machine will have an instruction with
   this many operands?  */

#define MAX_MAX_OPERANDS 40

static char general_mem[] = { TARGET_MEM_CONSTRAINT, 0 };

static int n_occurrences		(int, const char *);
static const char *strip_whitespace	(const char *);

/* This counts all operands used in the md file.  The first is null.  */

static int next_operand_number = 1;

/* Record in this chain all information about the operands we will output.  */

struct operand_data
{
  struct operand_data *next;
  int index;
  const char *predicate;
  const char *constraint;
  machine_mode mode;
  unsigned char n_alternatives;
  char address_p;
  char strict_low;
  char eliminable;
  char seen;
};

/* Begin with a null operand at index 0.  */

static struct operand_data null_operand =
{
  0, 0, "", "", E_VOIDmode, 0, 0, 0, 0, 0
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

class data
{
public:
  class data *next;
  const char *name;
  const char *template_code;
  file_location loc;
  int code_number;
  int n_generator_args;		/* Number of arguments passed to generator */
  int n_operands;		/* Number of operands this insn recognizes */
  int n_dups;			/* Number times match_dup appears in pattern */
  int n_alternatives;		/* Number of alternatives in each constraint */
  int operand_number;		/* Operand index in the big array.  */
  int output_format;		/* INSN_OUTPUT_FORMAT_*.  */
  struct operand_data operand[MAX_MAX_OPERANDS];
};

/* This variable points to the first link in the insn chain.  */
static class data *idata;

/* This variable points to the end of the insn chain.  This is where
   everything relevant from the machien description is appended to.  */
static class data **idata_end;


static void output_prologue (void);
static void output_operand_data (void);
static void output_insn_data (void);
static void output_get_insn_name (void);
static void scan_operands (class data *, rtx, int, int);
static int compare_operands (struct operand_data *,
			     struct operand_data *);
static void place_operands (class data *);
static void process_template (class data *, const char *);
static void validate_insn_alternatives (class data *);
static void validate_insn_operands (class data *);

class constraint_data
{
public:
  class constraint_data *next_this_letter;
  file_location loc;
  unsigned int namelen;
  char name[1];
};

/* All machine-independent constraint characters (except digits) that
   are handled outside the define*_constraint mechanism.  */
static const char indep_constraints[] = ",=+%*?!^$#&g";

static class constraint_data *
constraints_by_letter_table[1 << CHAR_BIT];

static int mdep_constraint_len (const char *, file_location, int);
static void note_constraint (md_rtx_info *);

static void
output_prologue (void)
{
  printf ("/* Generated automatically by the program `genoutput'\n\
   from the machine description file `md'.  */\n\n");

  printf ("#define IN_TARGET_CODE 1\n");
  printf ("#include \"config.h\"\n");
  printf ("#include \"system.h\"\n");
  printf ("#include \"coretypes.h\"\n");
  printf ("#include \"backend.h\"\n");
  printf ("#include \"predict.h\"\n");
  printf ("#include \"tree.h\"\n");
  printf ("#include \"rtl.h\"\n");
  printf ("#include \"flags.h\"\n");
  printf ("#include \"alias.h\"\n");
  printf ("#include \"varasm.h\"\n");
  printf ("#include \"stor-layout.h\"\n");
  printf ("#include \"calls.h\"\n");
  printf ("#include \"insn-config.h\"\n");
  printf ("#include \"expmed.h\"\n");
  printf ("#include \"dojump.h\"\n");
  printf ("#include \"explow.h\"\n");
  printf ("#include \"memmodel.h\"\n");
  printf ("#include \"emit-rtl.h\"\n");
  printf ("#include \"stmt.h\"\n");
  printf ("#include \"expr.h\"\n");
  printf ("#include \"insn-codes.h\"\n");
  printf ("#include \"tm_p.h\"\n");
  printf ("#include \"regs.h\"\n");
  printf ("#include \"conditions.h\"\n");
  printf ("#include \"insn-attr.h\"\n\n");
  printf ("#include \"recog.h\"\n\n");
  printf ("#include \"diagnostic-core.h\"\n");
  printf ("#include \"output.h\"\n");
  printf ("#include \"target.h\"\n");
  printf ("#include \"tm-constrs.h\"\n");
}

static void
output_operand_data (void)
{
  struct operand_data *d;

  printf ("\nstatic const struct insn_operand_data operand_data[] = \n{\n");

  for (d = odata; d; d = d->next)
    {
      struct pred_data *pred;

      printf ("  {\n");

      printf ("    %s,\n",
	      d->predicate && d->predicate[0] ? d->predicate : "0");

      printf ("    \"%s\",\n", d->constraint ? d->constraint : "");

      printf ("    E_%smode,\n", GET_MODE_NAME (d->mode));

      printf ("    %d,\n", d->strict_low);

      printf ("    %d,\n", d->constraint == NULL ? 1 : 0);

      printf ("    %d,\n", d->eliminable);

      pred = NULL;
      if (d->predicate)
	pred = lookup_predicate (d->predicate);
      printf ("    %d\n", pred && pred->codes[MEM]);

      printf ("  },\n");
    }
  printf ("};\n\n\n");
}

static void
output_insn_data (void)
{
  class data *d;
  int name_offset = 0;
  int next_name_offset;
  const char * last_name = 0;
  const char * next_name = 0;
  class data *n;

  for (n = idata, next_name_offset = 1; n; n = n->next, next_name_offset++)
    if (n->name)
      {
	next_name = n->name;
	break;
      }

  printf ("#if GCC_VERSION >= 2007\n__extension__\n#endif\n");
  printf ("\nconst struct insn_data_d insn_data[] = \n{\n");

  for (d = idata; d; d = d->next)
    {
      printf ("  /* %s:%d */\n", d->loc.filename, d->loc.lineno);
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
	  printf ("#if HAVE_DESIGNATED_UNION_INITIALIZERS\n");
	  printf ("    { 0 },\n");
	  printf ("#else\n");
	  printf ("    { 0, 0, 0 },\n");
	  printf ("#endif\n");
	  break;
	case INSN_OUTPUT_FORMAT_SINGLE:
	  {
	    const char *p = d->template_code;
	    char prev = 0;

	    printf ("#if HAVE_DESIGNATED_UNION_INITIALIZERS\n");
	    printf ("    { .single =\n");
	    printf ("#else\n");
	    printf ("    {\n");
	    printf ("#endif\n");
	    printf ("    \"");
	    while (*p)
	      {
		if (IS_VSPACE (*p) && prev != '\\')
		  {
		    /* Preserve two consecutive \n's or \r's, but treat \r\n
		       as a single newline.  */
		    if (*p == '\n' && prev != '\r')
		      printf ("\\n\\\n");
		  }
		else
		  putchar (*p);
		prev = *p;
		++p;
	      }
	    printf ("\",\n");
	    printf ("#if HAVE_DESIGNATED_UNION_INITIALIZERS\n");
	    printf ("    },\n");
	    printf ("#else\n");
	    printf ("    0, 0 },\n");
	    printf ("#endif\n");
	  }
	  break;
	case INSN_OUTPUT_FORMAT_MULTI:
	  printf ("#if HAVE_DESIGNATED_UNION_INITIALIZERS\n");
	  printf ("    { .multi = output_%d },\n", d->code_number);
	  printf ("#else\n");
	  printf ("    { 0, output_%d, 0 },\n", d->code_number);
	  printf ("#endif\n");
	  break;
	case INSN_OUTPUT_FORMAT_FUNCTION:
	  printf ("#if HAVE_DESIGNATED_UNION_INITIALIZERS\n");
	  printf ("    { .function = output_%d },\n", d->code_number);
	  printf ("#else\n");
	  printf ("    { 0, 0, output_%d },\n", d->code_number);
	  printf ("#endif\n");
	  break;
	default:
	  gcc_unreachable ();
	}

      if (d->name && d->name[0] != '*')
	printf ("    { (insn_gen_fn::stored_funcptr) gen_%s },\n", d->name);
      else
	printf ("    { 0 },\n");

      printf ("    &operand_data[%d],\n", d->operand_number);
      printf ("    %d,\n", d->n_generator_args);
      printf ("    %d,\n", d->n_operands);
      printf ("    %d,\n", d->n_dups);
      printf ("    %d,\n", d->n_alternatives);
      printf ("    %d\n", d->output_format);

      printf ("  },\n");
    }
  printf ("};\n\n\n");
}

static void
output_get_insn_name (void)
{
  printf ("const char *\n");
  printf ("get_insn_name (int code)\n");
  printf ("{\n");
  printf ("  if (code == NOOP_MOVE_INSN_CODE)\n");
  printf ("    return \"NOOP_MOVE\";\n");
  printf ("  else\n");
  printf ("    return insn_data[code].name;\n");
  printf ("}\n");
}


/* Stores the operand data into `d->operand[i]'.

   THIS_ADDRESS_P is nonzero if the containing rtx was an ADDRESS.
   THIS_STRICT_LOW is nonzero if the containing rtx was a STRICT_LOW_PART.  */

static void
scan_operands (class data *d, rtx part, int this_address_p,
	       int this_strict_low)
{
  int i, j;
  const char *format_ptr;
  int opno;

  if (part == 0)
    return;

  switch (GET_CODE (part))
    {
    case MATCH_OPERAND:
      opno = XINT (part, 0);
      if (opno >= MAX_MAX_OPERANDS)
	{
	  error_at (d->loc, "maximum number of operands exceeded");
	  return;
	}
      if (d->operand[opno].seen)
	error_at (d->loc, "repeated operand number %d\n", opno);

      d->operand[opno].seen = 1;
      d->operand[opno].mode = GET_MODE (part);
      d->operand[opno].strict_low = this_strict_low;
      d->operand[opno].predicate = XSTR (part, 1);
      d->operand[opno].constraint = strip_whitespace (XSTR (part, 2));
      d->operand[opno].n_alternatives
	= n_occurrences (',', d->operand[opno].constraint) + 1;
      d->operand[opno].address_p = this_address_p;
      d->operand[opno].eliminable = 1;
      return;

    case MATCH_SCRATCH:
      opno = XINT (part, 0);
      if (opno >= MAX_MAX_OPERANDS)
	{
	  error_at (d->loc, "maximum number of operands exceeded");
	  return;
	}
      if (d->operand[opno].seen)
	error_at (d->loc, "repeated operand number %d\n", opno);

      d->operand[opno].seen = 1;
      d->operand[opno].mode = GET_MODE (part);
      d->operand[opno].strict_low = 0;
      d->operand[opno].predicate = "scratch_operand";
      d->operand[opno].constraint = strip_whitespace (XSTR (part, 1));
      d->operand[opno].n_alternatives
	= n_occurrences (',', d->operand[opno].constraint) + 1;
      d->operand[opno].address_p = 0;
      d->operand[opno].eliminable = 0;
      return;

    case MATCH_OPERATOR:
    case MATCH_PARALLEL:
      opno = XINT (part, 0);
      if (opno >= MAX_MAX_OPERANDS)
	{
	  error_at (d->loc, "maximum number of operands exceeded");
	  return;
	}
      if (d->operand[opno].seen)
	error_at (d->loc, "repeated operand number %d\n", opno);

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
compare_operands (struct operand_data *d0, struct operand_data *d1)
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
place_operands (class data *d)
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
process_template (class data *d, const char *template_code)
{
  const char *cp;
  int i;

  /* Templates starting with * contain straight code to be run.  */
  if (template_code[0] == '*')
    {
      d->template_code = 0;
      d->output_format = INSN_OUTPUT_FORMAT_FUNCTION;

      puts ("\nstatic const char *");
      printf ("output_%d (rtx *operands ATTRIBUTE_UNUSED, rtx_insn *insn ATTRIBUTE_UNUSED)\n",
	      d->code_number);
      puts ("{");
      rtx_reader_ptr->print_md_ptr_loc (template_code);
      puts (template_code + 1);
      puts ("}");
    }

  /* If the assembler code template starts with a @ it is a newline-separated
     list of assembler code templates, one for each alternative.  */
  else if (template_code[0] == '@')
    {
      int found_star = 0;

      for (cp = &template_code[1]; *cp; )
	{
	  while (ISSPACE (*cp))
	    cp++;
	  if (*cp == '*')
	    found_star = 1;
	  while (!IS_VSPACE (*cp) && *cp != '\0')
	    ++cp;
	}
      d->template_code = 0;
      if (found_star)
	{
	  d->output_format = INSN_OUTPUT_FORMAT_FUNCTION;
	  puts ("\nstatic const char *");
	  printf ("output_%d (rtx *operands ATTRIBUTE_UNUSED, "
		  "rtx_insn *insn ATTRIBUTE_UNUSED)\n", d->code_number);
	  puts ("{");
	  puts ("  switch (which_alternative)\n    {");
	}
      else
	{
	  d->output_format = INSN_OUTPUT_FORMAT_MULTI;
	  printf ("\nstatic const char * const output_%d[] = {\n",
		  d->code_number);
	}

      for (i = 0, cp = &template_code[1]; *cp; )
	{
	  const char *ep, *sp, *bp;

	  while (ISSPACE (*cp))
	    cp++;

	  bp = cp;
	  if (found_star)
	    {
	      printf ("    case %d:", i);
	      if (*cp == '*')
		{
		  printf ("\n      ");
		  cp++;
		}
	      else
		printf (" return \"");
	    }
	  else
	    printf ("  \"");

	  for (ep = sp = cp; !IS_VSPACE (*ep) && *ep != '\0'; ++ep)
	    if (!ISSPACE (*ep))
	      sp = ep + 1;

	  if (sp != ep)
	    message_at (d->loc, "trailing whitespace in output template");

	  while (cp < sp)
	    {
	      putchar (*cp);
	      cp++;
	    }

	  if (!found_star)
	    puts ("\",");
	  else if (*bp != '*')
	    puts ("\";");
	  else
	    {
	      /* The usual action will end with a return.
		 If there is neither break or return at the end, this is
		 assumed to be intentional; this allows to have multiple
		 consecutive alternatives share some code.  */
	      puts ("");
	    }
	  i++;
	}
      if (i == 1)
	message_at (d->loc, "'@' is redundant for output template with"
		    " single alternative");
      if (i != d->n_alternatives)
	error_at (d->loc, "wrong number of alternatives in the output"
		  " template");

      if (found_star)
	puts ("      default: gcc_unreachable ();\n    }\n}");
      else
	printf ("};\n");
    }
  else
    {
      d->template_code = template_code;
      d->output_format = INSN_OUTPUT_FORMAT_SINGLE;
    }
}

/* Check insn D for consistency in number of constraint alternatives.  */

static void
validate_insn_alternatives (class data *d)
{
  int n = 0, start;

  /* Make sure all the operands have the same number of alternatives
     in their constraints.  Let N be that number.  */
  for (start = 0; start < d->n_operands; start++)
    if (d->operand[start].n_alternatives > 0)
      {
	int len, i;
	const char *p;
	char c;
	int which_alternative = 0;
	int alternative_count_unsure = 0;
	bool seen_write = false;

	for (p = d->operand[start].constraint; (c = *p); p += len)
	  {
	    if ((c == '%' || c == '=' || c == '+')
		&& p != d->operand[start].constraint)
	      error_at (d->loc, "character '%c' can only be used at the"
			" beginning of a constraint string", c);

	    if (c == '=' || c == '+')
	      seen_write = true;

	    /* Earlyclobber operands must always be marked write-only
	       or read/write.  */
	    if (!seen_write && c == '&')
	      error_at (d->loc, "earlyclobber operands may not be"
			" read-only in alternative %d", which_alternative);

	    if (ISSPACE (c) || strchr (indep_constraints, c))
	      len = 1;
	    else if (ISDIGIT (c))
	      {
		const char *q = p;
		do
		  q++;
		while (ISDIGIT (*q));
		len = q - p;
	      }
	    else
	      len = mdep_constraint_len (p, d->loc, start);

	    if (c == ',')
	      {
	        which_alternative++;
		continue;
	      }

	    for (i = 1; i < len; i++)
	      if (p[i] == '\0')
		{
		  error_at (d->loc, "NUL in alternative %d of operand %d",
			    which_alternative, start);
		  alternative_count_unsure = 1;
		  break;
		}
	      else if (strchr (",#*", p[i]))
		{
		  error_at (d->loc, "'%c' in alternative %d of operand %d",
			    p[i], which_alternative, start);
		  alternative_count_unsure = 1;
		}
	  }
	if (!alternative_count_unsure)
	  {
	    if (n == 0)
	      n = d->operand[start].n_alternatives;
	    else if (n != d->operand[start].n_alternatives)
	      error_at (d->loc, "wrong number of alternatives in operand %d",
			start);
	  }
      }

  /* Record the insn's overall number of alternatives.  */
  d->n_alternatives = n;
}

/* Verify that there are no gaps in operand numbers for INSNs.  */

static void
validate_insn_operands (class data *d)
{
  int i;

  for (i = 0; i < d->n_operands; ++i)
    if (d->operand[i].seen == 0)
      error_at (d->loc, "missing operand %d", i);
}

static void
validate_optab_operands (class data *d)
{
  if (!d->name || d->name[0] == '\0' || d->name[0] == '*')
    return;

  /* Miscellaneous tests.  */
  if (strncmp (d->name, "cstore", 6) == 0
      && d->name[strlen (d->name) - 1] == '4'
      && d->operand[0].mode == VOIDmode)
    {
      message_at (d->loc, "missing mode for operand 0 of cstore");
      have_error = 1;
    }
}

/* Look at a define_insn just read.  Assign its code number.  Record
   on idata the template and the number of arguments.  If the insn has
   a hairy output action, output a function for now.  */

static void
gen_insn (md_rtx_info *info)
{
  struct pattern_stats stats;
  rtx insn = info->def;
  data *d = new data;
  int i;

  d->code_number = info->index;
  d->loc = info->loc;
  if (XSTR (insn, 0)[0])
    d->name = XSTR (insn, 0);
  else
    d->name = 0;

  /* Build up the list in the same order as the insns are seen
     in the machine description.  */
  d->next = 0;
  *idata_end = d;
  idata_end = &d->next;

  memset (d->operand, 0, sizeof (d->operand));

  for (i = 0; i < XVECLEN (insn, 1); i++)
    scan_operands (d, XVECEXP (insn, 1, i), 0, 0);

  get_pattern_stats (&stats, XVEC (insn, 1));
  d->n_generator_args = stats.num_generator_args;
  d->n_operands = stats.num_insn_operands;
  d->n_dups = stats.num_dups;

  validate_insn_operands (d);
  validate_insn_alternatives (d);
  validate_optab_operands (d);
  place_operands (d);
  process_template (d, XTMPL (insn, 3));
}

/* Look at a define_peephole just read.  Assign its code number.
   Record on idata the template and the number of arguments.
   If the insn has a hairy output action, output it now.  */

static void
gen_peephole (md_rtx_info *info)
{
  struct pattern_stats stats;
  data *d = new data;
  int i;

  d->code_number = info->index;
  d->loc = info->loc;
  d->name = 0;

  /* Build up the list in the same order as the insns are seen
     in the machine description.  */
  d->next = 0;
  *idata_end = d;
  idata_end = &d->next;

  memset (d->operand, 0, sizeof (d->operand));

  /* Get the number of operands by scanning all the patterns of the
     peephole optimizer.  But ignore all the rest of the information
     thus obtained.  */
  rtx peep = info->def;
  for (i = 0; i < XVECLEN (peep, 0); i++)
    scan_operands (d, XVECEXP (peep, 0, i), 0, 0);

  get_pattern_stats (&stats, XVEC (peep, 0));
  d->n_generator_args = 0;
  d->n_operands = stats.num_insn_operands;
  d->n_dups = 0;

  validate_insn_alternatives (d);
  place_operands (d);
  process_template (d, XTMPL (peep, 2));
}

/* Process a define_expand just read.  Assign its code number,
   only for the purposes of `insn_gen_function'.  */

static void
gen_expand (md_rtx_info *info)
{
  struct pattern_stats stats;
  rtx insn = info->def;
  data *d = new data;
  int i;

  d->code_number = info->index;
  d->loc = info->loc;
  if (XSTR (insn, 0)[0])
    d->name = XSTR (insn, 0);
  else
    d->name = 0;

  /* Build up the list in the same order as the insns are seen
     in the machine description.  */
  d->next = 0;
  *idata_end = d;
  idata_end = &d->next;

  memset (d->operand, 0, sizeof (d->operand));

  /* Scan the operands to get the specified predicates and modes,
     since expand_binop needs to know them.  */

  if (XVEC (insn, 1))
    for (i = 0; i < XVECLEN (insn, 1); i++)
      scan_operands (d, XVECEXP (insn, 1, i), 0, 0);

  get_pattern_stats (&stats, XVEC (insn, 1));
  d->n_generator_args = stats.num_generator_args;
  d->n_operands = stats.num_insn_operands;
  d->n_dups = stats.num_dups;
  d->template_code = 0;
  d->output_format = INSN_OUTPUT_FORMAT_NONE;

  validate_insn_alternatives (d);
  validate_optab_operands (d);
  place_operands (d);
}

static void
init_insn_for_nothing (void)
{
  idata = XCNEW (class data);
  new (idata) data ();
  idata->name = "*placeholder_for_nothing";
  idata->loc = file_location ("<internal>", 0, 0);
  idata_end = &idata->next;
}

extern int main (int, const char **);

int
main (int argc, const char **argv)
{
  progname = "genoutput";

  init_insn_for_nothing ();

  if (!init_rtx_reader_args (argc, argv))
    return (FATAL_EXIT_CODE);

  output_prologue ();

  /* Read the machine description.  */

  md_rtx_info info;
  while (read_md_rtx (&info))
    switch (GET_CODE (info.def))
      {
      case DEFINE_INSN:
	gen_insn (&info);
	break;

      case DEFINE_PEEPHOLE:
	gen_peephole (&info);
	break;

      case DEFINE_EXPAND:
	gen_expand (&info);
	break;

      case DEFINE_CONSTRAINT:
      case DEFINE_REGISTER_CONSTRAINT:
      case DEFINE_ADDRESS_CONSTRAINT:
      case DEFINE_MEMORY_CONSTRAINT:
      case DEFINE_SPECIAL_MEMORY_CONSTRAINT:
	note_constraint (&info);
	break;

      default:
	break;
      }

  printf ("\n\n");
  output_operand_data ();
  output_insn_data ();
  output_get_insn_name ();

  fflush (stdout);
  return (ferror (stdout) != 0 || have_error
	? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
}

/* Return the number of occurrences of character C in string S or
   -1 if S is the null string.  */

static int
n_occurrences (int c, const char *s)
{
  int n = 0;

  if (s == 0 || *s == '\0')
    return -1;

  while (*s)
    n += (*s++ == c);

  return n;
}

/* Remove whitespace in `s' by moving up characters until the end.
   Return a new string.  */

static const char *
strip_whitespace (const char *s)
{
  char *p, *q;
  char ch;

  if (s == 0)
    return 0;

  p = q = XNEWVEC (char, strlen (s) + 1);
  while ((ch = *s++) != '\0')
    if (! ISSPACE (ch))
      *p++ = ch;

  *p = '\0';
  return q;
}

/* Record just enough information about the constraint in *INFO to allow
   checking of operand constraint strings above, in validate_insn_alternatives.
   Does not validate most properties of the constraint itself; does enforce
   no duplicate names, no overlap with MI constraints, and no prefixes.  */
static void
note_constraint (md_rtx_info *info)
{
  rtx exp = info->def;
  const char *name = XSTR (exp, 0);
  class constraint_data **iter, **slot, *new_cdata;

  if (strcmp (name, "TARGET_MEM_CONSTRAINT") == 0)
    name = general_mem;
  unsigned int namelen = strlen (name);

  if (strchr (indep_constraints, name[0]))
    {
      if (name[1] == '\0')
	error_at (info->loc, "constraint letter '%s' cannot be "
		  "redefined by the machine description", name);
      else
	error_at (info->loc, "constraint name '%s' cannot be defined by "
		  "the machine description, as it begins with '%c'",
		  name, name[0]);
      return;
    }

  slot = &constraints_by_letter_table[(unsigned int)name[0]];
  for (iter = slot; *iter; iter = &(*iter)->next_this_letter)
    {
      /* This causes slot to end up pointing to the
	 next_this_letter field of the last constraint with a name
	 of equal or greater length than the new constraint; hence
	 the new constraint will be inserted after all previous
	 constraints with names of the same length.  */
      if ((*iter)->namelen >= namelen)
	slot = iter;

      if (!strcmp ((*iter)->name, name))
	{
	  error_at (info->loc, "redefinition of constraint '%s'", name);
	  message_at ((*iter)->loc, "previous definition is here");
	  return;
	}
      else if (!strncmp ((*iter)->name, name, (*iter)->namelen))
	{
	  error_at (info->loc, "defining constraint '%s' here", name);
	  message_at ((*iter)->loc, "renders constraint '%s' "
		      "(defined here) a prefix", (*iter)->name);
	  return;
	}
      else if (!strncmp ((*iter)->name, name, namelen))
	{
	  error_at (info->loc, "constraint '%s' is a prefix", name);
	  message_at ((*iter)->loc, "of constraint '%s' "
		      "(defined here)", (*iter)->name);
	  return;
	}
    }
  new_cdata = XNEWVAR (class constraint_data,
		       sizeof (class constraint_data) + namelen);
  new (new_cdata) constraint_data ();
  strcpy (CONST_CAST (char *, new_cdata->name), name);
  new_cdata->namelen = namelen;
  new_cdata->loc = info->loc;
  new_cdata->next_this_letter = *slot;
  *slot = new_cdata;
}

/* Return the length of the constraint name beginning at position S
   of an operand constraint string, or issue an error message if there
   is no such constraint.  Does not expect to be called for generic
   constraints.  */
static int
mdep_constraint_len (const char *s, file_location loc, int opno)
{
  class constraint_data *p;

  p = constraints_by_letter_table[(unsigned int)s[0]];

  if (p)
    for (; p; p = p->next_this_letter)
      if (!strncmp (s, p->name, p->namelen))
	return p->namelen;

  error_at (loc, "error: undefined machine-specific constraint "
	    "at this point: \"%s\"", s);
  message_at (loc, "note:  in operand %d", opno);
  return 1; /* safe */
}
