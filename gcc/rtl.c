/* Allocate and read RTL for GNU C Compiler.
   Copyright (C) 1987, 1988, 1991, 1994, 1997, 1998, 1999, 2000
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
#include "rtl.h"
#include "real.h"
#include "bitmap.h"
#include "ggc.h"
#include "obstack.h"
#include "toplev.h"

#define	obstack_chunk_alloc	xmalloc
#define	obstack_chunk_free	free

/* Obstack used for allocating RTL objects.
   Between functions, this is the permanent_obstack.
   While parsing and expanding a function, this is maybepermanent_obstack
   so we can save it if it is an inline function.
   During optimization and output, this is function_obstack.  */

extern struct obstack *rtl_obstack;

/* Calculate the format for CONST_DOUBLE.  This depends on the relative
   widths of HOST_WIDE_INT and REAL_VALUE_TYPE.

   We need to go out to e0wwwww, since REAL_ARITHMETIC assumes 16-bits
   per element in REAL_VALUE_TYPE.

   This is duplicated in gengenrtl.c.

   A number of places assume that there are always at least two 'w'
   slots in a CONST_DOUBLE, so we provide them even if one would suffice.  */

#ifdef REAL_ARITHMETIC
#if MAX_LONG_DOUBLE_TYPE_SIZE == 96
#define REAL_WIDTH	(11*8 + HOST_BITS_PER_WIDE_INT)/HOST_BITS_PER_WIDE_INT
#elif MAX_LONG_DOUBLE_TYPE_SIZE == 128
#define REAL_WIDTH	(19*8 + HOST_BITS_PER_WIDE_INT)/HOST_BITS_PER_WIDE_INT
#elif HOST_FLOAT_FORMAT != TARGET_FLOAT_FORMAT
#define REAL_WIDTH	(7*8 + HOST_BITS_PER_WIDE_INT)/HOST_BITS_PER_WIDE_INT
#endif
#endif /* REAL_ARITHMETIC */

#ifndef REAL_WIDTH
#if HOST_BITS_PER_WIDE_INT*2 >= MAX_LONG_DOUBLE_TYPE_SIZE
#define REAL_WIDTH	2
#elif HOST_BITS_PER_WIDE_INT*3 >= MAX_LONG_DOUBLE_TYPE_SIZE
#define REAL_WIDTH	3
#elif HOST_BITS_PER_WIDE_INT*4 >= MAX_LONG_DOUBLE_TYPE_SIZE
#define REAL_WIDTH	4
#endif
#endif /* REAL_WIDTH */

#if REAL_WIDTH == 1
#define CONST_DOUBLE_FORMAT	"e0ww"
#elif REAL_WIDTH == 2
#define CONST_DOUBLE_FORMAT	"e0ww"
#elif REAL_WIDTH == 3
#define CONST_DOUBLE_FORMAT	"e0www"
#elif REAL_WIDTH == 4
#define CONST_DOUBLE_FORMAT	"e0wwww"
#elif REAL_WIDTH == 5
#define CONST_DOUBLE_FORMAT	"e0wwwww"
#else
#define CONST_DOUBLE_FORMAT	/* nothing - will cause syntax error */
#endif

/* Indexed by rtx code, gives number of operands for an rtx with that code.
   Does NOT include rtx header data (code and links).  */

#define DEF_RTL_EXPR(ENUM, NAME, FORMAT, CLASS)   sizeof FORMAT - 1 ,

const int rtx_length[NUM_RTX_CODE + 1] = {
#include "rtl.def"
};

#undef DEF_RTL_EXPR

/* Indexed by rtx code, gives the name of that kind of rtx, as a C string.  */

#define DEF_RTL_EXPR(ENUM, NAME, FORMAT, CLASS)   NAME ,

const char * const rtx_name[] = {
#include "rtl.def"		/* rtl expressions are documented here */
};

#undef DEF_RTL_EXPR

/* Indexed by machine mode, gives the name of that machine mode.
   This name does not include the letters "mode".  */

#define DEF_MACHMODE(SYM, NAME, CLASS, SIZE, UNIT, WIDER)  NAME,

const char * const mode_name[(int) MAX_MACHINE_MODE + 1] = {
#include "machmode.def"
  /* Add an extra field to avoid a core dump if someone tries to convert
     MAX_MACHINE_MODE to a string.   */
  ""
};

#undef DEF_MACHMODE

/* Indexed by machine mode, gives the length of the mode, in bytes.
   GET_MODE_CLASS uses this.  */

#define DEF_MACHMODE(SYM, NAME, CLASS, SIZE, UNIT, WIDER)  CLASS,

const enum mode_class mode_class[(int) MAX_MACHINE_MODE] = {
#include "machmode.def"
};

#undef DEF_MACHMODE

/* Indexed by machine mode, gives the length of the mode, in bytes.
   GET_MODE_SIZE uses this.  */

#define DEF_MACHMODE(SYM, NAME, CLASS, SIZE, UNIT, WIDER)  SIZE,

const int mode_size[(int) MAX_MACHINE_MODE] = {
#include "machmode.def"
};

#undef DEF_MACHMODE

/* Indexed by machine mode, gives the length of the mode's subunit.
   GET_MODE_UNIT_SIZE uses this.  */

#define DEF_MACHMODE(SYM, NAME, CLASS, SIZE, UNIT, WIDER)  UNIT,

const int mode_unit_size[(int) MAX_MACHINE_MODE] = {
#include "machmode.def"		/* machine modes are documented here */
};

#undef DEF_MACHMODE

/* Indexed by machine mode, gives next wider natural mode
   (QI -> HI -> SI -> DI, etc.)  Widening multiply instructions
   use this.  */

#define DEF_MACHMODE(SYM, NAME, CLASS, SIZE, UNIT, WIDER)  \
  (unsigned char) WIDER,

const unsigned char mode_wider_mode[(int) MAX_MACHINE_MODE] = {
#include "machmode.def"		/* machine modes are documented here */
};

#undef DEF_MACHMODE

#define DEF_MACHMODE(SYM, NAME, CLASS, SIZE, UNIT, WIDER)  \
  ((SIZE) * BITS_PER_UNIT >= HOST_BITS_PER_WIDE_INT) ? ~(unsigned HOST_WIDE_INT)0 : ((unsigned HOST_WIDE_INT) 1 << (SIZE) * BITS_PER_UNIT) - 1,

/* Indexed by machine mode, gives mask of significant bits in mode.  */

const unsigned HOST_WIDE_INT mode_mask_array[(int) MAX_MACHINE_MODE] = {
#include "machmode.def"
};

/* Indexed by mode class, gives the narrowest mode for each class.
   The Q modes are always of width 1 (2 for complex) - it is impossible
   for any mode to be narrower.  */

const enum machine_mode class_narrowest_mode[(int) MAX_MODE_CLASS] = {
    /* MODE_RANDOM */		VOIDmode,
    /* MODE_INT */		QImode,
    /* MODE_FLOAT */		QFmode,
    /* MODE_PARTIAL_INT */	PQImode,
    /* MODE_CC */		CCmode,
    /* MODE_COMPLEX_INT */	CQImode,
    /* MODE_COMPLEX_FLOAT */	QCmode
};
			

/* Indexed by rtx code, gives a sequence of operand-types for
   rtx's of that code.  The sequence is a C string in which
   each character describes one operand.  */

const char * const rtx_format[] = {
  /* "*" undefined.
         can cause a warning message
     "0" field is unused (or used in a phase-dependent manner)
         prints nothing
     "i" an integer
         prints the integer
     "n" like "i", but prints entries from `note_insn_name'
     "w" an integer of width HOST_BITS_PER_WIDE_INT
         prints the integer
     "s" a pointer to a string
         prints the string
     "S" like "s", but optional:
	 the containing rtx may end before this operand
     "e" a pointer to an rtl expression
         prints the expression
     "E" a pointer to a vector that points to a number of rtl expressions
         prints a list of the rtl expressions
     "V" like "E", but optional:
	 the containing rtx may end before this operand
     "u" a pointer to another insn
         prints the uid of the insn.
     "b" is a pointer to a bitmap header.
     "t" is a tree pointer. */

#define DEF_RTL_EXPR(ENUM, NAME, FORMAT, CLASS)   FORMAT ,
#include "rtl.def"		/* rtl expressions are defined here */
#undef DEF_RTL_EXPR
};

/* Indexed by rtx code, gives a character representing the "class" of
   that rtx code.  See rtl.def for documentation on the defined classes.  */

const char rtx_class[] = {
#define DEF_RTL_EXPR(ENUM, NAME, FORMAT, CLASS)   CLASS, 
#include "rtl.def"		/* rtl expressions are defined here */
#undef DEF_RTL_EXPR
};

/* Names for kinds of NOTEs and REG_NOTEs.  */

const char * const note_insn_name[] = { 0       , "NOTE_INSN_DELETED",
			   "NOTE_INSN_BLOCK_BEG", "NOTE_INSN_BLOCK_END",
			   "NOTE_INSN_LOOP_BEG", "NOTE_INSN_LOOP_END",
			   "NOTE_INSN_FUNCTION_END", "NOTE_INSN_SETJMP",
			   "NOTE_INSN_LOOP_CONT", "NOTE_INSN_LOOP_VTOP",
			   "NOTE_INSN_PROLOGUE_END", "NOTE_INSN_EPILOGUE_BEG",
			   "NOTE_INSN_DELETED_LABEL", "NOTE_INSN_FUNCTION_BEG",
			   "NOTE_INSN_EH_REGION_BEG", "NOTE_INSN_EH_REGION_END",
			   "NOTE_REPEATED_LINE_NUMBER", "NOTE_INSN_RANGE_START",
			   "NOTE_INSN_RANGE_END", "NOTE_INSN_LIVE",
			   "NOTE_INSN_BASIC_BLOCK" };

const char * const reg_note_name[] = { "", "REG_DEAD", "REG_INC", "REG_EQUIV", "REG_WAS_0",
			  "REG_EQUAL", "REG_RETVAL", "REG_LIBCALL",
			  "REG_NONNEG", "REG_NO_CONFLICT", "REG_UNUSED",
			  "REG_CC_SETTER", "REG_CC_USER", "REG_LABEL",
			  "REG_DEP_ANTI", "REG_DEP_OUTPUT", "REG_BR_PROB",
			  "REG_EXEC_COUNT", "REG_NOALIAS", "REG_SAVE_AREA",
			  "REG_BR_PRED", "REG_EH_CONTEXT",
			  "REG_FRAME_RELATED_EXPR", "REG_EH_REGION",
			  "REG_EH_RETHROW", "REG_SAVE_NOTE" };

static void fatal_with_file_and_line PARAMS ((FILE *, const char *, ...))
  ATTRIBUTE_PRINTF_2 ATTRIBUTE_NORETURN;
static void fatal_expected_char PARAMS ((FILE *, int, int)) ATTRIBUTE_NORETURN;
static void read_name		PARAMS ((char *, FILE *));
static const char *trim_filename PARAMS ((const char *));

/* Allocate an rtx vector of N elements.
   Store the length, and initialize all elements to zero.  */

rtvec
rtvec_alloc (n)
     int n;
{
  rtvec rt;
 
  if (ggc_p)
    rt = ggc_alloc_rtvec (n);
  else
    {
      int i;

      rt = (rtvec) obstack_alloc (rtl_obstack,
				  sizeof (struct rtvec_def)
				  + (( n - 1) * sizeof (rtx)));

      /* clear out the vector */
      for (i = 0; i < n; i++)
	rt->elem[i] = 0;
    }

  PUT_NUM_ELEM (rt, n);
  return rt;
}

/* Allocate an rtx of code CODE.  The CODE is stored in the rtx;
   all the rest is initialized to zero.  */

rtx
rtx_alloc (code)
  RTX_CODE code;
{
  rtx rt;

  if (ggc_p)
    rt = ggc_alloc_rtx (GET_RTX_LENGTH (code));
  else
    {
      register struct obstack *ob = rtl_obstack;
      register int nelts = GET_RTX_LENGTH (code);
      register int length = sizeof (struct rtx_def)
	+ (nelts - 1) * sizeof (rtunion);

      /* This function is called more than any other in GCC, so we
	 manipulate the obstack directly.
       
	 Even though rtx objects are word aligned, we may be sharing
	 an obstack with tree nodes, which may have to be double-word
	 aligned.  So align our length to the alignment mask in the
	 obstack.  */

      length = (length + ob->alignment_mask) & ~ ob->alignment_mask;

      if (ob->chunk_limit - ob->next_free < length)
	_obstack_newchunk (ob, length);
      rt = (rtx)ob->object_base;
      ob->next_free += length;
      ob->object_base = ob->next_free;

      /* We want to clear everything up to the FLD array.  Normally,
	 this is one int, but we don't want to assume that and it
	 isn't very portable anyway; this is.  */

      memset (rt, 0, sizeof (struct rtx_def) - sizeof (rtunion));
    }

  PUT_CODE (rt, code);
  return rt;
}

/* Free the rtx X and all RTL allocated since X.  */

void
rtx_free (x)
     rtx x;
{
  if (!ggc_p)
    obstack_free (rtl_obstack, x);
}

/* Create a new copy of an rtx.
   Recursively copies the operands of the rtx,
   except for those few rtx codes that are sharable.  */

rtx
copy_rtx (orig)
     register rtx orig;
{
  register rtx copy;
  register int i, j;
  register RTX_CODE code;
  register const char *format_ptr;

  code = GET_CODE (orig);

  switch (code)
    {
    case REG:
    case QUEUED:
    case CONST_INT:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case CODE_LABEL:
    case PC:
    case CC0:
    case SCRATCH:
      /* SCRATCH must be shared because they represent distinct values.  */
    case ADDRESSOF:
      return orig;

    case CONST:
      /* CONST can be shared if it contains a SYMBOL_REF.  If it contains
	 a LABEL_REF, it isn't sharable.  */
      if (GET_CODE (XEXP (orig, 0)) == PLUS
	  && GET_CODE (XEXP (XEXP (orig, 0), 0)) == SYMBOL_REF
	  && GET_CODE (XEXP (XEXP (orig, 0), 1)) == CONST_INT)
	return orig;
      break;

      /* A MEM with a constant address is not sharable.  The problem is that
	 the constant address may need to be reloaded.  If the mem is shared,
	 then reloading one copy of this mem will cause all copies to appear
	 to have been reloaded.  */

    default:
      break;
    }

  copy = rtx_alloc (code);

  /* Copy the various flags, and other information.  We assume that
     all fields need copying, and then clear the fields that should
     not be copied.  That is the sensible default behavior, and forces
     us to explicitly document why we are *not* copying a flag.  */
  memcpy (copy, orig, sizeof (struct rtx_def) - sizeof (rtunion));

  /* We do not copy the USED flag, which is used as a mark bit during
     walks over the RTL.  */
  copy->used = 0;

  /* We do not copy FRAME_RELATED for INSNs.  */
  if (GET_RTX_CLASS (code) == 'i')
    copy->frame_related = 0;
  copy->jump = orig->jump;
  copy->call = orig->call;

  format_ptr = GET_RTX_FORMAT (GET_CODE (copy));

  for (i = 0; i < GET_RTX_LENGTH (GET_CODE (copy)); i++)
    {
      copy->fld[i] = orig->fld[i];
      switch (*format_ptr++)
	{
	case 'e':
	  if (XEXP (orig, i) != NULL)
	    XEXP (copy, i) = copy_rtx (XEXP (orig, i));
	  break;

	case 'E':
	case 'V':
	  if (XVEC (orig, i) != NULL)
	    {
	      XVEC (copy, i) = rtvec_alloc (XVECLEN (orig, i));
	      for (j = 0; j < XVECLEN (copy, i); j++)
		XVECEXP (copy, i, j) = copy_rtx (XVECEXP (orig, i, j));
	    }
	  break;

	case 'b':
	  {
	    bitmap new_bits = BITMAP_OBSTACK_ALLOC (rtl_obstack);
	    bitmap_copy (new_bits, XBITMAP (orig, i));
	    XBITMAP (copy, i) = new_bits;
	    break;
	  }

	case 't':
	case 'w':
	case 'i':
	case 's':
	case 'S':
	case 'u':
	case '0':
	  /* These are left unchanged.  */
	  break;
	  
	default:
	  abort ();
	}
    }
  return copy;
}

/* Similar to `copy_rtx' except that if MAY_SHARE is present, it is
   placed in the result directly, rather than being copied.  */

rtx
copy_most_rtx (orig, may_share)
     register rtx orig;
     register rtx may_share;
{
  register rtx copy;
  register int i, j;
  register RTX_CODE code;
  register const char *format_ptr;

  if (orig == may_share)
    return orig;

  code = GET_CODE (orig);

  switch (code)
    {
    case REG:
    case QUEUED:
    case CONST_INT:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case CODE_LABEL:
    case PC:
    case CC0:
      return orig;
    default:
      break;
    }

  copy = rtx_alloc (code);
  PUT_MODE (copy, GET_MODE (orig));
  copy->in_struct = orig->in_struct;
  copy->volatil = orig->volatil;
  copy->unchanging = orig->unchanging;
  copy->integrated = orig->integrated;
  
  format_ptr = GET_RTX_FORMAT (GET_CODE (copy));

  for (i = 0; i < GET_RTX_LENGTH (GET_CODE (copy)); i++)
    {
      switch (*format_ptr++)
	{
	case 'e':
	  XEXP (copy, i) = XEXP (orig, i);
	  if (XEXP (orig, i) != NULL && XEXP (orig, i) != may_share)
	    XEXP (copy, i) = copy_most_rtx (XEXP (orig, i), may_share);
	  break;

	case 'u':
	  XEXP (copy, i) = XEXP (orig, i);
	  break;

	case 'E':
	case 'V':
	  XVEC (copy, i) = XVEC (orig, i);
	  if (XVEC (orig, i) != NULL)
	    {
	      XVEC (copy, i) = rtvec_alloc (XVECLEN (orig, i));
	      for (j = 0; j < XVECLEN (copy, i); j++)
		XVECEXP (copy, i, j)
		  = copy_most_rtx (XVECEXP (orig, i, j), may_share);
	    }
	  break;

	case 'w':
	  XWINT (copy, i) = XWINT (orig, i);
	  break;

	case 'n':
	case 'i':
	  XINT (copy, i) = XINT (orig, i);
	  break;

	case 't':
	  XTREE (copy, i) = XTREE (orig, i);
	  break;

	case 's':
	case 'S':
	  XSTR (copy, i) = XSTR (orig, i);
	  break;

	case '0':
	  /* Copy this through the wide int field; that's safest. */
	  X0WINT (copy, i) = X0WINT (orig, i);
	  break;

	default:
	  abort ();
	}
    }
  return copy;
}

/* Create a new copy of an rtx.  Only copy just one level.  */
rtx
shallow_copy_rtx (orig)
     rtx orig;
{
  register int i;
  register RTX_CODE code = GET_CODE (orig);
  register rtx copy = rtx_alloc (code);

  PUT_MODE (copy, GET_MODE (orig));
  copy->in_struct = orig->in_struct;
  copy->volatil = orig->volatil;
  copy->unchanging = orig->unchanging;
  copy->integrated = orig->integrated;

  for (i = 0; i < GET_RTX_LENGTH (code); i++)
    copy->fld[i] = orig->fld[i];

  return copy;
}

/* This is 1 until after the rtl generation pass.  */
int rtx_equal_function_value_matters;

/* Return 1 if X and Y are identical-looking rtx's.
   This is the Lisp function EQUAL for rtx arguments.  */

int
rtx_equal_p (x, y)
     rtx x, y;
{
  register int i;
  register int j;
  register enum rtx_code code;
  register const char *fmt;

  if (x == y)
    return 1;
  if (x == 0 || y == 0)
    return 0;

  code = GET_CODE (x);
  /* Rtx's of different codes cannot be equal.  */
  if (code != GET_CODE (y))
    return 0;

  /* (MULT:SI x y) and (MULT:HI x y) are NOT equivalent.
     (REG:SI x) and (REG:HI x) are NOT equivalent.  */

  if (GET_MODE (x) != GET_MODE (y))
    return 0;

  /* REG, LABEL_REF, and SYMBOL_REF can be compared nonrecursively.  */

  if (code == REG)
    /* Until rtl generation is complete, don't consider a reference to the
       return register of the current function the same as the return from a
       called function.  This eases the job of function integration.  Once the
       distinction is no longer needed, they can be considered equivalent.  */
    return (REGNO (x) == REGNO (y)
	    && (! rtx_equal_function_value_matters
		|| REG_FUNCTION_VALUE_P (x) == REG_FUNCTION_VALUE_P (y)));
  else if (code == LABEL_REF)
    return XEXP (x, 0) == XEXP (y, 0);
  else if (code == SYMBOL_REF)
    return XSTR (x, 0) == XSTR (y, 0);
  else if (code == SCRATCH || code == CONST_DOUBLE)
    return 0;

  /* Compare the elements.  If any pair of corresponding elements
     fail to match, return 0 for the whole things.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      switch (fmt[i])
	{
	case 'w':
	  if (XWINT (x, i) != XWINT (y, i))
	    return 0;
	  break;

	case 'n':
	case 'i':
	  if (XINT (x, i) != XINT (y, i))
	    return 0;
	  break;

	case 'V':
	case 'E':
	  /* Two vectors must have the same length.  */
	  if (XVECLEN (x, i) != XVECLEN (y, i))
	    return 0;

	  /* And the corresponding elements must match.  */
	  for (j = 0; j < XVECLEN (x, i); j++)
	    if (rtx_equal_p (XVECEXP (x, i, j), XVECEXP (y, i, j)) == 0)
	      return 0;
	  break;

	case 'e':
	  if (rtx_equal_p (XEXP (x, i), XEXP (y, i)) == 0)
	    return 0;
	  break;

	case 'S':
	case 's':
	  if (strcmp (XSTR (x, i), XSTR (y, i)))
	    return 0;
	  break;

	case 'u':
	  /* These are just backpointers, so they don't matter.  */
	  break;

	case '0':
	case 't':
	  break;

	  /* It is believed that rtx's at this level will never
	     contain anything but integers and other rtx's,
	     except for within LABEL_REFs and SYMBOL_REFs.  */
	default:
	  abort ();
	}
    }
  return 1;
}

/* Subroutines of read_rtx.  */

/* The current line number for the file.  */
int read_rtx_lineno = 1;

/* The filename for aborting with file and line.  */
const char *read_rtx_filename = "<unknown>";

static void
fatal_with_file_and_line VPARAMS ((FILE *infile, const char *msg, ...))
{
#ifndef ANSI_PROTOTYPES
  FILE *infile;
  const char *msg;
#endif
  va_list ap;
  char context[64];
  size_t i;
  int c;

  VA_START (ap, msg);

#ifndef ANSI_PROTOTYPES
  infile = va_arg (ap, FILE *);
  msg = va_arg (ap, const char *);
#endif

  fprintf (stderr, "%s:%d: ", read_rtx_filename, read_rtx_lineno);
  vfprintf (stderr, msg, ap);
  putc ('\n', stderr);

  /* Gather some following context.  */
  for (i = 0; i < sizeof(context)-1; ++i)
    {
      c = getc (infile);
      if (c == EOF)
	break;
      if (c == '\r' || c == '\n')
	break;
      context[i] = c;
    }
  context[i] = '\0';

  fprintf (stderr, "%s:%d: following context is `%s'\n",
	   read_rtx_filename, read_rtx_lineno, context);

  va_end (ap);
  exit (1);
}

/* Dump code after printing a message.  Used when read_rtx finds
   invalid data.  */

static void
fatal_expected_char (infile, expected_c, actual_c)
     FILE *infile;
     int expected_c, actual_c;
{
  fatal_with_file_and_line (infile, "expected character `%c', found `%c'",
			    expected_c, actual_c);
}

/* Read chars from INFILE until a non-whitespace char
   and return that.  Comments, both Lisp style and C style,
   are treated as whitespace.
   Tools such as genflags use this function.  */

int
read_skip_spaces (infile)
     FILE *infile;
{
  register int c;
  while (1)
    {
      c = getc (infile);
      switch (c)
	{
	case '\n':
	  read_rtx_lineno++;
	  break;

	case ' ': case '\t': case '\f': case '\r':
	  break;

	case ';':
	  do 
	    c = getc (infile);
	  while (c != '\n' && c != EOF);
	  read_rtx_lineno++;
	  break;

	case '/':
	  {
	    register int prevc;
	    c = getc (infile);
	    if (c != '*')
	      fatal_expected_char (infile, '*', c);
	  
	    prevc = 0;
	    while ((c = getc (infile)) && c != EOF)
	      {
		if (c == '\n')
		   read_rtx_lineno++;
	        else if (prevc == '*' && c == '/')
		  break;
	        prevc = c;
	      }
	  }
	  break;

	default:
	  return c;
	}
    }
}

/* Read an rtx code name into the buffer STR[].
   It is terminated by any of the punctuation chars of rtx printed syntax.  */

static void
read_name (str, infile)
     char *str;
     FILE *infile;
{
  register char *p;
  register int c;

  c = read_skip_spaces(infile);

  p = str;
  while (1)
    {
      if (c == ' ' || c == '\n' || c == '\t' || c == '\f')
	break;
      if (c == ':' || c == ')' || c == ']' || c == '"' || c == '/'
	  || c == '(' || c == '[')
	{
	  ungetc (c, infile);
	  break;
	}
      *p++ = c;
      c = getc (infile);
    }
  if (p == str)
    fatal_with_file_and_line (infile, "missing name or number");
  if (c == '\n')
    read_rtx_lineno++;

  *p = 0;
}

/* Provide a version of a function to read a long long if the system does
   not provide one.  */
#if HOST_BITS_PER_WIDE_INT > HOST_BITS_PER_LONG && !defined(HAVE_ATOLL) && !defined(HAVE_ATOQ)
HOST_WIDE_INT
atoll(p)
    const char *p;
{
  int neg = 0;
  HOST_WIDE_INT tmp_wide;

  while (ISSPACE(*p))
    p++;
  if (*p == '-')
    neg = 1, p++;
  else if (*p == '+')
    p++;

  tmp_wide = 0;
  while (ISDIGIT(*p))
    {
      HOST_WIDE_INT new_wide = tmp_wide*10 + (*p - '0');
      if (new_wide < tmp_wide)
	{
	  /* Return INT_MAX equiv on overflow.  */
	  tmp_wide = (~(unsigned HOST_WIDE_INT)0) >> 1;
	  break;
	}
      tmp_wide = new_wide;
      p++;
    }

  if (neg)
    tmp_wide = -tmp_wide;
  return tmp_wide;
}
#endif

/* Read an rtx in printed representation from INFILE
   and return an actual rtx in core constructed accordingly.
   read_rtx is not used in the compiler proper, but rather in
   the utilities gen*.c that construct C code from machine descriptions.  */

rtx
read_rtx (infile)
     FILE *infile;
{
  register int i, j, list_counter;
  RTX_CODE tmp_code;
  register const char *format_ptr;
  /* tmp_char is a buffer used for reading decimal integers
     and names of rtx types and machine modes.
     Therefore, 256 must be enough.  */
  char tmp_char[256];
  rtx return_rtx;
  register int c;
  int tmp_int;
  HOST_WIDE_INT tmp_wide;

  /* Linked list structure for making RTXs: */
  struct rtx_list
    {
      struct rtx_list *next;
      rtx value;		/* Value of this node.  */
    };

  c = read_skip_spaces (infile); /* Should be open paren.  */
  if (c != '(')
    fatal_expected_char (infile, '(', c);

  read_name (tmp_char, infile);

  tmp_code = UNKNOWN;

  for (i = 0; i < NUM_RTX_CODE; i++)
    if (! strcmp (tmp_char, GET_RTX_NAME (i)))
      {
	tmp_code = (RTX_CODE) i;	/* get value for name */
	break;
      }

  if (tmp_code == UNKNOWN)
    fatal_with_file_and_line (infile, "unknown rtx code `%s'", tmp_char);

  /* (NIL) stands for an expression that isn't there.  */
  if (tmp_code == NIL)
    {
      /* Discard the closeparen.  */
      while ((c = getc (infile)) && c != ')')
	;

      return 0;
    }

  /* If we end up with an insn expression then we free this space below.  */
  return_rtx = rtx_alloc (tmp_code);
  format_ptr = GET_RTX_FORMAT (GET_CODE (return_rtx));

  /* If what follows is `: mode ', read it and
     store the mode in the rtx.  */

  i = read_skip_spaces (infile);
  if (i == ':')
    {
      read_name (tmp_char, infile);
      for (j = 0; j < NUM_MACHINE_MODES; j++)
	if (! strcmp (GET_MODE_NAME (j), tmp_char))
	  break;

      if (j == MAX_MACHINE_MODE)
	fatal_with_file_and_line (infile, "unknown mode `%s'", tmp_char);

      PUT_MODE (return_rtx, (enum machine_mode) j);
    }
  else
    ungetc (i, infile);

  for (i = 0; i < GET_RTX_LENGTH (GET_CODE (return_rtx)); i++)
    switch (*format_ptr++)
      {
	/* 0 means a field for internal use only.
	   Don't expect it to be present in the input.  */
      case '0':
	break;

      case 'e':
      case 'u':
	XEXP (return_rtx, i) = read_rtx (infile);
	break;

      case 'V':
	/* 'V' is an optional vector: if a closeparen follows,
	   just store NULL for this element.  */
	c = read_skip_spaces (infile);
	ungetc (c, infile);
	if (c == ')')
	  {
	    XVEC (return_rtx, i) = 0;
	    break;
 	  }
	/* Now process the vector.  */
  
      case 'E':
	{
	  register struct rtx_list *next_rtx, *rtx_list_link;
	  struct rtx_list *list_rtx = NULL;

	  c = read_skip_spaces (infile);
	  if (c != '[')
	    fatal_expected_char (infile, '[', c);

	  /* add expressions to a list, while keeping a count */
	  next_rtx = NULL;
	  list_counter = 0;
	  while ((c = read_skip_spaces (infile)) && c != ']')
	    {
	      ungetc (c, infile);
	      list_counter++;
	      rtx_list_link = (struct rtx_list *)
		alloca (sizeof (struct rtx_list));
	      rtx_list_link->value = read_rtx (infile);
	      if (next_rtx == 0)
		list_rtx = rtx_list_link;
	      else
		next_rtx->next = rtx_list_link;
	      next_rtx = rtx_list_link;
	      rtx_list_link->next = 0;
	    }
	  /* get vector length and allocate it */
	  XVEC (return_rtx, i) = (list_counter
				  ? rtvec_alloc (list_counter) : NULL_RTVEC);
	  if (list_counter > 0)
	    {
	      next_rtx = list_rtx;
	      for (j = 0; j < list_counter; j++,
		   next_rtx = next_rtx->next)
		XVECEXP (return_rtx, i, j) = next_rtx->value;
	    }
	  /* close bracket gotten */
	}
	break;

      case 'S':
	/* 'S' is an optional string: if a closeparen follows,
	   just store NULL for this element.  */
	c = read_skip_spaces (infile);
	ungetc (c, infile);
	if (c == ')')
	  {
	    XSTR (return_rtx, i) = 0;
	    break;
	  }

      case 's':
	{
	  int saw_paren = 0;
	  register char *stringbuf;

	  c = read_skip_spaces (infile);
	  if (c == '(')
	    {
	      saw_paren = 1;
	      c = read_skip_spaces (infile);
	    }
	  if (c != '"')
	    fatal_expected_char (infile, '"', c);

	  while (1)
	    {
	      c = getc (infile); /* Read the string  */
	      if (c == '\n')
		read_rtx_lineno++;
	      if (c == '\\')
		{
		  c = getc (infile);	/* Read the string  */
		  /* \; makes stuff for a C string constant containing
		     newline and tab.  */
		  if (c == ';')
		    {
		      obstack_grow (rtl_obstack, "\\n\\t", 4);
		      continue;
		    }
		  if (c == '\n')
		    read_rtx_lineno++;
		}
	      else if (c == '"')
		break;

	      obstack_1grow (rtl_obstack, c);
	    }

	  obstack_1grow (rtl_obstack, 0);
	  stringbuf = (char *) obstack_finish (rtl_obstack);

	  if (saw_paren)
	    {
	      c = read_skip_spaces (infile);
	      if (c != ')')
		fatal_expected_char (infile, ')', c);
	    }
	  XSTR (return_rtx, i) = stringbuf;
	}
	break;

      case 'w':
	read_name (tmp_char, infile);
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
	tmp_wide = atoi (tmp_char);
#else
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_LONG
	tmp_wide = atol (tmp_char);
#else
	/* Prefer atoll over atoq, since the former is in the ISO C9X draft. 
	   But prefer not to use our hand-rolled function above either.  */
#if defined(HAVE_ATOLL) || !defined(HAVE_ATOQ)
	tmp_wide = atoll (tmp_char);
#else
	tmp_wide = atoq (tmp_char);
#endif
#endif
#endif
	XWINT (return_rtx, i) = tmp_wide;
	break;

      case 'i':
      case 'n':
	read_name (tmp_char, infile);
	tmp_int = atoi (tmp_char);
	XINT (return_rtx, i) = tmp_int;
	break;

      default:
	fprintf (stderr,
		 "switch format wrong in rtl.read_rtx(). format was: %c.\n",
		 format_ptr[-1]);
	fprintf (stderr, "\tfile position: %ld\n", ftell (infile));
	abort ();
      }

  c = read_skip_spaces (infile);
  if (c != ')')
    fatal_expected_char (infile, ')', c);

  return return_rtx;
}

#if defined ENABLE_RTL_CHECKING && (GCC_VERSION >= 2007)
void
rtl_check_failed_bounds (r, n, file, line, func)
    rtx r;
    int n;
    const char *file;
    int line;
    const char *func;
{
  error ("RTL check: access of elt %d of `%s' with last elt %d",
	 n, GET_RTX_NAME (GET_CODE (r)), GET_RTX_LENGTH (GET_CODE (r))-1);
  fancy_abort (file, line, func);
}

void
rtl_check_failed_type1 (r, n, c1, file, line, func)
    rtx r;
    int n;
    int c1;
    const char *file;
    int line;
    const char *func;
{
  error ("RTL check: expected elt %d type '%c', have '%c' (rtx %s)",
	 n, c1, GET_RTX_FORMAT (GET_CODE (r))[n], GET_RTX_NAME (GET_CODE (r)));
  fancy_abort (file, line, func);
}

void
rtl_check_failed_type2 (r, n, c1, c2, file, line, func)
    rtx r;
    int n;
    int c1;
    int c2;
    const char *file;
    int line;
    const char *func;
{
  error ("RTL check: expected elt %d type '%c' or '%c', have '%c' (rtx %s)",
	 n, c1, c2,
	 GET_RTX_FORMAT (GET_CODE (r))[n], GET_RTX_NAME (GET_CODE(r)));
  fancy_abort (file, line, func);
}

void
rtl_check_failed_code1 (r, code, file, line, func)
    rtx r;
    enum rtx_code code;
    const char *file;
    int line;
    const char *func;
{
  error ("RTL check: expected code `%s', have `%s'",
 	 GET_RTX_NAME (code), GET_RTX_NAME (GET_CODE (r)));
  fancy_abort (file, line, func);
}

void
rtl_check_failed_code2 (r, code1, code2, file, line, func)
    rtx r;
    enum rtx_code code1, code2;
    const char *file;
    int line;
    const char *func;
{
  error ("RTL check: expected code `%s' or `%s', have `%s'",
 	 GET_RTX_NAME (code1), GET_RTX_NAME (code2),
	 GET_RTX_NAME (GET_CODE (r)));
  fancy_abort (file, line, func);
}

/* XXX Maybe print the vector?  */
void
rtvec_check_failed_bounds (r, n, file, line, func)
    rtvec r;
    int n;
    const char *file;
    int line;
    const char *func;
{
  error ("RTL check: access of elt %d of vector with last elt %d",
	 n, GET_NUM_ELEM (r)-1);
  fancy_abort (file, line, func);
}
#endif /* ENABLE_RTL_CHECKING */

/* These are utility functions used by fatal-error functions all over the
   code.  rtl.c happens to be linked by all the programs that need them,
   so these are here.  In the future we want to break out all error handling
   to its own module.  */

/* Given a partial pathname as input, return another pathname that
   shares no directory elements with the pathname of __FILE__.  This
   is used by fancy_abort() to print `Internal compiler error in expr.c'
   instead of `Internal compiler error in ../../egcs/gcc/expr.c'.  */
static const char *
trim_filename (name)
     const char *name;
{
  static const char this_file[] = __FILE__;
  const char *p = name, *q = this_file;

  while (*p == *q && *p != 0 && *q != 0) p++, q++;
  while (p > name && p[-1] != DIR_SEPARATOR
#ifdef DIR_SEPARATOR_2
	 && p[-1] != DIR_SEPARATOR_2
#endif
	 )
    p--;

  return p;
}

/* Report an internal compiler error in a friendly manner and without
   dumping core.  */

void
fancy_abort (file, line, function)
     const char *file;
     int line;
     const char *function;
{
  if (function == NULL)
    function = "?";
  fatal (
"Internal compiler error in `%s', at %s:%d\n\
Please submit a full bug report.\n\
See %s for instructions.",
	 function, trim_filename (file), line, GCCBUGURL);
}
