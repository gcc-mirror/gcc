/* RTL utility routines.
   Copyright (C) 1987, 1988, 1991, 1994, 1997, 1998, 1999, 2000, 2001, 2002
   Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "rtl.h"
#include "real.h"
#include "ggc.h"
#include "errors.h"


/* Calculate the format for CONST_DOUBLE.  This depends on the relative
   widths of HOST_WIDE_INT and REAL_VALUE_TYPE.

   We need to go out to 0wwwww, since REAL_ARITHMETIC assumes 16-bits
   per element in REAL_VALUE_TYPE.

   This is duplicated in gengenrtl.c.

   A number of places assume that there are always at least two 'w'
   slots in a CONST_DOUBLE, so we provide them even if one would suffice.  */

#ifdef REAL_ARITHMETIC
# if MAX_LONG_DOUBLE_TYPE_SIZE == 96
#  define REAL_WIDTH	\
     (11*8 + HOST_BITS_PER_WIDE_INT)/HOST_BITS_PER_WIDE_INT
# else
#  if MAX_LONG_DOUBLE_TYPE_SIZE == 128
#   define REAL_WIDTH	\
      (19*8 + HOST_BITS_PER_WIDE_INT)/HOST_BITS_PER_WIDE_INT
#  else
#   if HOST_FLOAT_FORMAT != TARGET_FLOAT_FORMAT
#    define REAL_WIDTH	\
       (7*8 + HOST_BITS_PER_WIDE_INT)/HOST_BITS_PER_WIDE_INT
#   endif
#  endif
# endif
#endif /* REAL_ARITHMETIC */

#ifndef REAL_WIDTH
# if HOST_BITS_PER_WIDE_INT*2 >= MAX_LONG_DOUBLE_TYPE_SIZE
#  define REAL_WIDTH	2
# else
#  if HOST_BITS_PER_WIDE_INT*3 >= MAX_LONG_DOUBLE_TYPE_SIZE
#   define REAL_WIDTH	3
#  else
#   if HOST_BITS_PER_WIDE_INT*4 >= MAX_LONG_DOUBLE_TYPE_SIZE
#    define REAL_WIDTH	4
#   endif
#  endif
# endif
#endif /* REAL_WIDTH */

#if REAL_WIDTH == 1
# define CONST_DOUBLE_FORMAT	"0ww"
#else
# if REAL_WIDTH == 2
#  define CONST_DOUBLE_FORMAT	"0ww"
# else
#  if REAL_WIDTH == 3
#   define CONST_DOUBLE_FORMAT	"0www"
#  else
#   if REAL_WIDTH == 4
#    define CONST_DOUBLE_FORMAT	"0wwww"
#   else
#    if REAL_WIDTH == 5
#     define CONST_DOUBLE_FORMAT	"0wwwww"
#    else
#     define CONST_DOUBLE_FORMAT	/* nothing - will cause syntax error */
#    endif
#   endif
#  endif
# endif
#endif

/* Indexed by rtx code, gives number of operands for an rtx with that code.
   Does NOT include rtx header data (code and links).  */

#define DEF_RTL_EXPR(ENUM, NAME, FORMAT, CLASS)   sizeof FORMAT - 1 ,

const unsigned char rtx_length[NUM_RTX_CODE] = {
#include "rtl.def"
};

#undef DEF_RTL_EXPR

/* Indexed by rtx code, gives the name of that kind of rtx, as a C string.  */

#define DEF_RTL_EXPR(ENUM, NAME, FORMAT, CLASS)   NAME ,

const char * const rtx_name[NUM_RTX_CODE] = {
#include "rtl.def"		/* rtl expressions are documented here */
};

#undef DEF_RTL_EXPR

/* Indexed by machine mode, gives the name of that machine mode.
   This name does not include the letters "mode".  */

#define DEF_MACHMODE(SYM, NAME, CLASS, BITSIZE, SIZE, UNIT, WIDER, INNER)  NAME,

const char * const mode_name[NUM_MACHINE_MODES] = {
#include "machmode.def"
};

#undef DEF_MACHMODE

/* Indexed by machine mode, gives the class mode for GET_MODE_CLASS.  */

#define DEF_MACHMODE(SYM, NAME, CLASS, BITSIZE, SIZE, UNIT, WIDER, INNER)  CLASS,

const enum mode_class mode_class[NUM_MACHINE_MODES] = {
#include "machmode.def"
};

#undef DEF_MACHMODE

/* Indexed by machine mode, gives the length of the mode, in bits.
   GET_MODE_BITSIZE uses this.  */

#define DEF_MACHMODE(SYM, NAME, CLASS, BITSIZE, SIZE, UNIT, WIDER, INNER)  BITSIZE,

const unsigned short mode_bitsize[NUM_MACHINE_MODES] = {
#include "machmode.def"
};

#undef DEF_MACHMODE

/* Indexed by machine mode, gives the length of the mode, in bytes.
   GET_MODE_SIZE uses this.  */

#define DEF_MACHMODE(SYM, NAME, CLASS, BITSIZE, SIZE, UNIT, WIDER, INNER)  SIZE,

const unsigned char mode_size[NUM_MACHINE_MODES] = {
#include "machmode.def"
};

#undef DEF_MACHMODE

/* Indexed by machine mode, gives the length of the mode's subunit.
   GET_MODE_UNIT_SIZE uses this.  */

#define DEF_MACHMODE(SYM, NAME, CLASS, BITSIZE, SIZE, UNIT, WIDER, INNER)  UNIT,

const unsigned char mode_unit_size[NUM_MACHINE_MODES] = {
#include "machmode.def"		/* machine modes are documented here */
};

#undef DEF_MACHMODE

/* Indexed by machine mode, gives next wider natural mode
   (QI -> HI -> SI -> DI, etc.)  Widening multiply instructions
   use this.  */

#define DEF_MACHMODE(SYM, NAME, CLASS, BITSIZE, SIZE, UNIT, WIDER, INNER)  \
  (unsigned char) WIDER,

const unsigned char mode_wider_mode[NUM_MACHINE_MODES] = {
#include "machmode.def"		/* machine modes are documented here */
};

#undef DEF_MACHMODE

#define DEF_MACHMODE(SYM, NAME, CLASS, BITSIZE, SIZE, UNIT, WIDER, INNER)  \
  ((BITSIZE) >= HOST_BITS_PER_WIDE_INT) ? ~(unsigned HOST_WIDE_INT) 0 : ((unsigned HOST_WIDE_INT) 1 << (BITSIZE)) - 1,

/* Indexed by machine mode, gives mask of significant bits in mode.  */

const unsigned HOST_WIDE_INT mode_mask_array[NUM_MACHINE_MODES] = {
#include "machmode.def"
};

#undef DEF_MACHMODE

#define DEF_MACHMODE(SYM, NAME, CLASS, BITSIZE, SIZE, UNIT, WIDER, INNER) INNER,

/* Indexed by machine mode, gives the mode of the inner elements in a
   vector type.  */

const enum machine_mode inner_mode_array[NUM_MACHINE_MODES] = {
#include "machmode.def"
};

/* Indexed by mode class, gives the narrowest mode for each class.
   The Q modes are always of width 1 (2 for complex) - it is impossible
   for any mode to be narrower.

   Note that we use QImode instead of BImode for MODE_INT, since
   otherwise the middle end will try to use it for bitfields in
   structures and the like, which we do not want.  Only the target
   md file should generate BImode widgets.  */

const enum machine_mode class_narrowest_mode[(int) MAX_MODE_CLASS] = {
    /* MODE_RANDOM */		VOIDmode,
    /* MODE_INT */		QImode,
    /* MODE_FLOAT */		QFmode,
    /* MODE_PARTIAL_INT */	PQImode,
    /* MODE_CC */		CCmode,
    /* MODE_COMPLEX_INT */	CQImode,
    /* MODE_COMPLEX_FLOAT */	QCmode,
    /* MODE_VECTOR_INT */	V2QImode,
    /* MODE_VECTOR_FLOAT */	V2SFmode
};


/* Indexed by rtx code, gives a sequence of operand-types for
   rtx's of that code.  The sequence is a C string in which
   each character describes one operand.  */

const char * const rtx_format[NUM_RTX_CODE] = {
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
     "T" like "s", but treated specially by the RTL reader;
         only found in machine description patterns.
     "e" a pointer to an rtl expression
         prints the expression
     "E" a pointer to a vector that points to a number of rtl expressions
         prints a list of the rtl expressions
     "V" like "E", but optional:
	 the containing rtx may end before this operand
     "u" a pointer to another insn
         prints the uid of the insn.
     "b" is a pointer to a bitmap header.
     "t" is a tree pointer.  */

#define DEF_RTL_EXPR(ENUM, NAME, FORMAT, CLASS)   FORMAT ,
#include "rtl.def"		/* rtl expressions are defined here */
#undef DEF_RTL_EXPR
};

/* Indexed by rtx code, gives a character representing the "class" of
   that rtx code.  See rtl.def for documentation on the defined classes.  */

const char rtx_class[NUM_RTX_CODE] = {
#define DEF_RTL_EXPR(ENUM, NAME, FORMAT, CLASS)   CLASS,
#include "rtl.def"		/* rtl expressions are defined here */
#undef DEF_RTL_EXPR
};

/* Names for kinds of NOTEs and REG_NOTEs.  */

const char * const note_insn_name[NOTE_INSN_MAX - NOTE_INSN_BIAS] =
{
  "", "NOTE_INSN_DELETED",
  "NOTE_INSN_BLOCK_BEG", "NOTE_INSN_BLOCK_END",
  "NOTE_INSN_LOOP_BEG", "NOTE_INSN_LOOP_END",
  "NOTE_INSN_LOOP_CONT", "NOTE_INSN_LOOP_VTOP",
  "NOTE_INSN_LOOP_END_TOP_COND", "NOTE_INSN_FUNCTION_END",
  "NOTE_INSN_PROLOGUE_END", "NOTE_INSN_EPILOGUE_BEG",
  "NOTE_INSN_DELETED_LABEL", "NOTE_INSN_FUNCTION_BEG",
  "NOTE_INSN_EH_REGION_BEG", "NOTE_INSN_EH_REGION_END",
  "NOTE_INSN_REPEATED_LINE_NUMBER", "NOTE_INSN_RANGE_BEG",
  "NOTE_INSN_RANGE_END", "NOTE_INSN_LIVE",
  "NOTE_INSN_BASIC_BLOCK", "NOTE_INSN_EXPECTED_VALUE"
};

const char * const reg_note_name[] =
{
  "", "REG_DEAD", "REG_INC", "REG_EQUIV", "REG_EQUAL",
  "REG_WAS_0", "REG_RETVAL", "REG_LIBCALL", "REG_NONNEG",
  "REG_NO_CONFLICT", "REG_UNUSED", "REG_CC_SETTER", "REG_CC_USER",
  "REG_LABEL", "REG_DEP_ANTI", "REG_DEP_OUTPUT", "REG_BR_PROB",
  "REG_EXEC_COUNT", "REG_NOALIAS", "REG_SAVE_AREA", "REG_BR_PRED",
  "REG_FRAME_RELATED_EXPR", "REG_EH_CONTEXT", "REG_EH_REGION",
  "REG_SAVE_NOTE", "REG_MAYBE_DEAD", "REG_NORETURN",
  "REG_NON_LOCAL_GOTO", "REG_SETJMP", "REG_ALWAYS_RETURN",
  "REG_VTABLE_REF"
};


/* Allocate an rtx vector of N elements.
   Store the length, and initialize all elements to zero.  */

rtvec
rtvec_alloc (n)
     int n;
{
  rtvec rt;

  rt = ggc_alloc_rtvec (n);
  /* clear out the vector */
  memset (&rt->elem[0], 0, n * sizeof (rtx));

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
  int n = GET_RTX_LENGTH (code);

  rt = ggc_alloc_rtx (n);

  /* We want to clear everything up to the FLD array.  Normally, this
     is one int, but we don't want to assume that and it isn't very
     portable anyway; this is.  */

  memset (rt, 0, sizeof (struct rtx_def) - sizeof (rtunion));
  PUT_CODE (rt, code);
  return rt;
}


/* Create a new copy of an rtx.
   Recursively copies the operands of the rtx,
   except for those few rtx codes that are sharable.  */

rtx
copy_rtx (orig)
     rtx orig;
{
  rtx copy;
  int i, j;
  RTX_CODE code;
  const char *format_ptr;

  code = GET_CODE (orig);

  switch (code)
    {
    case REG:
    case QUEUED:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST_VECTOR:
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

	case 't':
	case 'w':
	case 'i':
	case 's':
	case 'S':
	case 'T':
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

/* Create a new copy of an rtx.  Only copy just one level.  */

rtx
shallow_copy_rtx (orig)
     rtx orig;
{
  int i;
  RTX_CODE code = GET_CODE (orig);
  rtx copy = rtx_alloc (code);

  PUT_MODE (copy, GET_MODE (orig));
  copy->in_struct = orig->in_struct;
  copy->volatil = orig->volatil;
  copy->unchanging = orig->unchanging;
  copy->integrated = orig->integrated;
  copy->frame_related = orig->frame_related;

  for (i = 0; i < GET_RTX_LENGTH (code); i++)
    copy->fld[i] = orig->fld[i];

  return copy;
}

/* Return the alignment of MODE. This will be bounded by 1 and
   BIGGEST_ALIGNMENT.  */

unsigned int
get_mode_alignment (mode)
     enum machine_mode mode;
{
  unsigned int alignment;

  if (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT
      || GET_MODE_CLASS (mode) == MODE_COMPLEX_INT)
    alignment = GET_MODE_UNIT_SIZE (mode);
  else
    alignment = GET_MODE_SIZE (mode);
  
  /* Extract the LSB of the size.  */
  alignment = alignment & -alignment;
  alignment *= BITS_PER_UNIT;

  alignment = MIN (BIGGEST_ALIGNMENT, MAX (1, alignment));
  return alignment;
}

/* This is 1 until after the rtl generation pass.  */
int rtx_equal_function_value_matters;

/* Nonzero when we are generating CONCATs.  */
int generating_concat_p;

/* Return 1 if X and Y are identical-looking rtx's.
   This is the Lisp function EQUAL for rtx arguments.  */

int
rtx_equal_p (x, y)
     rtx x, y;
{
  int i;
  int j;
  enum rtx_code code;
  const char *fmt;

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

  /* Some RTL can be compared nonrecursively.  */
  switch (code)
    {
    case REG:
      /* Until rtl generation is complete, don't consider a reference
	 to the return register of the current function the same as
	 the return from a called function.  This eases the job of
	 function integration.  Once the distinction is no longer
	 needed, they can be considered equivalent.  */
      return (REGNO (x) == REGNO (y)
	      && (! rtx_equal_function_value_matters
		  || REG_FUNCTION_VALUE_P (x) == REG_FUNCTION_VALUE_P (y)));

    case LABEL_REF:
      return XEXP (x, 0) == XEXP (y, 0);

    case SYMBOL_REF:
      return XSTR (x, 0) == XSTR (y, 0);

    case SCRATCH:
    case CONST_DOUBLE:
    case CONST_INT:
    case CONST_VECTOR:
      return 0;

    default:
      break;
    }

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
	  if ((XSTR (x, i) || XSTR (y, i))
	      && (! XSTR (x, i) || ! XSTR (y, i)
		  || strcmp (XSTR (x, i), XSTR (y, i))))
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

#if defined ENABLE_RTL_CHECKING && (GCC_VERSION >= 2007)
void
rtl_check_failed_bounds (r, n, file, line, func)
    rtx r;
    int n;
    const char *file;
    int line;
    const char *func;
{
  internal_error
    ("RTL check: access of elt %d of `%s' with last elt %d in %s, at %s:%d",
     n, GET_RTX_NAME (GET_CODE (r)), GET_RTX_LENGTH (GET_CODE (r)) - 1,
     func, trim_filename (file), line);
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
  internal_error
    ("RTL check: expected elt %d type '%c', have '%c' (rtx %s) in %s, at %s:%d",
     n, c1, GET_RTX_FORMAT (GET_CODE (r))[n], GET_RTX_NAME (GET_CODE (r)),
     func, trim_filename (file), line);
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
  internal_error
    ("RTL check: expected elt %d type '%c' or '%c', have '%c' (rtx %s) in %s, at %s:%d",
     n, c1, c2, GET_RTX_FORMAT (GET_CODE (r))[n], GET_RTX_NAME (GET_CODE (r)),
     func, trim_filename (file), line);
}

void
rtl_check_failed_code1 (r, code, file, line, func)
    rtx r;
    enum rtx_code code;
    const char *file;
    int line;
    const char *func;
{
  internal_error ("RTL check: expected code `%s', have `%s' in %s, at %s:%d",
		  GET_RTX_NAME (code), GET_RTX_NAME (GET_CODE (r)), func,
		  trim_filename (file), line);
}

void
rtl_check_failed_code2 (r, code1, code2, file, line, func)
    rtx r;
    enum rtx_code code1, code2;
    const char *file;
    int line;
    const char *func;
{
  internal_error
    ("RTL check: expected code `%s' or `%s', have `%s' in %s, at %s:%d",
     GET_RTX_NAME (code1), GET_RTX_NAME (code2), GET_RTX_NAME (GET_CODE (r)),
     func, trim_filename (file), line);
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
  internal_error
    ("RTL check: access of elt %d of vector with last elt %d in %s, at %s:%d",
     n, GET_NUM_ELEM (r) - 1, func, trim_filename (file), line);
}
#endif /* ENABLE_RTL_CHECKING */
