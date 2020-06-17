/* RTL utility routines.
   Copyright (C) 1987-2020 Free Software Foundation, Inc.

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

/* This file is compiled twice: once for the generator programs
   once for the compiler.  */
#ifdef GENERATOR_FILE
#include "bconfig.h"
#else
#include "config.h"
#endif

#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#ifdef GENERATOR_FILE
# include "errors.h"
#else
# include "rtlhash.h"
# include "diagnostic-core.h"
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
     "B" is a basic block pointer.
     "t" is a tree pointer.
     "r" a register.
     "p" is a poly_uint16 offset.  */

#define DEF_RTL_EXPR(ENUM, NAME, FORMAT, CLASS)   FORMAT ,
#include "rtl.def"		/* rtl expressions are defined here */
#undef DEF_RTL_EXPR
};

/* Indexed by rtx code, gives a character representing the "class" of
   that rtx code.  See rtl.def for documentation on the defined classes.  */

const enum rtx_class rtx_class[NUM_RTX_CODE] = {
#define DEF_RTL_EXPR(ENUM, NAME, FORMAT, CLASS)   CLASS,
#include "rtl.def"		/* rtl expressions are defined here */
#undef DEF_RTL_EXPR
};

/* Whether rtxs with the given code store data in the hwint field.  */

#define RTX_CODE_HWINT_P_1(ENUM)					\
    ((ENUM) == CONST_INT || (ENUM) == CONST_DOUBLE			\
     || (ENUM) == CONST_FIXED || (ENUM) == CONST_WIDE_INT)
#ifdef GENERATOR_FILE
#define RTX_CODE_HWINT_P(ENUM)						\
    (RTX_CODE_HWINT_P_1 (ENUM) || (ENUM) == EQ_ATTR_ALT)
#else
#define RTX_CODE_HWINT_P RTX_CODE_HWINT_P_1
#endif

/* Indexed by rtx code, gives the size of the rtx in bytes.  */

const unsigned char rtx_code_size[NUM_RTX_CODE] = {
#define DEF_RTL_EXPR(ENUM, NAME, FORMAT, CLASS)				\
  (RTX_CODE_HWINT_P (ENUM)						\
   ? RTX_HDR_SIZE + (sizeof FORMAT - 1) * sizeof (HOST_WIDE_INT)	\
   : (ENUM) == REG							\
   ? RTX_HDR_SIZE + sizeof (reg_info)					\
   : RTX_HDR_SIZE + (sizeof FORMAT - 1) * sizeof (rtunion)),

#include "rtl.def"
#undef DEF_RTL_EXPR
};

/* Names for kinds of NOTEs and REG_NOTEs.  */

const char * const note_insn_name[NOTE_INSN_MAX] =
{
#define DEF_INSN_NOTE(NAME) #NAME,
#include "insn-notes.def"
#undef DEF_INSN_NOTE
};

const char * const reg_note_name[REG_NOTE_MAX] =
{
#define DEF_REG_NOTE(NAME) #NAME,
#include "reg-notes.def"
#undef DEF_REG_NOTE
};

static size_t rtx_alloc_counts[(int) LAST_AND_UNUSED_RTX_CODE];
static size_t rtx_alloc_sizes[(int) LAST_AND_UNUSED_RTX_CODE];
static size_t rtvec_alloc_counts;
static size_t rtvec_alloc_sizes;


/* Allocate an rtx vector of N elements.
   Store the length, and initialize all elements to zero.  */

rtvec
rtvec_alloc (int n)
{
  rtvec rt;

  rt = ggc_alloc_rtvec_sized (n);
  /* Clear out the vector.  */
  memset (&rt->elem[0], 0, n * sizeof (rtx));

  PUT_NUM_ELEM (rt, n);

  if (GATHER_STATISTICS)
    {
      rtvec_alloc_counts++;
      rtvec_alloc_sizes += n * sizeof (rtx);
    }

  return rt;
}

/* Create a bitwise copy of VEC.  */

rtvec
shallow_copy_rtvec (rtvec vec)
{
  rtvec newvec;
  int n;

  n = GET_NUM_ELEM (vec);
  newvec = rtvec_alloc (n);
  memcpy (&newvec->elem[0], &vec->elem[0], sizeof (rtx) * n);
  return newvec;
}

/* Return the number of bytes occupied by rtx value X.  */

unsigned int
rtx_size (const_rtx x)
{
  if (CONST_WIDE_INT_P (x))
    return (RTX_HDR_SIZE
	    + sizeof (struct hwivec_def)
	    + ((CONST_WIDE_INT_NUNITS (x) - 1)
	       * sizeof (HOST_WIDE_INT)));
  if (CONST_POLY_INT_P (x))
    return (RTX_HDR_SIZE
	    + sizeof (struct const_poly_int_def)
	    + CONST_POLY_INT_COEFFS (x).extra_size ());
  if (GET_CODE (x) == SYMBOL_REF && SYMBOL_REF_HAS_BLOCK_INFO_P (x))
    return RTX_HDR_SIZE + sizeof (struct block_symbol);
  return RTX_CODE_SIZE (GET_CODE (x));
}

/* Allocate an rtx of code CODE with EXTRA bytes in it.  The CODE is
   stored in the rtx; all the rest is initialized to zero.  */

rtx
rtx_alloc_stat_v (RTX_CODE code MEM_STAT_DECL, int extra)
{
  rtx rt = ggc_alloc_rtx_def_stat (RTX_CODE_SIZE (code) + extra
				   PASS_MEM_STAT);

  rtx_init (rt, code);

  if (GATHER_STATISTICS)
    {
      rtx_alloc_counts[code]++;
      rtx_alloc_sizes[code] += RTX_CODE_SIZE (code);
    }

  return rt;
}

/* Allocate an rtx of code CODE.  The CODE is stored in the rtx;
   all the rest is initialized to zero.  */

rtx
rtx_alloc (RTX_CODE code MEM_STAT_DECL)
{
  return rtx_alloc_stat_v (code PASS_MEM_STAT, 0);
}

/* Write the wide constant X to OUTFILE.  */

void
cwi_output_hex (FILE *outfile, const_rtx x)
{
  int i = CWI_GET_NUM_ELEM (x);
  gcc_assert (i > 0);
  if (CWI_ELT (x, i - 1) == 0)
    /* The HOST_WIDE_INT_PRINT_HEX prepends a 0x only if the val is
       non zero.  We want all numbers to have a 0x prefix.  */
    fprintf (outfile, "0x");
  fprintf (outfile, HOST_WIDE_INT_PRINT_HEX, CWI_ELT (x, --i));
  while (--i >= 0)
    fprintf (outfile, HOST_WIDE_INT_PRINT_PADDED_HEX, CWI_ELT (x, i));
}


/* Return true if ORIG is a sharable CONST.  */

bool
shared_const_p (const_rtx orig)
{
  gcc_assert (GET_CODE (orig) == CONST);

  /* CONST can be shared if it contains a SYMBOL_REF.  If it contains
     a LABEL_REF, it isn't sharable.  */
  poly_int64 offset;
  return (GET_CODE (XEXP (orig, 0)) == PLUS
	  && GET_CODE (XEXP (XEXP (orig, 0), 0)) == SYMBOL_REF
	  && poly_int_rtx_p (XEXP (XEXP (orig, 0), 1), &offset));
}


/* Create a new copy of an rtx.
   Recursively copies the operands of the rtx,
   except for those few rtx codes that are sharable.  */

rtx
copy_rtx (rtx orig)
{
  rtx copy;
  int i, j;
  RTX_CODE code;
  const char *format_ptr;

  code = GET_CODE (orig);

  switch (code)
    {
    case REG:
    case DEBUG_EXPR:
    case VALUE:
    CASE_CONST_ANY:
    case SYMBOL_REF:
    case CODE_LABEL:
    case PC:
    case CC0:
    case RETURN:
    case SIMPLE_RETURN:
    case SCRATCH:
      /* SCRATCH must be shared because they represent distinct values.  */
      return orig;
    case CLOBBER:
      /* Share clobbers of hard registers (like cc0), but do not share pseudo reg
         clobbers or clobbers of hard registers that originated as pseudos.
         This is needed to allow safe register renaming.  */
      if (REG_P (XEXP (orig, 0)) && REGNO (XEXP (orig, 0)) < FIRST_PSEUDO_REGISTER
	  && ORIGINAL_REGNO (XEXP (orig, 0)) == REGNO (XEXP (orig, 0)))
	return orig;
      break;

    case CONST:
      if (shared_const_p (orig))
	return orig;
      break;

      /* A MEM with a constant address is not sharable.  The problem is that
	 the constant address may need to be reloaded.  If the mem is shared,
	 then reloading one copy of this mem will cause all copies to appear
	 to have been reloaded.  */

    default:
      break;
    }

  /* Copy the various flags, fields, and other information.  We assume
     that all fields need copying, and then clear the fields that should
     not be copied.  That is the sensible default behavior, and forces
     us to explicitly document why we are *not* copying a flag.  */
  copy = shallow_copy_rtx (orig);

  format_ptr = GET_RTX_FORMAT (GET_CODE (copy));

  for (i = 0; i < GET_RTX_LENGTH (GET_CODE (copy)); i++)
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
      case 'p':
      case 's':
      case 'S':
      case 'T':
      case 'u':
      case 'B':
      case '0':
	/* These are left unchanged.  */
	break;

      default:
	gcc_unreachable ();
      }
  return copy;
}

/* Create a new copy of an rtx.  Only copy just one level.  */

rtx
shallow_copy_rtx (const_rtx orig MEM_STAT_DECL)
{
  const unsigned int size = rtx_size (orig);
  rtx const copy = ggc_alloc_rtx_def_stat (size PASS_MEM_STAT);
  memcpy (copy, orig, size);
  switch (GET_CODE (orig))
    {
      /* RTX codes copy_rtx_if_shared_1 considers are shareable,
	 the used flag is often used for other purposes.  */
    case REG:
    case DEBUG_EXPR:
    case VALUE:
    CASE_CONST_ANY:
    case SYMBOL_REF:
    case CODE_LABEL:
    case PC:
    case CC0:
    case RETURN:
    case SIMPLE_RETURN:
    case SCRATCH:
      break;
    default:
      /* For all other RTXes clear the used flag on the copy.  */
      RTX_FLAG (copy, used) = 0;
      break;
    }
  return copy;
}

/* Nonzero when we are generating CONCATs.  */
int generating_concat_p;

/* Nonzero when we are expanding trees to RTL.  */
int currently_expanding_to_rtl;



/* Same as rtx_equal_p, but call CB on each pair of rtx if CB is not NULL.
   When the callback returns true, we continue with the new pair.
   Whenever changing this function check if rtx_equal_p below doesn't need
   changing as well.  */

int
rtx_equal_p_cb (const_rtx x, const_rtx y, rtx_equal_p_callback_function cb)
{
  int i;
  int j;
  enum rtx_code code;
  const char *fmt;
  rtx nx, ny;

  if (x == y)
    return 1;
  if (x == 0 || y == 0)
    return 0;

  /* Invoke the callback first.  */
  if (cb != NULL
      && ((*cb) (&x, &y, &nx, &ny)))
    return rtx_equal_p_cb (nx, ny, cb);

  code = GET_CODE (x);
  /* Rtx's of different codes cannot be equal.  */
  if (code != GET_CODE (y))
    return 0;

  /* (MULT:SI x y) and (MULT:HI x y) are NOT equivalent.
     (REG:SI x) and (REG:HI x) are NOT equivalent.  */

  if (GET_MODE (x) != GET_MODE (y))
    return 0;

  /* MEMs referring to different address space are not equivalent.  */
  if (code == MEM && MEM_ADDR_SPACE (x) != MEM_ADDR_SPACE (y))
    return 0;

  /* Some RTL can be compared nonrecursively.  */
  switch (code)
    {
    case REG:
      return (REGNO (x) == REGNO (y));

    case LABEL_REF:
      return label_ref_label (x) == label_ref_label (y);

    case SYMBOL_REF:
      return XSTR (x, 0) == XSTR (y, 0);

    case DEBUG_EXPR:
    case VALUE:
    case SCRATCH:
    CASE_CONST_UNIQUE:
      return 0;

    case DEBUG_IMPLICIT_PTR:
      return DEBUG_IMPLICIT_PTR_DECL (x)
	     == DEBUG_IMPLICIT_PTR_DECL (y);

    case DEBUG_PARAMETER_REF:
      return DEBUG_PARAMETER_REF_DECL (x)
	     == DEBUG_PARAMETER_REF_DECL (y);

    case ENTRY_VALUE:
      return rtx_equal_p_cb (ENTRY_VALUE_EXP (x), ENTRY_VALUE_EXP (y), cb);

    default:
      break;
    }

  /* Compare the elements.  If any pair of corresponding elements
     fail to match, return 0 for the whole thing.  */

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
	    {
#ifndef GENERATOR_FILE
	      if (((code == ASM_OPERANDS && i == 6)
		   || (code == ASM_INPUT && i == 1))
		  && XINT (x, i) == XINT (y, i))
		break;
#endif
	      return 0;
	    }
	  break;

	case 'p':
	  if (maybe_ne (SUBREG_BYTE (x), SUBREG_BYTE (y)))
	    return 0;
	  break;

	case 'V':
	case 'E':
	  /* Two vectors must have the same length.  */
	  if (XVECLEN (x, i) != XVECLEN (y, i))
	    return 0;

	  /* And the corresponding elements must match.  */
	  for (j = 0; j < XVECLEN (x, i); j++)
	    if (rtx_equal_p_cb (XVECEXP (x, i, j),
                                XVECEXP (y, i, j), cb) == 0)
	      return 0;
	  break;

	case 'e':
	  if (rtx_equal_p_cb (XEXP (x, i), XEXP (y, i), cb) == 0)
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
	  gcc_unreachable ();
	}
    }
  return 1;
}

/* Return 1 if X and Y are identical-looking rtx's.
   This is the Lisp function EQUAL for rtx arguments.
   Whenever changing this function check if rtx_equal_p_cb above doesn't need
   changing as well.  */

int
rtx_equal_p (const_rtx x, const_rtx y)
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

  /* MEMs referring to different address space are not equivalent.  */
  if (code == MEM && MEM_ADDR_SPACE (x) != MEM_ADDR_SPACE (y))
    return 0;

  /* Some RTL can be compared nonrecursively.  */
  switch (code)
    {
    case REG:
      return (REGNO (x) == REGNO (y));

    case LABEL_REF:
      return label_ref_label (x) == label_ref_label (y);

    case SYMBOL_REF:
      return XSTR (x, 0) == XSTR (y, 0);

    case DEBUG_EXPR:
    case VALUE:
    case SCRATCH:
    CASE_CONST_UNIQUE:
      return 0;

    case DEBUG_IMPLICIT_PTR:
      return DEBUG_IMPLICIT_PTR_DECL (x)
	     == DEBUG_IMPLICIT_PTR_DECL (y);

    case DEBUG_PARAMETER_REF:
      return DEBUG_PARAMETER_REF_DECL (x)
	     == DEBUG_PARAMETER_REF_DECL (y);

    case ENTRY_VALUE:
      return rtx_equal_p (ENTRY_VALUE_EXP (x), ENTRY_VALUE_EXP (y));

    default:
      break;
    }

  /* Compare the elements.  If any pair of corresponding elements
     fail to match, return 0 for the whole thing.  */

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
	    {
#ifndef GENERATOR_FILE
	      if (((code == ASM_OPERANDS && i == 6)
		   || (code == ASM_INPUT && i == 1))
		  && XINT (x, i) == XINT (y, i))
		break;
#endif
	      return 0;
	    }
	  break;

	case 'p':
	  if (maybe_ne (SUBREG_BYTE (x), SUBREG_BYTE (y)))
	    return 0;
	  break;

	case 'V':
	case 'E':
	  /* Two vectors must have the same length.  */
	  if (XVECLEN (x, i) != XVECLEN (y, i))
	    return 0;

	  /* And the corresponding elements must match.  */
	  for (j = 0; j < XVECLEN (x, i); j++)
	    if (rtx_equal_p (XVECEXP (x, i, j),  XVECEXP (y, i, j)) == 0)
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
	  gcc_unreachable ();
	}
    }
  return 1;
}

/* Return true if all elements of VEC are equal.  */

bool
rtvec_all_equal_p (const_rtvec vec)
{
  const_rtx first = RTVEC_ELT (vec, 0);
  /* Optimize the important special case of a vector of constants.
     The main use of this function is to detect whether every element
     of CONST_VECTOR is the same.  */
  switch (GET_CODE (first))
    {
    CASE_CONST_UNIQUE:
      for (int i = 1, n = GET_NUM_ELEM (vec); i < n; ++i)
	if (first != RTVEC_ELT (vec, i))
	  return false;
      return true;

    default:
      for (int i = 1, n = GET_NUM_ELEM (vec); i < n; ++i)
	if (!rtx_equal_p (first, RTVEC_ELT (vec, i)))
	  return false;
      return true;
    }
}

/* Return an indication of which type of insn should have X as a body.
   In generator files, this can be UNKNOWN if the answer is only known
   at (GCC) runtime.  Otherwise the value is CODE_LABEL, INSN, CALL_INSN
   or JUMP_INSN.  */

enum rtx_code
classify_insn (rtx x)
{
  if (LABEL_P (x))
    return CODE_LABEL;
  if (GET_CODE (x) == CALL)
    return CALL_INSN;
  if (ANY_RETURN_P (x))
    return JUMP_INSN;
  if (GET_CODE (x) == ASM_OPERANDS && ASM_OPERANDS_LABEL_VEC (x))
    return JUMP_INSN;
  if (GET_CODE (x) == SET)
    {
      if (GET_CODE (SET_DEST (x)) == PC)
	return JUMP_INSN;
      else if (GET_CODE (SET_SRC (x)) == CALL)
	return CALL_INSN;
      else
	return INSN;
    }
  if (GET_CODE (x) == PARALLEL)
    {
      int j;
      bool has_return_p = false;
      for (j = XVECLEN (x, 0) - 1; j >= 0; j--)
	if (GET_CODE (XVECEXP (x, 0, j)) == CALL)
	  return CALL_INSN;
	else if (ANY_RETURN_P (XVECEXP (x, 0, j)))
	  has_return_p = true;
	else if (GET_CODE (XVECEXP (x, 0, j)) == SET
		 && GET_CODE (SET_DEST (XVECEXP (x, 0, j))) == PC)
	  return JUMP_INSN;
	else if (GET_CODE (XVECEXP (x, 0, j)) == SET
		 && GET_CODE (SET_SRC (XVECEXP (x, 0, j))) == CALL)
	  return CALL_INSN;
      if (has_return_p)
	return JUMP_INSN;
      if (GET_CODE (XVECEXP (x, 0, 0)) == ASM_OPERANDS
	  && ASM_OPERANDS_LABEL_VEC (XVECEXP (x, 0, 0)))
	return JUMP_INSN;
    }
#ifdef GENERATOR_FILE
  if (GET_CODE (x) == MATCH_OPERAND
      || GET_CODE (x) == MATCH_OPERATOR
      || GET_CODE (x) == MATCH_PARALLEL
      || GET_CODE (x) == MATCH_OP_DUP
      || GET_CODE (x) == MATCH_DUP
      || GET_CODE (x) == PARALLEL)
    return UNKNOWN;
#endif
  return INSN;
}

/* Comparator of indices based on rtx_alloc_counts.  */

static int
rtx_count_cmp (const void *p1, const void *p2)
{
  const unsigned *n1 = (const unsigned *)p1;
  const unsigned *n2 = (const unsigned *)p2;

  return rtx_alloc_counts[*n1] - rtx_alloc_counts[*n2];
}

void
dump_rtx_statistics (void)
{
  int total_counts = 0;
  int total_sizes = 0;

  if (! GATHER_STATISTICS)
    {
      fprintf (stderr, "No RTX statistics\n");
      return;
    }

  fprintf (stderr, "\nRTX Kind                   Count     Bytes\n");
  fprintf (stderr, "-------------------------------------------\n");

  auto_vec<unsigned> indices (LAST_AND_UNUSED_RTX_CODE);
  for (unsigned i = 0; i < LAST_AND_UNUSED_RTX_CODE; i++)
    indices.quick_push (i);
  indices.qsort (rtx_count_cmp);

  for (unsigned i = 0; i < LAST_AND_UNUSED_RTX_CODE; i++)
    {
      unsigned j = indices[i];
      if (rtx_alloc_counts[j])
	{
	  fprintf (stderr, "%-24s " PRsa (6) " " PRsa (9) "\n",
		   GET_RTX_NAME (j),
		   SIZE_AMOUNT (rtx_alloc_counts[j]),
		   SIZE_AMOUNT (rtx_alloc_sizes[j]));
	  total_counts += rtx_alloc_counts[j];
	  total_sizes += rtx_alloc_sizes[j];
	}
    }

  if (rtvec_alloc_counts)
    {
      fprintf (stderr, "%-24s " PRsa (6) " " PRsa (9) "\n", "rtvec",
	       SIZE_AMOUNT (rtvec_alloc_counts),
	       SIZE_AMOUNT (rtvec_alloc_sizes));
      total_counts += rtvec_alloc_counts;
      total_sizes += rtvec_alloc_sizes;
    }
  fprintf (stderr, "-----------------------------------------------\n");
  fprintf (stderr, "%-24s " PRsa (6) " " PRsa (9) "\n",
	   "Total", SIZE_AMOUNT (total_counts),
	   SIZE_AMOUNT (total_sizes));
  fprintf (stderr, "-----------------------------------------------\n");
}

#if defined ENABLE_RTL_CHECKING && (GCC_VERSION >= 2007)
void
rtl_check_failed_bounds (const_rtx r, int n, const char *file, int line,
			 const char *func)
{
  internal_error
    ("RTL check: access of elt %d of '%s' with last elt %d in %s, at %s:%d",
     n, GET_RTX_NAME (GET_CODE (r)), GET_RTX_LENGTH (GET_CODE (r)) - 1,
     func, trim_filename (file), line);
}

void
rtl_check_failed_type1 (const_rtx r, int n, int c1, const char *file, int line,
			const char *func)
{
  internal_error
    ("RTL check: expected elt %d type '%c', have '%c' (rtx %s) in %s, at %s:%d",
     n, c1, GET_RTX_FORMAT (GET_CODE (r))[n], GET_RTX_NAME (GET_CODE (r)),
     func, trim_filename (file), line);
}

void
rtl_check_failed_type2 (const_rtx r, int n, int c1, int c2, const char *file,
			int line, const char *func)
{
  internal_error
    ("RTL check: expected elt %d type '%c' or '%c', have '%c' (rtx %s) in %s, at %s:%d",
     n, c1, c2, GET_RTX_FORMAT (GET_CODE (r))[n], GET_RTX_NAME (GET_CODE (r)),
     func, trim_filename (file), line);
}

void
rtl_check_failed_code1 (const_rtx r, enum rtx_code code, const char *file,
			int line, const char *func)
{
  internal_error ("RTL check: expected code '%s', have '%s' in %s, at %s:%d",
		  GET_RTX_NAME (code), GET_RTX_NAME (GET_CODE (r)), func,
		  trim_filename (file), line);
}

void
rtl_check_failed_code2 (const_rtx r, enum rtx_code code1, enum rtx_code code2,
			const char *file, int line, const char *func)
{
  internal_error
    ("RTL check: expected code '%s' or '%s', have '%s' in %s, at %s:%d",
     GET_RTX_NAME (code1), GET_RTX_NAME (code2), GET_RTX_NAME (GET_CODE (r)),
     func, trim_filename (file), line);
}

void
rtl_check_failed_code3 (const_rtx r, enum rtx_code code1, enum rtx_code code2,
			enum rtx_code code3, const char *file, int line,
			const char *func)
{
  internal_error
    ("RTL check: expected code '%s', '%s' or '%s', have '%s' in %s, at %s:%d",
     GET_RTX_NAME (code1), GET_RTX_NAME (code2), GET_RTX_NAME (code3),
     GET_RTX_NAME (GET_CODE (r)), func, trim_filename (file), line);
}

void
rtl_check_failed_code_mode (const_rtx r, enum rtx_code code, machine_mode mode,
			    bool not_mode, const char *file, int line,
			    const char *func)
{
  internal_error ((not_mode
		   ? ("RTL check: expected code '%s' and not mode '%s', "
		      "have code '%s' and mode '%s' in %s, at %s:%d")
		   : ("RTL check: expected code '%s' and mode '%s', "
		      "have code '%s' and mode '%s' in %s, at %s:%d")),
		  GET_RTX_NAME (code), GET_MODE_NAME (mode),
		  GET_RTX_NAME (GET_CODE (r)), GET_MODE_NAME (GET_MODE (r)),
		  func, trim_filename (file), line);
}

/* Report that line LINE of FILE tried to access the block symbol fields
   of a non-block symbol.  FUNC is the function that contains the line.  */

void
rtl_check_failed_block_symbol (const char *file, int line, const char *func)
{
  internal_error
    ("RTL check: attempt to treat non-block symbol as a block symbol "
     "in %s, at %s:%d", func, trim_filename (file), line);
}

/* XXX Maybe print the vector?  */
void
cwi_check_failed_bounds (const_rtx x, int n, const char *file, int line,
			 const char *func)
{
  internal_error
    ("RTL check: access of hwi elt %d of vector with last elt %d in %s, at %s:%d",
     n, CWI_GET_NUM_ELEM (x) - 1, func, trim_filename (file), line);
}

/* XXX Maybe print the vector?  */
void
rtvec_check_failed_bounds (const_rtvec r, int n, const char *file, int line,
			   const char *func)
{
  internal_error
    ("RTL check: access of elt %d of vector with last elt %d in %s, at %s:%d",
     n, GET_NUM_ELEM (r) - 1, func, trim_filename (file), line);
}
#endif /* ENABLE_RTL_CHECKING */

#if defined ENABLE_RTL_FLAG_CHECKING
void
rtl_check_failed_flag (const char *name, const_rtx r, const char *file,
		       int line, const char *func)
{
  internal_error
    ("RTL flag check: %s used with unexpected rtx code '%s' in %s, at %s:%d",
     name, GET_RTX_NAME (GET_CODE (r)), func, trim_filename (file), line);
}
#endif /* ENABLE_RTL_FLAG_CHECKING */
