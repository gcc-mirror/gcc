/* Emit RTL for the GNU C-Compiler expander.
   Copyright (C) 1987, 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998,
   1999, 2000 Free Software Foundation, Inc.

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


/* Middle-to-low level generation of rtx code and insns.

   This file contains the functions `gen_rtx', `gen_reg_rtx'
   and `gen_label_rtx' that are the usual ways of creating rtl
   expressions for most purposes.

   It also has the functions for creating insns and linking
   them in the doubly-linked chain.

   The patterns of the insns are created by machine-dependent
   routines in insn-emit.c, which is generated automatically from
   the machine description.  These routines use `gen_rtx' to make
   the individual rtx's of the pattern; what is machine dependent
   is the kind of rtx's they make and what arguments they use.  */

#include "config.h"
#include "system.h"
#include "toplev.h"
#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "flags.h"
#include "function.h"
#include "expr.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "hashtab.h"
#include "insn-config.h"
#include "recog.h"
#include "real.h"
#include "obstack.h"
#include "bitmap.h"
#include "basic-block.h"
#include "ggc.h"

/* Commonly used modes.  */

enum machine_mode byte_mode;	/* Mode whose width is BITS_PER_UNIT.  */
enum machine_mode word_mode;	/* Mode whose width is BITS_PER_WORD.  */
enum machine_mode double_mode;	/* Mode whose width is DOUBLE_TYPE_SIZE.  */
enum machine_mode ptr_mode;	/* Mode whose width is POINTER_SIZE.  */


/* This is *not* reset after each function.  It gives each CODE_LABEL
   in the entire compilation a unique label number.  */

static int label_num = 1;

/* Highest label number in current function.
   Zero means use the value of label_num instead.
   This is nonzero only when belatedly compiling an inline function.  */

static int last_label_num;

/* Value label_num had when set_new_first_and_last_label_number was called.
   If label_num has not changed since then, last_label_num is valid.  */

static int base_label_num;

/* Nonzero means do not generate NOTEs for source line numbers.  */

static int no_line_numbers;

/* Commonly used rtx's, so that we only need space for one copy.
   These are initialized once for the entire compilation.
   All of these except perhaps the floating-point CONST_DOUBLEs
   are unique; no other rtx-object will be equal to any of these.  */

rtx global_rtl[GR_MAX];

/* We record floating-point CONST_DOUBLEs in each floating-point mode for
   the values of 0, 1, and 2.  For the integer entries and VOIDmode, we
   record a copy of const[012]_rtx.  */

rtx const_tiny_rtx[3][(int) MAX_MACHINE_MODE];

rtx const_true_rtx;

REAL_VALUE_TYPE dconst0;
REAL_VALUE_TYPE dconst1;
REAL_VALUE_TYPE dconst2;
REAL_VALUE_TYPE dconstm1;

/* All references to the following fixed hard registers go through
   these unique rtl objects.  On machines where the frame-pointer and
   arg-pointer are the same register, they use the same unique object.

   After register allocation, other rtl objects which used to be pseudo-regs
   may be clobbered to refer to the frame-pointer register.
   But references that were originally to the frame-pointer can be
   distinguished from the others because they contain frame_pointer_rtx.

   When to use frame_pointer_rtx and hard_frame_pointer_rtx is a little
   tricky: until register elimination has taken place hard_frame_pointer_rtx
   should be used if it is being set, and frame_pointer_rtx otherwise.  After 
   register elimination hard_frame_pointer_rtx should always be used.
   On machines where the two registers are same (most) then these are the
   same.

   In an inline procedure, the stack and frame pointer rtxs may not be
   used for anything else.  */
rtx struct_value_rtx;		/* (REG:Pmode STRUCT_VALUE_REGNUM) */
rtx struct_value_incoming_rtx;	/* (REG:Pmode STRUCT_VALUE_INCOMING_REGNUM) */
rtx static_chain_rtx;		/* (REG:Pmode STATIC_CHAIN_REGNUM) */
rtx static_chain_incoming_rtx;	/* (REG:Pmode STATIC_CHAIN_INCOMING_REGNUM) */
rtx pic_offset_table_rtx;	/* (REG:Pmode PIC_OFFSET_TABLE_REGNUM) */

/* This is used to implement __builtin_return_address for some machines.
   See for instance the MIPS port.  */
rtx return_address_pointer_rtx;	/* (REG:Pmode RETURN_ADDRESS_POINTER_REGNUM) */

/* We make one copy of (const_int C) where C is in
   [- MAX_SAVED_CONST_INT, MAX_SAVED_CONST_INT]
   to save space during the compilation and simplify comparisons of
   integers.  */

rtx const_int_rtx[MAX_SAVED_CONST_INT * 2 + 1];

/* A hash table storing CONST_INTs whose absolute value is greater
   than MAX_SAVED_CONST_INT.  */

static htab_t const_int_htab;

/* start_sequence and gen_sequence can make a lot of rtx expressions which are
   shortly thrown away.  We use two mechanisms to prevent this waste:

   For sizes up to 5 elements, we keep a SEQUENCE and its associated
   rtvec for use by gen_sequence.  One entry for each size is
   sufficient because most cases are calls to gen_sequence followed by
   immediately emitting the SEQUENCE.  Reuse is safe since emitting a
   sequence is destructive on the insn in it anyway and hence can't be
   redone.

   We do not bother to save this cached data over nested function calls.
   Instead, we just reinitialize them.  */

#define SEQUENCE_RESULT_SIZE 5

static rtx sequence_result[SEQUENCE_RESULT_SIZE];

/* During RTL generation, we also keep a list of free INSN rtl codes.  */
static rtx free_insn;

#define first_insn (cfun->emit->x_first_insn)
#define last_insn (cfun->emit->x_last_insn)
#define cur_insn_uid (cfun->emit->x_cur_insn_uid)
#define last_linenum (cfun->emit->x_last_linenum)
#define last_filename (cfun->emit->x_last_filename)
#define first_label_num (cfun->emit->x_first_label_num)

/* This is where the pointer to the obstack being used for RTL is stored.  */
extern struct obstack *rtl_obstack;

static rtx make_jump_insn_raw		PARAMS ((rtx));
static rtx make_call_insn_raw		PARAMS ((rtx));
static rtx find_line_note		PARAMS ((rtx));
static void mark_sequence_stack         PARAMS ((struct sequence_stack *));
static void unshare_all_rtl_1		PARAMS ((rtx));
static hashval_t const_int_htab_hash    PARAMS ((const void *));
static int const_int_htab_eq            PARAMS ((const void *,
						 const void *));
static int rtx_htab_mark_1              PARAMS ((void **, void *));
static void rtx_htab_mark               PARAMS ((void *));


/* Returns a hash code for X (which is a really a CONST_INT).  */

static hashval_t
const_int_htab_hash (x)
     const void *x;
{
  return (hashval_t) INTVAL ((rtx) x);
}

/* Returns non-zero if the value represented by X (which is really a
   CONST_INT) is the same as that given by Y (which is really a
   HOST_WIDE_INT *).  */

static int
const_int_htab_eq (x, y)
     const void *x;
     const void *y;
{
  return (INTVAL ((rtx) x) == *((HOST_WIDE_INT *) y));
}

/* Mark the hash-table element X (which is really a pointer to an
   rtx).  */

static int
rtx_htab_mark_1 (x, data)
     void **x;
     void *data ATTRIBUTE_UNUSED;
{
  ggc_mark_rtx (*x);
  return 1;
}

/* Mark all the elements of HTAB (which is really an htab_t full of
   rtxs).  */

static void
rtx_htab_mark (htab)
     void *htab;
{
  htab_traverse (*((htab_t *) htab), rtx_htab_mark_1, NULL);
}

/* There are some RTL codes that require special attention; the generation
   functions do the raw handling.  If you add to this list, modify
   special_rtx in gengenrtl.c as well.  */

rtx
gen_rtx_CONST_INT (mode, arg)
     enum machine_mode mode ATTRIBUTE_UNUSED;
     HOST_WIDE_INT arg;
{
  void **slot;

  if (arg >= - MAX_SAVED_CONST_INT && arg <= MAX_SAVED_CONST_INT)
    return const_int_rtx[arg + MAX_SAVED_CONST_INT];

#if STORE_FLAG_VALUE != 1 && STORE_FLAG_VALUE != -1
  if (const_true_rtx && arg == STORE_FLAG_VALUE)
    return const_true_rtx;
#endif

  /* Look up the CONST_INT in the hash table.  */
  slot = htab_find_slot_with_hash (const_int_htab, 
				   &arg,
				   (hashval_t) arg,
				   /*insert=*/1);
  if (!*slot)
    {
      if (!ggc_p)
	{
	  push_obstacks_nochange ();
	  end_temporary_allocation ();
	}
      *slot = gen_rtx_raw_CONST_INT (VOIDmode, arg);
      if (!ggc_p)
	pop_obstacks ();
    }

  return (rtx) *slot;
}

/* CONST_DOUBLEs needs special handling because its length is known
   only at run-time.  */
rtx
gen_rtx_CONST_DOUBLE (mode, arg0, arg1, arg2)
     enum machine_mode mode;
     rtx arg0;
     HOST_WIDE_INT arg1, arg2;
{
  rtx r = rtx_alloc (CONST_DOUBLE);
  int i;

  PUT_MODE (r, mode);
  XEXP (r, 0) = arg0;
  X0EXP (r, 1) = NULL_RTX;
  XWINT (r, 2) = arg1;
  XWINT (r, 3) = arg2;

  for (i = GET_RTX_LENGTH (CONST_DOUBLE) - 1; i > 3; --i)
    XWINT (r, i) = 0;

  return r;
}

rtx
gen_rtx_REG (mode, regno)
     enum machine_mode mode;
     int regno;
{
  /* In case the MD file explicitly references the frame pointer, have
     all such references point to the same frame pointer.  This is
     used during frame pointer elimination to distinguish the explicit
     references to these registers from pseudos that happened to be
     assigned to them.

     If we have eliminated the frame pointer or arg pointer, we will
     be using it as a normal register, for example as a spill
     register.  In such cases, we might be accessing it in a mode that
     is not Pmode and therefore cannot use the pre-allocated rtx.

     Also don't do this when we are making new REGs in reload, since
     we don't want to get confused with the real pointers.  */

  if (mode == Pmode && !reload_in_progress)
    {
      if (regno == FRAME_POINTER_REGNUM)
	return frame_pointer_rtx;
#if FRAME_POINTER_REGNUM != HARD_FRAME_POINTER_REGNUM
      if (regno == HARD_FRAME_POINTER_REGNUM)
	return hard_frame_pointer_rtx;
#endif
#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM && HARD_FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
      if (regno == ARG_POINTER_REGNUM)
	return arg_pointer_rtx;
#endif
#ifdef RETURN_ADDRESS_POINTER_REGNUM
      if (regno == RETURN_ADDRESS_POINTER_REGNUM)
	return return_address_pointer_rtx;
#endif
      if (regno == STACK_POINTER_REGNUM)
	return stack_pointer_rtx;
    }

  return gen_rtx_raw_REG (mode, regno);
}

rtx
gen_rtx_MEM (mode, addr)
     enum machine_mode mode;
     rtx addr;
{
  rtx rt = gen_rtx_raw_MEM (mode, addr);

  /* This field is not cleared by the mere allocation of the rtx, so
     we clear it here.  */
  MEM_ALIAS_SET (rt) = 0;

  return rt;
}

/* rtx gen_rtx (code, mode, [element1, ..., elementn])
**
**	    This routine generates an RTX of the size specified by
**	<code>, which is an RTX code.   The RTX structure is initialized
**	from the arguments <element1> through <elementn>, which are
**	interpreted according to the specific RTX type's format.   The
**	special machine mode associated with the rtx (if any) is specified
**	in <mode>.
**
**	    gen_rtx can be invoked in a way which resembles the lisp-like
**	rtx it will generate.   For example, the following rtx structure:
**
**	      (plus:QI (mem:QI (reg:SI 1))
**		       (mem:QI (plusw:SI (reg:SI 2) (reg:SI 3))))
**
**		...would be generated by the following C code:
**
**	    	gen_rtx (PLUS, QImode,
**		    gen_rtx (MEM, QImode,
**			gen_rtx (REG, SImode, 1)),
**		    gen_rtx (MEM, QImode,
**			gen_rtx (PLUS, SImode,
**			    gen_rtx (REG, SImode, 2),
**			    gen_rtx (REG, SImode, 3)))),
*/

/*VARARGS2*/
rtx
gen_rtx VPARAMS ((enum rtx_code code, enum machine_mode mode, ...))
{
#ifndef ANSI_PROTOTYPES
  enum rtx_code code;
  enum machine_mode mode;
#endif
  va_list p;
  register int i;		/* Array indices...			*/
  register const char *fmt;	/* Current rtx's format...		*/
  register rtx rt_val;		/* RTX to return to caller...		*/

  VA_START (p, mode);

#ifndef ANSI_PROTOTYPES
  code = va_arg (p, enum rtx_code);
  mode = va_arg (p, enum machine_mode);
#endif

  switch (code)
    {
    case CONST_INT:
      rt_val = gen_rtx_CONST_INT (mode, va_arg (p, HOST_WIDE_INT));
      break;

    case CONST_DOUBLE:
      {
	rtx arg0 = va_arg (p, rtx);
	HOST_WIDE_INT arg1 = va_arg (p, HOST_WIDE_INT);
	HOST_WIDE_INT arg2 = va_arg (p, HOST_WIDE_INT);
        rt_val = gen_rtx_CONST_DOUBLE (mode, arg0, arg1, arg2);
      }
      break;

    case REG:
      rt_val = gen_rtx_REG (mode, va_arg (p, int));
      break;

    case MEM:
      rt_val = gen_rtx_MEM (mode, va_arg (p, rtx));
      break;

    default:
      rt_val = rtx_alloc (code);	/* Allocate the storage space.  */
      rt_val->mode = mode;		/* Store the machine mode...  */

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

	    case 'w':		/* A wide integer? */
	      XWINT (rt_val, i) = va_arg (p, HOST_WIDE_INT);
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

	    case 'b':           /* A bitmap? */
	      XBITMAP (rt_val, i) = va_arg (p, bitmap);
	      break;

	    case 't':           /* A tree? */
	      XTREE (rt_val, i) = va_arg (p, tree);
	      break;

	    default:
	      abort ();
	    }
	}
      break;
    }

  va_end (p);
  return rt_val;
}

/* gen_rtvec (n, [rt1, ..., rtn])
**
**	    This routine creates an rtvec and stores within it the
**	pointers to rtx's which are its arguments.
*/

/*VARARGS1*/
rtvec
gen_rtvec VPARAMS ((int n, ...))
{
#ifndef ANSI_PROTOTYPES
  int n;
#endif
  int i;
  va_list p;
  rtx *vector;

  VA_START (p, n);

#ifndef ANSI_PROTOTYPES
  n = va_arg (p, int);
#endif

  if (n == 0)
    return NULL_RTVEC;		/* Don't allocate an empty rtvec...	*/

  vector = (rtx *) alloca (n * sizeof (rtx));

  for (i = 0; i < n; i++)
    vector[i] = va_arg (p, rtx);
  va_end (p);

  return gen_rtvec_v (n, vector);
}

rtvec
gen_rtvec_v (n, argp)
     int n;
     rtx *argp;
{
  register int i;
  register rtvec rt_val;

  if (n == 0)
    return NULL_RTVEC;		/* Don't allocate an empty rtvec...	*/

  rt_val = rtvec_alloc (n);	/* Allocate an rtvec...			*/

  for (i = 0; i < n; i++)
    rt_val->elem[i] = *argp++;

  return rt_val;
}


/* Generate a REG rtx for a new pseudo register of mode MODE.
   This pseudo is assigned the next sequential register number.  */

rtx
gen_reg_rtx (mode)
     enum machine_mode mode;
{
  struct function *f = cfun;
  register rtx val;

  /* Don't let anything called after initial flow analysis create new
     registers.  */
  if (no_new_pseudos)
    abort ();

  if (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT
      || GET_MODE_CLASS (mode) == MODE_COMPLEX_INT)
    {
      /* For complex modes, don't make a single pseudo.
	 Instead, make a CONCAT of two pseudos.
	 This allows noncontiguous allocation of the real and imaginary parts,
	 which makes much better code.  Besides, allocating DCmode
	 pseudos overstrains reload on some machines like the 386.  */
      rtx realpart, imagpart;
      int size = GET_MODE_UNIT_SIZE (mode);
      enum machine_mode partmode
	= mode_for_size (size * BITS_PER_UNIT,
			 (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT
			  ? MODE_FLOAT : MODE_INT),
			 0);

      realpart = gen_reg_rtx (partmode);
      imagpart = gen_reg_rtx (partmode);
      return gen_rtx_CONCAT (mode, realpart, imagpart);
    }

  /* Make sure regno_pointer_flag and regno_reg_rtx are large
     enough to have an element for this pseudo reg number.  */

  if (reg_rtx_no == f->emit->regno_pointer_flag_length)
    {
      int old_size = f->emit->regno_pointer_flag_length;
      rtx *new1;
      char *new;
      new = xrealloc (f->emit->regno_pointer_flag, old_size * 2);
      memset (new + old_size, 0, old_size);
      f->emit->regno_pointer_flag = new;

      new = xrealloc (f->emit->regno_pointer_align, old_size * 2);
      memset (new + old_size, 0, old_size);
      f->emit->regno_pointer_align = new;

      new1 = (rtx *) xrealloc (f->emit->x_regno_reg_rtx,
			       old_size * 2 * sizeof (rtx));
      memset (new1 + old_size, 0, old_size * sizeof (rtx));
      regno_reg_rtx = new1;

      f->emit->regno_pointer_flag_length = old_size * 2;
    }

  val = gen_rtx_raw_REG (mode, reg_rtx_no);
  regno_reg_rtx[reg_rtx_no++] = val;
  return val;
}

/* Identify REG (which may be a CONCAT) as a user register.  */

void
mark_user_reg (reg)
     rtx reg;
{
  if (GET_CODE (reg) == CONCAT)
    {
      REG_USERVAR_P (XEXP (reg, 0)) = 1;
      REG_USERVAR_P (XEXP (reg, 1)) = 1;
    }
  else if (GET_CODE (reg) == REG)
    REG_USERVAR_P (reg) = 1;
  else
    abort ();
}

/* Identify REG as a probable pointer register and show its alignment
   as ALIGN, if nonzero.  */

void
mark_reg_pointer (reg, align)
     rtx reg;
     int align;
{
  if (! REGNO_POINTER_FLAG (REGNO (reg)))
    {
      REGNO_POINTER_FLAG (REGNO (reg)) = 1;

      if (align)
	REGNO_POINTER_ALIGN (REGNO (reg)) = align;
    }
  else if (align && align < REGNO_POINTER_ALIGN (REGNO (reg)))
    /* We can no-longer be sure just how aligned this pointer is */
    REGNO_POINTER_ALIGN (REGNO (reg)) = align;
}

/* Return 1 plus largest pseudo reg number used in the current function.  */

int
max_reg_num ()
{
  return reg_rtx_no;
}

/* Return 1 + the largest label number used so far in the current function.  */

int
max_label_num ()
{
  if (last_label_num && label_num == base_label_num)
    return last_label_num;
  return label_num;
}

/* Return first label number used in this function (if any were used).  */

int
get_first_label_num ()
{
  return first_label_num;
}

/* Return a value representing some low-order bits of X, where the number
   of low-order bits is given by MODE.  Note that no conversion is done
   between floating-point and fixed-point values, rather, the bit 
   representation is returned.

   This function handles the cases in common between gen_lowpart, below,
   and two variants in cse.c and combine.c.  These are the cases that can
   be safely handled at all points in the compilation.

   If this is not a case we can handle, return 0.  */

rtx
gen_lowpart_common (mode, x)
     enum machine_mode mode;
     register rtx x;
{
  int word = 0;

  if (GET_MODE (x) == mode)
    return x;

  /* MODE must occupy no more words than the mode of X.  */
  if (GET_MODE (x) != VOIDmode
      && ((GET_MODE_SIZE (mode) + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD
	  > ((GET_MODE_SIZE (GET_MODE (x)) + (UNITS_PER_WORD - 1))
	     / UNITS_PER_WORD)))
    return 0;

  if (WORDS_BIG_ENDIAN && GET_MODE_SIZE (GET_MODE (x)) > UNITS_PER_WORD)
    word = ((GET_MODE_SIZE (GET_MODE (x))
	     - MAX (GET_MODE_SIZE (mode), UNITS_PER_WORD))
	    / UNITS_PER_WORD);

  if ((GET_CODE (x) == ZERO_EXTEND || GET_CODE (x) == SIGN_EXTEND)
      && (GET_MODE_CLASS (mode) == MODE_INT
	  || GET_MODE_CLASS (mode) == MODE_PARTIAL_INT))
    {
      /* If we are getting the low-order part of something that has been
	 sign- or zero-extended, we can either just use the object being
	 extended or make a narrower extension.  If we want an even smaller
	 piece than the size of the object being extended, call ourselves
	 recursively.

	 This case is used mostly by combine and cse.  */

      if (GET_MODE (XEXP (x, 0)) == mode)
	return XEXP (x, 0);
      else if (GET_MODE_SIZE (mode) < GET_MODE_SIZE (GET_MODE (XEXP (x, 0))))
	return gen_lowpart_common (mode, XEXP (x, 0));
      else if (GET_MODE_SIZE (mode) < GET_MODE_SIZE (GET_MODE (x)))
	return gen_rtx_fmt_e (GET_CODE (x), mode, XEXP (x, 0));
    }
  else if (GET_CODE (x) == SUBREG
	   && (GET_MODE_SIZE (mode) <= UNITS_PER_WORD
	       || GET_MODE_SIZE (mode) == GET_MODE_UNIT_SIZE (GET_MODE (x))))
    return (GET_MODE (SUBREG_REG (x)) == mode && SUBREG_WORD (x) == 0
	    ? SUBREG_REG (x)
	    : gen_rtx_SUBREG (mode, SUBREG_REG (x), SUBREG_WORD (x) + word));
  else if (GET_CODE (x) == REG)
    {
      /* Let the backend decide how many registers to skip.  This is needed
         in particular for Sparc64 where fp regs are smaller than a word.  */
      /* ??? Note that subregs are now ambiguous, in that those against
	 pseudos are sized by the Word Size, while those against hard
	 regs are sized by the underlying register size.  Better would be
	 to always interpret the subreg offset parameter as bytes or bits.  */

      if (WORDS_BIG_ENDIAN && REGNO (x) < FIRST_PSEUDO_REGISTER)
	word = (HARD_REGNO_NREGS (REGNO (x), GET_MODE (x))
		- HARD_REGNO_NREGS (REGNO (x), mode));

      /* If the register is not valid for MODE, return 0.  If we don't
	 do this, there is no way to fix up the resulting REG later.  
	 But we do do this if the current REG is not valid for its
	 mode.  This latter is a kludge, but is required due to the
	 way that parameters are passed on some machines, most
	 notably Sparc.  */
      if (REGNO (x) < FIRST_PSEUDO_REGISTER
	  && ! HARD_REGNO_MODE_OK (REGNO (x) + word, mode)
	  && HARD_REGNO_MODE_OK (REGNO (x), GET_MODE (x)))
	return 0;
      else if (REGNO (x) < FIRST_PSEUDO_REGISTER
	       /* integrate.c can't handle parts of a return value register. */
	       && (! REG_FUNCTION_VALUE_P (x)
		   || ! rtx_equal_function_value_matters)
#ifdef CLASS_CANNOT_CHANGE_SIZE
	       && ! (GET_MODE_SIZE (mode) != GET_MODE_SIZE (GET_MODE (x))
		     && GET_MODE_CLASS (GET_MODE (x)) != MODE_COMPLEX_INT
		     && GET_MODE_CLASS (GET_MODE (x)) != MODE_COMPLEX_FLOAT
		     && (TEST_HARD_REG_BIT
			 (reg_class_contents[(int) CLASS_CANNOT_CHANGE_SIZE],
			  REGNO (x))))
#endif
	       /* We want to keep the stack, frame, and arg pointers
		  special.  */
	       && x != frame_pointer_rtx
#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
	       && x != arg_pointer_rtx
#endif
	       && x != stack_pointer_rtx)
	return gen_rtx_REG (mode, REGNO (x) + word);
      else
	return gen_rtx_SUBREG (mode, x, word);
    }
  /* If X is a CONST_INT or a CONST_DOUBLE, extract the appropriate bits
     from the low-order part of the constant.  */
  else if ((GET_MODE_CLASS (mode) == MODE_INT
	    || GET_MODE_CLASS (mode) == MODE_PARTIAL_INT)
	   && GET_MODE (x) == VOIDmode
	   && (GET_CODE (x) == CONST_INT || GET_CODE (x) == CONST_DOUBLE))
    {
      /* If MODE is twice the host word size, X is already the desired
	 representation.  Otherwise, if MODE is wider than a word, we can't
	 do this.  If MODE is exactly a word, return just one CONST_INT.
	 If MODE is smaller than a word, clear the bits that don't belong
	 in our mode, unless they and our sign bit are all one.  So we get
	 either a reasonable negative value or a reasonable unsigned value
	 for this mode.  */

      if (GET_MODE_BITSIZE (mode) >= 2 * HOST_BITS_PER_WIDE_INT)
	return x;
      else if (GET_MODE_BITSIZE (mode) > HOST_BITS_PER_WIDE_INT)
	return 0;
      else if (GET_MODE_BITSIZE (mode) == HOST_BITS_PER_WIDE_INT)
	return (GET_CODE (x) == CONST_INT ? x
		: GEN_INT (CONST_DOUBLE_LOW (x)));
      else
	{
	  /* MODE must be narrower than HOST_BITS_PER_WIDE_INT.  */
	  int width = GET_MODE_BITSIZE (mode);
	  HOST_WIDE_INT val = (GET_CODE (x) == CONST_INT ? INTVAL (x)
			       : CONST_DOUBLE_LOW (x));

	  /* Sign extend to HOST_WIDE_INT.  */
	  val = val << (HOST_BITS_PER_WIDE_INT - width) >> (HOST_BITS_PER_WIDE_INT - width);

	  return (GET_CODE (x) == CONST_INT && INTVAL (x) == val ? x
		  : GEN_INT (val));
	}
    }

  /* If X is an integral constant but we want it in floating-point, it
     must be the case that we have a union of an integer and a floating-point
     value.  If the machine-parameters allow it, simulate that union here
     and return the result.  The two-word and single-word cases are 
     different.  */

  else if (((HOST_FLOAT_FORMAT == TARGET_FLOAT_FORMAT
	     && HOST_BITS_PER_WIDE_INT == BITS_PER_WORD)
	    || flag_pretend_float)
	   && GET_MODE_CLASS (mode) == MODE_FLOAT
	   && GET_MODE_SIZE (mode) == UNITS_PER_WORD
	   && GET_CODE (x) == CONST_INT
	   && sizeof (float) * HOST_BITS_PER_CHAR == HOST_BITS_PER_WIDE_INT)
#ifdef REAL_ARITHMETIC
    {
      REAL_VALUE_TYPE r;
      HOST_WIDE_INT i;

      i = INTVAL (x);
      r = REAL_VALUE_FROM_TARGET_SINGLE (i);
      return CONST_DOUBLE_FROM_REAL_VALUE (r, mode);
    }
#else
    {
      union {HOST_WIDE_INT i; float d; } u;

      u.i = INTVAL (x);
      return CONST_DOUBLE_FROM_REAL_VALUE (u.d, mode);
    }
#endif
  else if (((HOST_FLOAT_FORMAT == TARGET_FLOAT_FORMAT
	     && HOST_BITS_PER_WIDE_INT == BITS_PER_WORD)
	    || flag_pretend_float)
	   && GET_MODE_CLASS (mode) == MODE_FLOAT
	   && GET_MODE_SIZE (mode) == 2 * UNITS_PER_WORD
	   && (GET_CODE (x) == CONST_INT || GET_CODE (x) == CONST_DOUBLE)
	   && GET_MODE (x) == VOIDmode
	   && (sizeof (double) * HOST_BITS_PER_CHAR
	       == 2 * HOST_BITS_PER_WIDE_INT))
#ifdef REAL_ARITHMETIC
    {
      REAL_VALUE_TYPE r;
      HOST_WIDE_INT i[2];
      HOST_WIDE_INT low, high;

      if (GET_CODE (x) == CONST_INT)
	low = INTVAL (x), high = low >> (HOST_BITS_PER_WIDE_INT -1);
      else
	low = CONST_DOUBLE_LOW (x), high = CONST_DOUBLE_HIGH (x);

      /* REAL_VALUE_TARGET_DOUBLE takes the addressing order of the
	 target machine.  */
      if (WORDS_BIG_ENDIAN)
	i[0] = high, i[1] = low;
      else
	i[0] = low, i[1] = high;

      r = REAL_VALUE_FROM_TARGET_DOUBLE (i);
      return CONST_DOUBLE_FROM_REAL_VALUE (r, mode);
    }
#else
    {
      union {HOST_WIDE_INT i[2]; double d; } u;
      HOST_WIDE_INT low, high;

      if (GET_CODE (x) == CONST_INT)
	low = INTVAL (x), high = low >> (HOST_BITS_PER_WIDE_INT -1);
      else
	low = CONST_DOUBLE_LOW (x), high = CONST_DOUBLE_HIGH (x);

#ifdef HOST_WORDS_BIG_ENDIAN
      u.i[0] = high, u.i[1] = low;
#else
      u.i[0] = low, u.i[1] = high;
#endif

      return CONST_DOUBLE_FROM_REAL_VALUE (u.d, mode);
    }
#endif

  /* We need an extra case for machines where HOST_BITS_PER_WIDE_INT is the
     same as sizeof (double) or when sizeof (float) is larger than the
     size of a word on the target machine.  */
#ifdef REAL_ARITHMETIC
  else if (mode == SFmode && GET_CODE (x) == CONST_INT)
    {
      REAL_VALUE_TYPE r;
      HOST_WIDE_INT i;

      i = INTVAL (x);
      r = REAL_VALUE_FROM_TARGET_SINGLE (i);
      return CONST_DOUBLE_FROM_REAL_VALUE (r, mode);
    }
  else if (((HOST_FLOAT_FORMAT == TARGET_FLOAT_FORMAT
	     && HOST_BITS_PER_WIDE_INT == BITS_PER_WORD)
	    || flag_pretend_float)
	   && GET_MODE_CLASS (mode) == MODE_FLOAT
	   && GET_MODE_SIZE (mode) == UNITS_PER_WORD
	   && GET_CODE (x) == CONST_INT
	   && (sizeof (double) * HOST_BITS_PER_CHAR
	       == HOST_BITS_PER_WIDE_INT))
    {
      REAL_VALUE_TYPE r;
      HOST_WIDE_INT i;

      i = INTVAL (x);
      r = REAL_VALUE_FROM_TARGET_DOUBLE (&i);
      return CONST_DOUBLE_FROM_REAL_VALUE (r, mode);
    }
#endif

  /* Similarly, if this is converting a floating-point value into a
     single-word integer.  Only do this is the host and target parameters are
     compatible.  */

  else if (((HOST_FLOAT_FORMAT == TARGET_FLOAT_FORMAT
	     && HOST_BITS_PER_WIDE_INT == BITS_PER_WORD)
	    || flag_pretend_float)
	   && (GET_MODE_CLASS (mode) == MODE_INT
	       || GET_MODE_CLASS (mode) == MODE_PARTIAL_INT)
	   && GET_CODE (x) == CONST_DOUBLE
	   && GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT
	   && GET_MODE_BITSIZE (mode) == BITS_PER_WORD)
    return operand_subword (x, word, 0, GET_MODE (x));

  /* Similarly, if this is converting a floating-point value into a
     two-word integer, we can do this one word at a time and make an
     integer.  Only do this is the host and target parameters are
     compatible.  */

  else if (((HOST_FLOAT_FORMAT == TARGET_FLOAT_FORMAT
	     && HOST_BITS_PER_WIDE_INT == BITS_PER_WORD)
	    || flag_pretend_float)
	   && (GET_MODE_CLASS (mode) == MODE_INT
	       || GET_MODE_CLASS (mode) == MODE_PARTIAL_INT)
	   && GET_CODE (x) == CONST_DOUBLE
	   && GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT
	   && GET_MODE_BITSIZE (mode) == 2 * BITS_PER_WORD)
    {
      rtx lowpart
	= operand_subword (x, word + WORDS_BIG_ENDIAN, 0, GET_MODE (x));
      rtx highpart
	= operand_subword (x, word + ! WORDS_BIG_ENDIAN, 0, GET_MODE (x));

      if (lowpart && GET_CODE (lowpart) == CONST_INT
	  && highpart && GET_CODE (highpart) == CONST_INT)
	return immed_double_const (INTVAL (lowpart), INTVAL (highpart), mode);
    }

  /* Otherwise, we can't do this.  */
  return 0;
}

/* Return the real part (which has mode MODE) of a complex value X.
   This always comes at the low address in memory.  */

rtx
gen_realpart (mode, x)
     enum machine_mode mode;
     register rtx x;
{
  if (GET_CODE (x) == CONCAT && GET_MODE (XEXP (x, 0)) == mode)
    return XEXP (x, 0);
  else if (WORDS_BIG_ENDIAN
	   && GET_MODE_BITSIZE (mode) < BITS_PER_WORD
	   && REG_P (x)
	   && REGNO (x) < FIRST_PSEUDO_REGISTER)
    fatal ("Unable to access real part of complex value in a hard register on this target");
  else if (WORDS_BIG_ENDIAN)
    return gen_highpart (mode, x);
  else
    return gen_lowpart (mode, x);
}

/* Return the imaginary part (which has mode MODE) of a complex value X.
   This always comes at the high address in memory.  */

rtx
gen_imagpart (mode, x)
     enum machine_mode mode;
     register rtx x;
{
  if (GET_CODE (x) == CONCAT && GET_MODE (XEXP (x, 0)) == mode)
    return XEXP (x, 1);
  else if (WORDS_BIG_ENDIAN)
    return gen_lowpart (mode, x);
  else if (!WORDS_BIG_ENDIAN
	   && GET_MODE_BITSIZE (mode) < BITS_PER_WORD
	   && REG_P (x)
	   && REGNO (x) < FIRST_PSEUDO_REGISTER)
    fatal ("Unable to access imaginary part of complex value in a hard register on this target");
  else
    return gen_highpart (mode, x);
}

/* Return 1 iff X, assumed to be a SUBREG,
   refers to the real part of the complex value in its containing reg.
   Complex values are always stored with the real part in the first word,
   regardless of WORDS_BIG_ENDIAN.  */

int
subreg_realpart_p (x)
     rtx x;
{
  if (GET_CODE (x) != SUBREG)
    abort ();

  return ((unsigned int) SUBREG_WORD (x) * UNITS_PER_WORD
	  < GET_MODE_UNIT_SIZE (GET_MODE (SUBREG_REG (x))));
}

/* Assuming that X is an rtx (e.g., MEM, REG or SUBREG) for a value,
   return an rtx (MEM, SUBREG, or CONST_INT) that refers to the
   least-significant part of X.
   MODE specifies how big a part of X to return;
   it usually should not be larger than a word.
   If X is a MEM whose address is a QUEUED, the value may be so also.  */

rtx
gen_lowpart (mode, x)
     enum machine_mode mode;
     register rtx x;
{
  rtx result = gen_lowpart_common (mode, x);

  if (result)
    return result;
  else if (GET_CODE (x) == REG)
    {
      /* Must be a hard reg that's not valid in MODE.  */
      result = gen_lowpart_common (mode, copy_to_reg (x));
      if (result == 0)
	abort ();
      return result;
    }
  else if (GET_CODE (x) == MEM)
    {
      /* The only additional case we can do is MEM.  */
      register int offset = 0;
      if (WORDS_BIG_ENDIAN)
	offset = (MAX (GET_MODE_SIZE (GET_MODE (x)), UNITS_PER_WORD)
		  - MAX (GET_MODE_SIZE (mode), UNITS_PER_WORD));

      if (BYTES_BIG_ENDIAN)
	/* Adjust the address so that the address-after-the-data
	   is unchanged.  */
	offset -= (MIN (UNITS_PER_WORD, GET_MODE_SIZE (mode))
		   - MIN (UNITS_PER_WORD, GET_MODE_SIZE (GET_MODE (x))));

      return change_address (x, mode, plus_constant (XEXP (x, 0), offset));
    }
  else if (GET_CODE (x) == ADDRESSOF)
    return gen_lowpart (mode, force_reg (GET_MODE (x), x));
  else
    abort ();
}

/* Like `gen_lowpart', but refer to the most significant part. 
   This is used to access the imaginary part of a complex number.  */

rtx
gen_highpart (mode, x)
     enum machine_mode mode;
     register rtx x;
{
  /* This case loses if X is a subreg.  To catch bugs early,
     complain if an invalid MODE is used even in other cases.  */
  if (GET_MODE_SIZE (mode) > UNITS_PER_WORD
      && GET_MODE_SIZE (mode) != GET_MODE_UNIT_SIZE (GET_MODE (x)))
    abort ();
  if (GET_CODE (x) == CONST_DOUBLE
#if !(TARGET_FLOAT_FORMAT != HOST_FLOAT_FORMAT || defined (REAL_IS_NOT_DOUBLE))
      && GET_MODE_CLASS (GET_MODE (x)) != MODE_FLOAT
#endif
      )
    return GEN_INT (CONST_DOUBLE_HIGH (x) & GET_MODE_MASK (mode));
  else if (GET_CODE (x) == CONST_INT)
    {
      if (HOST_BITS_PER_WIDE_INT <= BITS_PER_WORD)
	return const0_rtx;
      return GEN_INT (INTVAL (x) >> (HOST_BITS_PER_WIDE_INT - BITS_PER_WORD));
    }
  else if (GET_CODE (x) == MEM)
    {
      register int offset = 0;
      if (! WORDS_BIG_ENDIAN)
	offset = (MAX (GET_MODE_SIZE (GET_MODE (x)), UNITS_PER_WORD)
		  - MAX (GET_MODE_SIZE (mode), UNITS_PER_WORD));

      if (! BYTES_BIG_ENDIAN
	  && GET_MODE_SIZE (mode) < UNITS_PER_WORD)
	offset -= (GET_MODE_SIZE (mode)
		   - MIN (UNITS_PER_WORD,
			  GET_MODE_SIZE (GET_MODE (x))));

      return change_address (x, mode, plus_constant (XEXP (x, 0), offset));
    }
  else if (GET_CODE (x) == SUBREG)
    {
      /* The only time this should occur is when we are looking at a
	 multi-word item with a SUBREG whose mode is the same as that of the
	 item.  It isn't clear what we would do if it wasn't.  */
      if (SUBREG_WORD (x) != 0)
	abort ();
      return gen_highpart (mode, SUBREG_REG (x));
    }
  else if (GET_CODE (x) == REG)
    {
      int word;

      /* Let the backend decide how many registers to skip.  This is needed
         in particular for sparc64 where fp regs are smaller than a word.  */
      /* ??? Note that subregs are now ambiguous, in that those against
	 pseudos are sized by the word size, while those against hard
	 regs are sized by the underlying register size.  Better would be
	 to always interpret the subreg offset parameter as bytes or bits.  */

      if (WORDS_BIG_ENDIAN)
	word = 0;
      else if (REGNO (x) < FIRST_PSEUDO_REGISTER)
	word = (HARD_REGNO_NREGS (REGNO (x), GET_MODE (x))
		- HARD_REGNO_NREGS (REGNO (x), mode));
      else
	word = ((GET_MODE_SIZE (GET_MODE (x))
		 - MAX (GET_MODE_SIZE (mode), UNITS_PER_WORD))
		/ UNITS_PER_WORD);

      if (REGNO (x) < FIRST_PSEUDO_REGISTER
	  /* integrate.c can't handle parts of a return value register.  */
	  && (! REG_FUNCTION_VALUE_P (x)
	      || ! rtx_equal_function_value_matters)
	  /* We want to keep the stack, frame, and arg pointers special.  */
	  && x != frame_pointer_rtx
#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
	  && x != arg_pointer_rtx
#endif
	  && x != stack_pointer_rtx)
	return gen_rtx_REG (mode, REGNO (x) + word);
      else
	return gen_rtx_SUBREG (mode, x, word);
    }
  else
    abort ();
}

/* Return 1 iff X, assumed to be a SUBREG,
   refers to the least significant part of its containing reg.
   If X is not a SUBREG, always return 1 (it is its own low part!).  */

int
subreg_lowpart_p (x)
     rtx x;
{
  if (GET_CODE (x) != SUBREG)
    return 1;
  else if (GET_MODE (SUBREG_REG (x)) == VOIDmode)
    return 0;

  if (WORDS_BIG_ENDIAN
      && GET_MODE_SIZE (GET_MODE (SUBREG_REG (x))) > UNITS_PER_WORD)
    return (SUBREG_WORD (x)
	    == ((GET_MODE_SIZE (GET_MODE (SUBREG_REG (x)))
		 - MAX (GET_MODE_SIZE (GET_MODE (x)), UNITS_PER_WORD))
		/ UNITS_PER_WORD));

  return SUBREG_WORD (x) == 0;
}

/* Return subword I of operand OP.
   The word number, I, is interpreted as the word number starting at the
   low-order address.  Word 0 is the low-order word if not WORDS_BIG_ENDIAN,
   otherwise it is the high-order word.

   If we cannot extract the required word, we return zero.  Otherwise, an
   rtx corresponding to the requested word will be returned.

   VALIDATE_ADDRESS is nonzero if the address should be validated.  Before
   reload has completed, a valid address will always be returned.  After
   reload, if a valid address cannot be returned, we return zero.

   If VALIDATE_ADDRESS is zero, we simply form the required address; validating
   it is the responsibility of the caller.

   MODE is the mode of OP in case it is a CONST_INT.  */

rtx
operand_subword (op, i, validate_address, mode)
     rtx op;
     unsigned int i;
     int validate_address;
     enum machine_mode mode;
{
  HOST_WIDE_INT val;
  int size_ratio = HOST_BITS_PER_WIDE_INT / BITS_PER_WORD;

  if (mode == VOIDmode)
    mode = GET_MODE (op);

  if (mode == VOIDmode)
    abort ();

  /* If OP is narrower than a word, fail. */
  if (mode != BLKmode
      && (GET_MODE_SIZE (mode) < UNITS_PER_WORD))
    return 0;

  /* If we want a word outside OP, return zero. */
  if (mode != BLKmode
      && (i + 1) * UNITS_PER_WORD > GET_MODE_SIZE (mode))
    return const0_rtx;

  /* If OP is already an integer word, return it.  */
  if (GET_MODE_CLASS (mode) == MODE_INT
      && GET_MODE_SIZE (mode) == UNITS_PER_WORD)
    return op;

  /* If OP is a REG or SUBREG, we can handle it very simply.  */
  if (GET_CODE (op) == REG)
    {
      /* ??? There is a potential problem with this code.  It does not
	 properly handle extractions of a subword from a hard register
	 that is larger than word_mode.  Presumably the check for
	 HARD_REGNO_MODE_OK catches these most of these cases.  */

      /* If OP is a hard register, but OP + I is not a hard register,
	 then extracting a subword is impossible.

	 For example, consider if OP is the last hard register and it is
	 larger than word_mode.  If we wanted word N (for N > 0) because a
	 part of that hard register was known to contain a useful value,
	 then OP + I would refer to a pseudo, not the hard register we
	 actually wanted.  */
      if (REGNO (op) < FIRST_PSEUDO_REGISTER
	  && REGNO (op) + i >= FIRST_PSEUDO_REGISTER)
	return 0;

      /* If the register is not valid for MODE, return 0.  Note we
	 have to check both OP and OP + I since they may refer to
	 different parts of the register file.

	 Consider if OP refers to the last 96bit FP register and we want
	 subword 3 because that subword is known to contain a value we
	 needed.  */
      if (REGNO (op) < FIRST_PSEUDO_REGISTER
	  && (! HARD_REGNO_MODE_OK (REGNO (op), word_mode)
	      || ! HARD_REGNO_MODE_OK (REGNO (op) + i, word_mode)))
	return 0;
      else if (REGNO (op) >= FIRST_PSEUDO_REGISTER
	       || (REG_FUNCTION_VALUE_P (op)
		   && rtx_equal_function_value_matters)
	       /* We want to keep the stack, frame, and arg pointers
		  special.  */
	       || op == frame_pointer_rtx
#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
	       || op == arg_pointer_rtx
#endif
	       || op == stack_pointer_rtx)
	return gen_rtx_SUBREG (word_mode, op, i);
      else
	return gen_rtx_REG (word_mode, REGNO (op) + i);
    }
  else if (GET_CODE (op) == SUBREG)
    return gen_rtx_SUBREG (word_mode, SUBREG_REG (op), i + SUBREG_WORD (op));
  else if (GET_CODE (op) == CONCAT)
    {
      unsigned int partwords
	= GET_MODE_UNIT_SIZE (GET_MODE (op)) / UNITS_PER_WORD;

      if (i < partwords)
	return operand_subword (XEXP (op, 0), i, validate_address, mode);
      return operand_subword (XEXP (op, 1), i - partwords,
			      validate_address, mode);
    }

  /* Form a new MEM at the requested address.  */
  if (GET_CODE (op) == MEM)
    {
      rtx addr = plus_constant (XEXP (op, 0), i * UNITS_PER_WORD);
      rtx new;

      if (validate_address)
	{
	  if (reload_completed)
	    {
	      if (! strict_memory_address_p (word_mode, addr))
		return 0;
	    }
	  else
	    addr = memory_address (word_mode, addr);
	}

      new = gen_rtx_MEM (word_mode, addr);

      MEM_COPY_ATTRIBUTES (new, op);
      RTX_UNCHANGING_P (new) = RTX_UNCHANGING_P (op);
      MEM_ALIAS_SET (new) = MEM_ALIAS_SET (op);

      return new;
    }

  /* The only remaining cases are when OP is a constant.  If the host and
     target floating formats are the same, handling two-word floating
     constants are easy.  Note that REAL_VALUE_TO_TARGET_{SINGLE,DOUBLE}
     are defined as returning one or two 32 bit values, respectively,
     and not values of BITS_PER_WORD bits.  */
#ifdef REAL_ARITHMETIC
  /* The output is some bits, the width of the target machine's word.
     A wider-word host can surely hold them in a CONST_INT. A narrower-word
     host can't.  */
  if (HOST_BITS_PER_WIDE_INT >= BITS_PER_WORD
      && GET_MODE_CLASS (mode) == MODE_FLOAT
      && GET_MODE_BITSIZE (mode) == 64
      && GET_CODE (op) == CONST_DOUBLE)
    {
      long k[2];
      REAL_VALUE_TYPE rv;

      REAL_VALUE_FROM_CONST_DOUBLE (rv, op);
      REAL_VALUE_TO_TARGET_DOUBLE (rv, k);

      /* We handle 32-bit and >= 64-bit words here.  Note that the order in
	 which the words are written depends on the word endianness.
	 ??? This is a potential portability problem and should
	 be fixed at some point.

	 We must excercise caution with the sign bit.  By definition there
	 are 32 significant bits in K; there may be more in a HOST_WIDE_INT.
	 Consider a host with a 32-bit long and a 64-bit HOST_WIDE_INT.
	 So we explicitly mask and sign-extend as necessary.  */
      if (BITS_PER_WORD == 32)
	{
	  val = k[i];
	  val = ((val & 0xffffffff) ^ 0x80000000) - 0x80000000;
	  return GEN_INT (val);
	}
#if HOST_BITS_PER_WIDE_INT >= 64
      else if (BITS_PER_WORD >= 64 && i == 0)
	{
	  val = k[! WORDS_BIG_ENDIAN];
	  val = (((val & 0xffffffff) ^ 0x80000000) - 0x80000000) << 32;
	  val |= (HOST_WIDE_INT) k[WORDS_BIG_ENDIAN] & 0xffffffff;
	  return GEN_INT (val);
	}
#endif
      else if (BITS_PER_WORD == 16)
	{
	  val = k[i >> 1];
	  if ((i & 1) == !WORDS_BIG_ENDIAN)
	    val >>= 16;
	  val &= 0xffff;
	  return GEN_INT (val);
	}
      else
	abort ();
    }
  else if (HOST_BITS_PER_WIDE_INT >= BITS_PER_WORD
	   && GET_MODE_CLASS (mode) == MODE_FLOAT
	   && GET_MODE_BITSIZE (mode) > 64
	   && GET_CODE (op) == CONST_DOUBLE)
    {
      long k[4];
      REAL_VALUE_TYPE rv;

      REAL_VALUE_FROM_CONST_DOUBLE (rv, op);
      REAL_VALUE_TO_TARGET_LONG_DOUBLE (rv, k);

      if (BITS_PER_WORD == 32)
	{
	  val = k[i];
	  val = ((val & 0xffffffff) ^ 0x80000000) - 0x80000000;
	  return GEN_INT (val);
	}
#if HOST_BITS_PER_WIDE_INT >= 64
      else if (BITS_PER_WORD >= 64 && i <= 1)
	{
	  val = k[i*2 + ! WORDS_BIG_ENDIAN];
	  val = (((val & 0xffffffff) ^ 0x80000000) - 0x80000000) << 32;
	  val |= (HOST_WIDE_INT) k[i*2 + WORDS_BIG_ENDIAN] & 0xffffffff;
	  return GEN_INT (val);
	}
#endif
      else
	abort ();
    }
#else /* no REAL_ARITHMETIC */
  if (((HOST_FLOAT_FORMAT == TARGET_FLOAT_FORMAT
	&& HOST_BITS_PER_WIDE_INT == BITS_PER_WORD)
       || flag_pretend_float)
      && GET_MODE_CLASS (mode) == MODE_FLOAT
      && GET_MODE_SIZE (mode) == 2 * UNITS_PER_WORD
      && GET_CODE (op) == CONST_DOUBLE)
    {
      /* The constant is stored in the host's word-ordering,
	 but we want to access it in the target's word-ordering.  Some
	 compilers don't like a conditional inside macro args, so we have two
	 copies of the return.  */
#ifdef HOST_WORDS_BIG_ENDIAN
      return GEN_INT (i == WORDS_BIG_ENDIAN
		      ? CONST_DOUBLE_HIGH (op) : CONST_DOUBLE_LOW (op));
#else
      return GEN_INT (i != WORDS_BIG_ENDIAN
		      ? CONST_DOUBLE_HIGH (op) : CONST_DOUBLE_LOW (op));
#endif
    }
#endif /* no REAL_ARITHMETIC */

  /* Single word float is a little harder, since single- and double-word
     values often do not have the same high-order bits.  We have already
     verified that we want the only defined word of the single-word value.  */
#ifdef REAL_ARITHMETIC
  if (GET_MODE_CLASS (mode) == MODE_FLOAT
      && GET_MODE_BITSIZE (mode) == 32
      && GET_CODE (op) == CONST_DOUBLE)
    {
      long l;
      REAL_VALUE_TYPE rv;

      REAL_VALUE_FROM_CONST_DOUBLE (rv, op);
      REAL_VALUE_TO_TARGET_SINGLE (rv, l);

      /* Sign extend from known 32-bit value to HOST_WIDE_INT.  */
      val = l;
      val = ((val & 0xffffffff) ^ 0x80000000) - 0x80000000;

      if (BITS_PER_WORD == 16)
	{
	  if ((i & 1) == !WORDS_BIG_ENDIAN)
	    val >>= 16;
	  val &= 0xffff;
	}

      return GEN_INT (val);
    }
#else
  if (((HOST_FLOAT_FORMAT == TARGET_FLOAT_FORMAT
	&& HOST_BITS_PER_WIDE_INT == BITS_PER_WORD)
       || flag_pretend_float)
      && sizeof (float) * 8 == HOST_BITS_PER_WIDE_INT
      && GET_MODE_CLASS (mode) == MODE_FLOAT
      && GET_MODE_SIZE (mode) == UNITS_PER_WORD
      && GET_CODE (op) == CONST_DOUBLE)
    {
      double d;
      union {float f; HOST_WIDE_INT i; } u;

      REAL_VALUE_FROM_CONST_DOUBLE (d, op);

      u.f = d;
      return GEN_INT (u.i);
    }
  if (((HOST_FLOAT_FORMAT == TARGET_FLOAT_FORMAT
	&& HOST_BITS_PER_WIDE_INT == BITS_PER_WORD)
       || flag_pretend_float)
      && sizeof (double) * 8 == HOST_BITS_PER_WIDE_INT
      && GET_MODE_CLASS (mode) == MODE_FLOAT
      && GET_MODE_SIZE (mode) == UNITS_PER_WORD
      && GET_CODE (op) == CONST_DOUBLE)
    {
      double d;
      union {double d; HOST_WIDE_INT i; } u;

      REAL_VALUE_FROM_CONST_DOUBLE (d, op);

      u.d = d;
      return GEN_INT (u.i);
    }
#endif /* no REAL_ARITHMETIC */
      
  /* The only remaining cases that we can handle are integers.
     Convert to proper endianness now since these cases need it.
     At this point, i == 0 means the low-order word.  

     We do not want to handle the case when BITS_PER_WORD <= HOST_BITS_PER_INT
     in general.  However, if OP is (const_int 0), we can just return
     it for any word.  */

  if (op == const0_rtx)
    return op;

  if (GET_MODE_CLASS (mode) != MODE_INT
      || (GET_CODE (op) != CONST_INT && GET_CODE (op) != CONST_DOUBLE)
      || BITS_PER_WORD > HOST_BITS_PER_WIDE_INT)
    return 0;

  if (WORDS_BIG_ENDIAN)
    i = GET_MODE_SIZE (mode) / UNITS_PER_WORD - 1 - i;

  /* Find out which word on the host machine this value is in and get
     it from the constant.  */
  val = (i / size_ratio == 0
	 ? (GET_CODE (op) == CONST_INT ? INTVAL (op) : CONST_DOUBLE_LOW (op))
	 : (GET_CODE (op) == CONST_INT
	    ? (INTVAL (op) < 0 ? ~0 : 0) : CONST_DOUBLE_HIGH (op)));

  /* Get the value we want into the low bits of val.  */
  if (BITS_PER_WORD < HOST_BITS_PER_WIDE_INT)
    val = ((val >> ((i % size_ratio) * BITS_PER_WORD)));

  val = trunc_int_for_mode (val, word_mode);

  return GEN_INT (val);
}

/* Similar to `operand_subword', but never return 0.  If we can't extract
   the required subword, put OP into a register and try again.  If that fails,
   abort.  We always validate the address in this case.  It is not valid
   to call this function after reload; it is mostly meant for RTL
   generation. 

   MODE is the mode of OP, in case it is CONST_INT.  */

rtx
operand_subword_force (op, i, mode)
     rtx op;
     unsigned int i;
     enum machine_mode mode;
{
  rtx result = operand_subword (op, i, 1, mode);

  if (result)
    return result;

  if (mode != BLKmode && mode != VOIDmode)
    {
      /* If this is a register which can not be accessed by words, copy it
	 to a pseudo register.  */
      if (GET_CODE (op) == REG)
	op = copy_to_reg (op);
      else
	op = force_reg (mode, op);
    }

  result = operand_subword (op, i, 1, mode);
  if (result == 0)
    abort ();

  return result;
}

/* Given a compare instruction, swap the operands.
   A test instruction is changed into a compare of 0 against the operand.  */

void
reverse_comparison (insn)
     rtx insn;
{
  rtx body = PATTERN (insn);
  rtx comp;

  if (GET_CODE (body) == SET)
    comp = SET_SRC (body);
  else
    comp = SET_SRC (XVECEXP (body, 0, 0));

  if (GET_CODE (comp) == COMPARE)
    {
      rtx op0 = XEXP (comp, 0);
      rtx op1 = XEXP (comp, 1);
      XEXP (comp, 0) = op1;
      XEXP (comp, 1) = op0;
    }
  else
    {
      rtx new = gen_rtx_COMPARE (VOIDmode,
				 CONST0_RTX (GET_MODE (comp)), comp);
      if (GET_CODE (body) == SET)
	SET_SRC (body) = new;
      else
	SET_SRC (XVECEXP (body, 0, 0)) = new;
    }
}

/* Return a memory reference like MEMREF, but with its mode changed
   to MODE and its address changed to ADDR.
   (VOIDmode means don't change the mode.
   NULL for ADDR means don't change the address.)  */

rtx
change_address (memref, mode, addr)
     rtx memref;
     enum machine_mode mode;
     rtx addr;
{
  rtx new;

  if (GET_CODE (memref) != MEM)
    abort ();
  if (mode == VOIDmode)
    mode = GET_MODE (memref);
  if (addr == 0)
    addr = XEXP (memref, 0);

  /* If reload is in progress or has completed, ADDR must be valid.
     Otherwise, we can call memory_address to make it valid.  */
  if (reload_completed || reload_in_progress)
    {
      if (! memory_address_p (mode, addr))
	abort ();
    }
  else
    addr = memory_address (mode, addr);
	
  if (rtx_equal_p (addr, XEXP (memref, 0)) && mode == GET_MODE (memref))
    return memref;

  new = gen_rtx_MEM (mode, addr);
  RTX_UNCHANGING_P (new) = RTX_UNCHANGING_P (memref);
  MEM_COPY_ATTRIBUTES (new, memref);
  MEM_ALIAS_SET (new) = MEM_ALIAS_SET (memref);
  return new;
}

/* Return a newly created CODE_LABEL rtx with a unique label number.  */

rtx
gen_label_rtx ()
{
  register rtx label;

  label = gen_rtx_CODE_LABEL (VOIDmode, 0, NULL_RTX,
			      NULL_RTX, label_num++, NULL_PTR, NULL_PTR);

  LABEL_NUSES (label) = 0;
  LABEL_ALTERNATE_NAME (label) = NULL;
  return label;
}

/* For procedure integration.  */

/* Install new pointers to the first and last insns in the chain.
   Also, set cur_insn_uid to one higher than the last in use.
   Used for an inline-procedure after copying the insn chain.  */

void
set_new_first_and_last_insn (first, last)
     rtx first, last;
{
  rtx insn;

  first_insn = first;
  last_insn = last;
  cur_insn_uid = 0;

  for (insn = first; insn; insn = NEXT_INSN (insn))
    cur_insn_uid = MAX (cur_insn_uid, INSN_UID (insn));

  cur_insn_uid++;
}

/* Set the range of label numbers found in the current function.
   This is used when belatedly compiling an inline function.  */

void
set_new_first_and_last_label_num (first, last)
     int first, last;
{
  base_label_num = label_num;
  first_label_num = first;
  last_label_num = last;
}

/* Set the last label number found in the current function.
   This is used when belatedly compiling an inline function.  */

void
set_new_last_label_num (last)
     int last;
{
  base_label_num = label_num;
  last_label_num = last;
}

/* Restore all variables describing the current status from the structure *P.
   This is used after a nested function.  */

void
restore_emit_status (p)
     struct function *p ATTRIBUTE_UNUSED;
{
  last_label_num = 0;
  clear_emit_caches ();
}

/* Clear out all parts of the state in F that can safely be discarded
   after the function has been compiled, to let garbage collection
   reclaim the memory.  */

void
free_emit_status (f)
     struct function *f;
{
  free (f->emit->x_regno_reg_rtx);
  free (f->emit->regno_pointer_flag);
  free (f->emit->regno_pointer_align);
  free (f->emit);
  f->emit = NULL;
}

/* Go through all the RTL insn bodies and copy any invalid shared 
   structure.  This routine should only be called once.  */

void
unshare_all_rtl (fndecl, insn)
     tree fndecl;
     rtx insn;
{
  tree decl;

  /* Make sure that virtual parameters are not shared.  */
  for (decl = DECL_ARGUMENTS (fndecl); decl; decl = TREE_CHAIN (decl))
    copy_rtx_if_shared (DECL_RTL (decl));

  /* Unshare just about everything else.  */
  unshare_all_rtl_1 (insn);
  
  /* Make sure the addresses of stack slots found outside the insn chain
     (such as, in DECL_RTL of a variable) are not shared
     with the insn chain.

     This special care is necessary when the stack slot MEM does not
     actually appear in the insn chain.  If it does appear, its address
     is unshared from all else at that point.  */
  copy_rtx_if_shared (stack_slot_list);
}

/* Go through all the RTL insn bodies and copy any invalid shared 
   structure, again.  This is a fairly expensive thing to do so it
   should be done sparingly.  */

void
unshare_all_rtl_again (insn)
     rtx insn;
{
  rtx p;
  for (p = insn; p; p = NEXT_INSN (p))
    if (GET_RTX_CLASS (GET_CODE (p)) == 'i')
      {
	reset_used_flags (PATTERN (p));
	reset_used_flags (REG_NOTES (p));
	reset_used_flags (LOG_LINKS (p));
      }
  unshare_all_rtl_1 (insn);
}

/* Go through all the RTL insn bodies and copy any invalid shared structure.
   Assumes the mark bits are cleared at entry.  */

static void
unshare_all_rtl_1 (insn)
     rtx insn;
{
  for (; insn; insn = NEXT_INSN (insn))
    if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
      {
	PATTERN (insn) = copy_rtx_if_shared (PATTERN (insn));
	REG_NOTES (insn) = copy_rtx_if_shared (REG_NOTES (insn));
	LOG_LINKS (insn) = copy_rtx_if_shared (LOG_LINKS (insn));
      }
}

/* Mark ORIG as in use, and return a copy of it if it was already in use.
   Recursively does the same for subexpressions.  */

rtx
copy_rtx_if_shared (orig)
     rtx orig;
{
  register rtx x = orig;
  register int i;
  register enum rtx_code code;
  register const char *format_ptr;
  int copied = 0;

  if (x == 0)
    return 0;

  code = GET_CODE (x);

  /* These types may be freely shared.  */

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
      return x;

    case CONST:
      /* CONST can be shared if it contains a SYMBOL_REF.  If it contains
	 a LABEL_REF, it isn't sharable.  */
      if (GET_CODE (XEXP (x, 0)) == PLUS
	  && GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF
	  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT)
	return x;
      break;

    case INSN:
    case JUMP_INSN:
    case CALL_INSN:
    case NOTE:
    case BARRIER:
      /* The chain of insns is not being copied.  */
      return x;

    case MEM:
      /* A MEM is allowed to be shared if its address is constant.

	 We used to allow sharing of MEMs which referenced 
	 virtual_stack_vars_rtx or virtual_incoming_args_rtx, but
	 that can lose.  instantiate_virtual_regs will not unshare
	 the MEMs, and combine may change the structure of the address
	 because it looks safe and profitable in one context, but
	 in some other context it creates unrecognizable RTL.  */
      if (CONSTANT_ADDRESS_P (XEXP (x, 0)))
	return x;

      break;

    default:
      break;
    }

  /* This rtx may not be shared.  If it has already been seen,
     replace it with a copy of itself.  */

  if (x->used)
    {
      register rtx copy;

      copy = rtx_alloc (code);
      bcopy ((char *) x, (char *) copy,
	     (sizeof (*copy) - sizeof (copy->fld)
	      + sizeof (copy->fld[0]) * GET_RTX_LENGTH (code)));
      x = copy;
      copied = 1;
    }
  x->used = 1;

  /* Now scan the subexpressions recursively.
     We can store any replaced subexpressions directly into X
     since we know X is not shared!  Any vectors in X
     must be copied if X was copied.  */

  format_ptr = GET_RTX_FORMAT (code);

  for (i = 0; i < GET_RTX_LENGTH (code); i++)
    {
      switch (*format_ptr++)
	{
	case 'e':
	  XEXP (x, i) = copy_rtx_if_shared (XEXP (x, i));
	  break;

	case 'E':
	  if (XVEC (x, i) != NULL)
	    {
	      register int j;
	      int len = XVECLEN (x, i);

	      if (copied && len > 0)
		XVEC (x, i) = gen_rtvec_v (len, XVEC (x, i)->elem);
	      for (j = 0; j < len; j++)
		XVECEXP (x, i, j) = copy_rtx_if_shared (XVECEXP (x, i, j));
	    }
	  break;
	}
    }
  return x;
}

/* Clear all the USED bits in X to allow copy_rtx_if_shared to be used
   to look for shared sub-parts.  */

void
reset_used_flags (x)
     rtx x;
{
  register int i, j;
  register enum rtx_code code;
  register const char *format_ptr;

  if (x == 0)
    return;

  code = GET_CODE (x);

  /* These types may be freely shared so we needn't do any resetting
     for them.  */

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
      return;

    case INSN:
    case JUMP_INSN:
    case CALL_INSN:
    case NOTE:
    case LABEL_REF:
    case BARRIER:
      /* The chain of insns is not being copied.  */
      return;
      
    default:
      break;
    }

  x->used = 0;

  format_ptr = GET_RTX_FORMAT (code);
  for (i = 0; i < GET_RTX_LENGTH (code); i++)
    {
      switch (*format_ptr++)
	{
	case 'e':
	  reset_used_flags (XEXP (x, i));
	  break;

	case 'E':
	  for (j = 0; j < XVECLEN (x, i); j++)
	    reset_used_flags (XVECEXP (x, i, j));
	  break;
	}
    }
}

/* Copy X if necessary so that it won't be altered by changes in OTHER.
   Return X or the rtx for the pseudo reg the value of X was copied into.
   OTHER must be valid as a SET_DEST.  */

rtx
make_safe_from (x, other)
     rtx x, other;
{
  while (1)
    switch (GET_CODE (other))
      {
      case SUBREG:
	other = SUBREG_REG (other);
	break;
      case STRICT_LOW_PART:
      case SIGN_EXTEND:
      case ZERO_EXTEND:
	other = XEXP (other, 0);
	break;
      default:
	goto done;
      }
 done:
  if ((GET_CODE (other) == MEM
       && ! CONSTANT_P (x)
       && GET_CODE (x) != REG
       && GET_CODE (x) != SUBREG)
      || (GET_CODE (other) == REG
	  && (REGNO (other) < FIRST_PSEUDO_REGISTER
	      || reg_mentioned_p (other, x))))
    {
      rtx temp = gen_reg_rtx (GET_MODE (x));
      emit_move_insn (temp, x);
      return temp;
    }
  return x;
}

/* Emission of insns (adding them to the doubly-linked list).  */

/* Return the first insn of the current sequence or current function.  */

rtx
get_insns ()
{
  return first_insn;
}

/* Return the last insn emitted in current sequence or current function.  */

rtx
get_last_insn ()
{
  return last_insn;
}

/* Specify a new insn as the last in the chain.  */

void
set_last_insn (insn)
     rtx insn;
{
  if (NEXT_INSN (insn) != 0)
    abort ();
  last_insn = insn;
}

/* Return the last insn emitted, even if it is in a sequence now pushed.  */

rtx
get_last_insn_anywhere ()
{
  struct sequence_stack *stack;
  if (last_insn)
    return last_insn;
  for (stack = seq_stack; stack; stack = stack->next)
    if (stack->last != 0)
      return stack->last;
  return 0;
}

/* Return a number larger than any instruction's uid in this function.  */

int
get_max_uid ()
{
  return cur_insn_uid;
}

/* Renumber instructions so that no instruction UIDs are wasted.  */

void
renumber_insns (stream)
     FILE *stream;
{
  rtx insn;

  /* If we're not supposed to renumber instructions, don't.  */
  if (!flag_renumber_insns)
    return;

  /* If there aren't that many instructions, then it's not really
     worth renumbering them.  */
  if (flag_renumber_insns == 1 && get_max_uid () < 25000)
    return;

  cur_insn_uid = 1;

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (stream)
	fprintf (stream, "Renumbering insn %d to %d\n", 
		 INSN_UID (insn), cur_insn_uid);
      INSN_UID (insn) = cur_insn_uid++;
    }
}

/* Return the next insn.  If it is a SEQUENCE, return the first insn
   of the sequence.  */

rtx
next_insn (insn)
     rtx insn;
{
  if (insn)
    {
      insn = NEXT_INSN (insn);
      if (insn && GET_CODE (insn) == INSN
	  && GET_CODE (PATTERN (insn)) == SEQUENCE)
	insn = XVECEXP (PATTERN (insn), 0, 0);
    }

  return insn;
}

/* Return the previous insn.  If it is a SEQUENCE, return the last insn
   of the sequence.  */

rtx
previous_insn (insn)
     rtx insn;
{
  if (insn)
    {
      insn = PREV_INSN (insn);
      if (insn && GET_CODE (insn) == INSN
	  && GET_CODE (PATTERN (insn)) == SEQUENCE)
	insn = XVECEXP (PATTERN (insn), 0, XVECLEN (PATTERN (insn), 0) - 1);
    }

  return insn;
}

/* Return the next insn after INSN that is not a NOTE.  This routine does not
   look inside SEQUENCEs.  */

rtx
next_nonnote_insn (insn)
     rtx insn;
{
  while (insn)
    {
      insn = NEXT_INSN (insn);
      if (insn == 0 || GET_CODE (insn) != NOTE)
	break;
    }

  return insn;
}

/* Return the previous insn before INSN that is not a NOTE.  This routine does
   not look inside SEQUENCEs.  */

rtx
prev_nonnote_insn (insn)
     rtx insn;
{
  while (insn)
    {
      insn = PREV_INSN (insn);
      if (insn == 0 || GET_CODE (insn) != NOTE)
	break;
    }

  return insn;
}

/* Return the next INSN, CALL_INSN or JUMP_INSN after INSN;
   or 0, if there is none.  This routine does not look inside
   SEQUENCEs.  */

rtx
next_real_insn (insn)
     rtx insn;
{
  while (insn)
    {
      insn = NEXT_INSN (insn);
      if (insn == 0 || GET_CODE (insn) == INSN
	  || GET_CODE (insn) == CALL_INSN || GET_CODE (insn) == JUMP_INSN)
	break;
    }

  return insn;
}

/* Return the last INSN, CALL_INSN or JUMP_INSN before INSN;
   or 0, if there is none.  This routine does not look inside
   SEQUENCEs.  */

rtx
prev_real_insn (insn)
     rtx insn;
{
  while (insn)
    {
      insn = PREV_INSN (insn);
      if (insn == 0 || GET_CODE (insn) == INSN || GET_CODE (insn) == CALL_INSN
	  || GET_CODE (insn) == JUMP_INSN)
	break;
    }

  return insn;
}

/* Find the next insn after INSN that really does something.  This routine
   does not look inside SEQUENCEs.  Until reload has completed, this is the
   same as next_real_insn.  */

int
active_insn_p (insn)
     rtx insn;
{
  return (GET_CODE (insn) == CALL_INSN || GET_CODE (insn) == JUMP_INSN
	  || (GET_CODE (insn) == INSN
	      && (! reload_completed
		  || (GET_CODE (PATTERN (insn)) != USE
		      && GET_CODE (PATTERN (insn)) != CLOBBER))));
}

rtx
next_active_insn (insn)
     rtx insn;
{
  while (insn)
    {
      insn = NEXT_INSN (insn);
      if (insn == 0 || active_insn_p (insn))
	break;
    }

  return insn;
}

/* Find the last insn before INSN that really does something.  This routine
   does not look inside SEQUENCEs.  Until reload has completed, this is the
   same as prev_real_insn.  */

rtx
prev_active_insn (insn)
     rtx insn;
{
  while (insn)
    {
      insn = PREV_INSN (insn);
      if (insn == 0 || active_insn_p (insn))
	break;
    }

  return insn;
}

/* Return the next CODE_LABEL after the insn INSN, or 0 if there is none.  */

rtx
next_label (insn)
     rtx insn;
{
  while (insn)
    {
      insn = NEXT_INSN (insn);
      if (insn == 0 || GET_CODE (insn) == CODE_LABEL)
	break;
    }

  return insn;
}

/* Return the last CODE_LABEL before the insn INSN, or 0 if there is none.  */

rtx
prev_label (insn)
     rtx insn;
{
  while (insn)
    {
      insn = PREV_INSN (insn);
      if (insn == 0 || GET_CODE (insn) == CODE_LABEL)
	break;
    }

  return insn;
}

#ifdef HAVE_cc0
/* INSN uses CC0 and is being moved into a delay slot.  Set up REG_CC_SETTER
   and REG_CC_USER notes so we can find it.  */

void
link_cc0_insns (insn)
     rtx insn;
{
  rtx user = next_nonnote_insn (insn);

  if (GET_CODE (user) == INSN && GET_CODE (PATTERN (user)) == SEQUENCE)
    user = XVECEXP (PATTERN (user), 0, 0);

  REG_NOTES (user) = gen_rtx_INSN_LIST (REG_CC_SETTER, insn,
					REG_NOTES (user));
  REG_NOTES (insn) = gen_rtx_INSN_LIST (REG_CC_USER, user, REG_NOTES (insn));
}

/* Return the next insn that uses CC0 after INSN, which is assumed to
   set it.  This is the inverse of prev_cc0_setter (i.e., prev_cc0_setter
   applied to the result of this function should yield INSN).

   Normally, this is simply the next insn.  However, if a REG_CC_USER note
   is present, it contains the insn that uses CC0.

   Return 0 if we can't find the insn.  */

rtx
next_cc0_user (insn)
     rtx insn;
{
  rtx note = find_reg_note (insn, REG_CC_USER, NULL_RTX);

  if (note)
    return XEXP (note, 0);

  insn = next_nonnote_insn (insn);
  if (insn && GET_CODE (insn) == INSN && GET_CODE (PATTERN (insn)) == SEQUENCE)
    insn = XVECEXP (PATTERN (insn), 0, 0);

  if (insn && GET_RTX_CLASS (GET_CODE (insn)) == 'i'
      && reg_mentioned_p (cc0_rtx, PATTERN (insn)))
    return insn;

  return 0;
}

/* Find the insn that set CC0 for INSN.  Unless INSN has a REG_CC_SETTER
   note, it is the previous insn.  */

rtx
prev_cc0_setter (insn)
     rtx insn;
{
  rtx note = find_reg_note (insn, REG_CC_SETTER, NULL_RTX);

  if (note)
    return XEXP (note, 0);

  insn = prev_nonnote_insn (insn);
  if (! sets_cc0_p (PATTERN (insn)))
    abort ();

  return insn;
}
#endif

/* Try splitting insns that can be split for better scheduling.
   PAT is the pattern which might split.
   TRIAL is the insn providing PAT.
   LAST is non-zero if we should return the last insn of the sequence produced.

   If this routine succeeds in splitting, it returns the first or last
   replacement insn depending on the value of LAST.  Otherwise, it
   returns TRIAL.  If the insn to be returned can be split, it will be.  */

rtx
try_split (pat, trial, last)
     rtx pat, trial;
     int last;
{
  rtx before = PREV_INSN (trial);
  rtx after = NEXT_INSN (trial);
  rtx seq = split_insns (pat, trial);
  int has_barrier = 0;
  rtx tem;

  /* If we are splitting a JUMP_INSN, it might be followed by a BARRIER.
     We may need to handle this specially.  */
  if (after && GET_CODE (after) == BARRIER)
    {
      has_barrier = 1;
      after = NEXT_INSN (after);
    }

  if (seq)
    {
      /* SEQ can either be a SEQUENCE or the pattern of a single insn.
	 The latter case will normally arise only when being done so that
	 it, in turn, will be split (SFmode on the 29k is an example).  */
      if (GET_CODE (seq) == SEQUENCE)
	{
	  /* If we are splitting a JUMP_INSN, look for the JUMP_INSN in
	     SEQ and copy our JUMP_LABEL to it.  If JUMP_LABEL is non-zero,
	     increment the usage count so we don't delete the label.  */
	  int i;

	  if (GET_CODE (trial) == JUMP_INSN)
	    for (i = XVECLEN (seq, 0) - 1; i >= 0; i--)
	      if (GET_CODE (XVECEXP (seq, 0, i)) == JUMP_INSN)
		{
		  JUMP_LABEL (XVECEXP (seq, 0, i)) = JUMP_LABEL (trial);

		  if (JUMP_LABEL (trial))
		    LABEL_NUSES (JUMP_LABEL (trial))++;
		}

	  tem = emit_insn_after (seq, before);

	  delete_insn (trial);
	  if (has_barrier)
	    emit_barrier_after (tem);

	  /* Recursively call try_split for each new insn created; by the
	     time control returns here that insn will be fully split, so
	     set LAST and continue from the insn after the one returned.
	     We can't use next_active_insn here since AFTER may be a note.
	     Ignore deleted insns, which can be occur if not optimizing.  */
	  for (tem = NEXT_INSN (before); tem != after;
	       tem = NEXT_INSN (tem))
	    if (! INSN_DELETED_P (tem)
		&& GET_RTX_CLASS (GET_CODE (tem)) == 'i')
	      tem = try_split (PATTERN (tem), tem, 1);
	}
      /* Avoid infinite loop if the result matches the original pattern.  */
      else if (rtx_equal_p (seq, pat))
	return trial;
      else
	{
	  PATTERN (trial) = seq;
	  INSN_CODE (trial) = -1;
	  try_split (seq, trial, last);
	}

      /* Return either the first or the last insn, depending on which was
	 requested.  */
      return last 
		? (after ? prev_active_insn (after) : last_insn) 
		: next_active_insn (before);
    }

  return trial;
}

/* Make and return an INSN rtx, initializing all its slots.
   Store PATTERN in the pattern slots.  */

rtx
make_insn_raw (pattern)
     rtx pattern;
{
  register rtx insn;

  /* If in RTL generation phase, see if FREE_INSN can be used.  */
  if (!ggc_p && free_insn != 0 && rtx_equal_function_value_matters)
    {
      insn = free_insn;
      free_insn = NEXT_INSN (free_insn);
      PUT_CODE (insn, INSN);
    }
  else
    insn = rtx_alloc (INSN);

  INSN_UID (insn) = cur_insn_uid++;
  PATTERN (insn) = pattern;
  INSN_CODE (insn) = -1;
  LOG_LINKS (insn) = NULL;
  REG_NOTES (insn) = NULL;

#ifdef ENABLE_RTL_CHECKING
  if (insn
      && GET_RTX_CLASS (GET_CODE (insn)) == 'i'
      && (returnjump_p (insn)
	  || (GET_CODE (insn) == SET
	      && SET_DEST (insn) == pc_rtx)))
    {
      warning ("ICE: emit_insn used where emit_jump_insn needed:\n");
      debug_rtx (insn);
    }
#endif
  
  return insn;
}

/* Like `make_insn' but make a JUMP_INSN instead of an insn.  */

static rtx
make_jump_insn_raw (pattern)
     rtx pattern;
{
  register rtx insn;

  insn = rtx_alloc (JUMP_INSN);
  INSN_UID (insn) = cur_insn_uid++;

  PATTERN (insn) = pattern;
  INSN_CODE (insn) = -1;
  LOG_LINKS (insn) = NULL;
  REG_NOTES (insn) = NULL;
  JUMP_LABEL (insn) = NULL;

  return insn;
}

/* Like `make_insn' but make a CALL_INSN instead of an insn.  */

static rtx
make_call_insn_raw (pattern)
     rtx pattern;
{
  register rtx insn;

  insn = rtx_alloc (CALL_INSN);
  INSN_UID (insn) = cur_insn_uid++;

  PATTERN (insn) = pattern;
  INSN_CODE (insn) = -1;
  LOG_LINKS (insn) = NULL;
  REG_NOTES (insn) = NULL;
  CALL_INSN_FUNCTION_USAGE (insn) = NULL;

  return insn;
}

/* Add INSN to the end of the doubly-linked list.
   INSN may be an INSN, JUMP_INSN, CALL_INSN, CODE_LABEL, BARRIER or NOTE.  */

void
add_insn (insn)
     register rtx insn;
{
  PREV_INSN (insn) = last_insn;
  NEXT_INSN (insn) = 0;

  if (NULL != last_insn)
    NEXT_INSN (last_insn) = insn;

  if (NULL == first_insn)
    first_insn = insn;

  last_insn = insn;
}

/* Add INSN into the doubly-linked list after insn AFTER.  This and
   the next should be the only functions called to insert an insn once
   delay slots have been filled since only they know how to update a
   SEQUENCE.  */

void
add_insn_after (insn, after)
     rtx insn, after;
{
  rtx next = NEXT_INSN (after);

  if (optimize && INSN_DELETED_P (after))
    abort ();

  NEXT_INSN (insn) = next;
  PREV_INSN (insn) = after;

  if (next)
    {
      PREV_INSN (next) = insn;
      if (GET_CODE (next) == INSN && GET_CODE (PATTERN (next)) == SEQUENCE)
	PREV_INSN (XVECEXP (PATTERN (next), 0, 0)) = insn;
    }
  else if (last_insn == after)
    last_insn = insn;
  else
    {
      struct sequence_stack *stack = seq_stack;
      /* Scan all pending sequences too.  */
      for (; stack; stack = stack->next)
	if (after == stack->last)
	  {
	    stack->last = insn;
	    break;
	  }

      if (stack == 0)
	abort ();
    }

  NEXT_INSN (after) = insn;
  if (GET_CODE (after) == INSN && GET_CODE (PATTERN (after)) == SEQUENCE)
    {
      rtx sequence = PATTERN (after);
      NEXT_INSN (XVECEXP (sequence, 0, XVECLEN (sequence, 0) - 1)) = insn;
    }
}

/* Add INSN into the doubly-linked list before insn BEFORE.  This and
   the previous should be the only functions called to insert an insn once
   delay slots have been filled since only they know how to update a
   SEQUENCE.  */

void
add_insn_before (insn, before)
     rtx insn, before;
{
  rtx prev = PREV_INSN (before);

  if (optimize && INSN_DELETED_P (before))
    abort ();

  PREV_INSN (insn) = prev;
  NEXT_INSN (insn) = before;

  if (prev)
    {
      NEXT_INSN (prev) = insn;
      if (GET_CODE (prev) == INSN && GET_CODE (PATTERN (prev)) == SEQUENCE)
	{
	  rtx sequence = PATTERN (prev);
	  NEXT_INSN (XVECEXP (sequence, 0, XVECLEN (sequence, 0) - 1)) = insn;
	}
    }
  else if (first_insn == before)
    first_insn = insn;
  else
    {
      struct sequence_stack *stack = seq_stack;
      /* Scan all pending sequences too.  */
      for (; stack; stack = stack->next)
	if (before == stack->first)
	  {
	    stack->first = insn;
	    break;
	  }

      if (stack == 0)
	abort ();
    }

  PREV_INSN (before) = insn;
  if (GET_CODE (before) == INSN && GET_CODE (PATTERN (before)) == SEQUENCE)
    PREV_INSN (XVECEXP (PATTERN (before), 0, 0)) = insn;
}

/* Remove an insn from its doubly-linked list.  This function knows how
   to handle sequences.  */
void
remove_insn (insn)
     rtx insn;
{
  rtx next = NEXT_INSN (insn);
  rtx prev = PREV_INSN (insn);
  if (prev)
    {
      NEXT_INSN (prev) = next;
      if (GET_CODE (prev) == INSN && GET_CODE (PATTERN (prev)) == SEQUENCE)
	{
	  rtx sequence = PATTERN (prev);
	  NEXT_INSN (XVECEXP (sequence, 0, XVECLEN (sequence, 0) - 1)) = next;
	}
    }
  else if (first_insn == insn)
    first_insn = next;
  else
    {
      struct sequence_stack *stack = seq_stack;
      /* Scan all pending sequences too.  */
      for (; stack; stack = stack->next)
	if (insn == stack->first)
	  {
	    stack->first = next;
	    break;
	  }

      if (stack == 0)
	abort ();
    }

  if (next)
    {
      PREV_INSN (next) = prev;
      if (GET_CODE (next) == INSN && GET_CODE (PATTERN (next)) == SEQUENCE)
	PREV_INSN (XVECEXP (PATTERN (next), 0, 0)) = prev;
    }
  else if (last_insn == insn)
    last_insn = prev;
  else
    {
      struct sequence_stack *stack = seq_stack;
      /* Scan all pending sequences too.  */
      for (; stack; stack = stack->next)
	if (insn == stack->last)
	  {
	    stack->last = prev;
	    break;
	  }

      if (stack == 0)
	abort ();
    }
}

/* Delete all insns made since FROM.
   FROM becomes the new last instruction.  */

void
delete_insns_since (from)
     rtx from;
{
  if (from == 0)
    first_insn = 0;
  else
    NEXT_INSN (from) = 0;
  last_insn = from;
}

/* This function is deprecated, please use sequences instead.

   Move a consecutive bunch of insns to a different place in the chain.
   The insns to be moved are those between FROM and TO.
   They are moved to a new position after the insn AFTER.
   AFTER must not be FROM or TO or any insn in between.

   This function does not know about SEQUENCEs and hence should not be
   called after delay-slot filling has been done.  */

void
reorder_insns (from, to, after)
     rtx from, to, after;
{
  /* Splice this bunch out of where it is now.  */
  if (PREV_INSN (from))
    NEXT_INSN (PREV_INSN (from)) = NEXT_INSN (to);
  if (NEXT_INSN (to))
    PREV_INSN (NEXT_INSN (to)) = PREV_INSN (from);
  if (last_insn == to)
    last_insn = PREV_INSN (from);
  if (first_insn == from)
    first_insn = NEXT_INSN (to);

  /* Make the new neighbors point to it and it to them.  */
  if (NEXT_INSN (after))
    PREV_INSN (NEXT_INSN (after)) = to;

  NEXT_INSN (to) = NEXT_INSN (after);
  PREV_INSN (from) = after;
  NEXT_INSN (after) = from;
  if (after == last_insn)
    last_insn = to;
}

/* Return the line note insn preceding INSN.  */

static rtx
find_line_note (insn)
     rtx insn;
{
  if (no_line_numbers)
    return 0;

  for (; insn; insn = PREV_INSN (insn))
    if (GET_CODE (insn) == NOTE
        && NOTE_LINE_NUMBER (insn) >= 0)
      break;

  return insn;
}

/* Like reorder_insns, but inserts line notes to preserve the line numbers
   of the moved insns when debugging.  This may insert a note between AFTER
   and FROM, and another one after TO.  */

void
reorder_insns_with_line_notes (from, to, after)
     rtx from, to, after;
{
  rtx from_line = find_line_note (from);
  rtx after_line = find_line_note (after);

  reorder_insns (from, to, after);

  if (from_line == after_line)
    return;

  if (from_line)
    emit_line_note_after (NOTE_SOURCE_FILE (from_line),
			  NOTE_LINE_NUMBER (from_line),
			  after);
  if (after_line)
    emit_line_note_after (NOTE_SOURCE_FILE (after_line),
			  NOTE_LINE_NUMBER (after_line),
			  to);
}

/* Remove unncessary notes from the instruction stream.  */

void
remove_unncessary_notes ()
{
  rtx insn;
  rtx next;

  /* We must not remove the first instruction in the function because
     the compiler depends on the first instruction being a note.  */
  for (insn = NEXT_INSN (get_insns ()); insn; insn = next)
    {
      /* Remember what's next.  */
      next = NEXT_INSN (insn);

      /* We're only interested in notes.  */
      if (GET_CODE (insn) != NOTE)
	continue;

      /* By now, all notes indicating lexical blocks should have
	 NOTE_BLOCK filled in.  */
      if ((NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_BEG
	   || NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_END)
	  && NOTE_BLOCK (insn) == NULL_TREE)
	abort ();

      /* Remove NOTE_INSN_DELETED notes.  */
      if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_DELETED)
	remove_insn (insn);
      else if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_END)
	{
	  /* Scan back to see if there are any non-note instructions
	     between INSN and the beginning of this block.  If not,
	     then there is no PC range in the generated code that will
	     actually be in this block, so there's no point in
	     remembering the existence of the block.  */
	  rtx prev;

	  for (prev = PREV_INSN (insn); prev; prev = PREV_INSN (prev))
	    {
	      /* This block contains a real instruction.  Note that we
		 don't include labels; if the only thing in the block
		 is a label, then there are still no PC values that
		 lie within the block.  */
	      if (GET_RTX_CLASS (GET_CODE (prev)) == 'i')
		break;

	      /* We're only interested in NOTEs.  */
	      if (GET_CODE (prev) != NOTE)
		continue;

	      if (NOTE_LINE_NUMBER (prev) == NOTE_INSN_BLOCK_BEG)
		{
		  /* If the BLOCKs referred to by these notes don't
		     match, then something is wrong with our BLOCK
		     nesting structure.  */
		  if (NOTE_BLOCK (prev) != NOTE_BLOCK (insn))
		    abort ();

		  /* Never delete the BLOCK for the outermost scope
		     of the function; we can refer to names from
		     that scope even if the block notes are messed up.  */
		  if (! is_body_block (NOTE_BLOCK (insn)))
		    {
		      debug_ignore_block (NOTE_BLOCK (insn));

		      remove_insn (prev);
		      remove_insn (insn);
		    }
		  break;
		}
	      else if (NOTE_LINE_NUMBER (prev) == NOTE_INSN_BLOCK_END)
		/* There's a nested block.  We need to leave the
		   current block in place since otherwise the debugger
		   wouldn't be able to show symbols from our block in
		   the nested block.  */
		break;
	    }
	}
    }
}


/* Emit an insn of given code and pattern
   at a specified place within the doubly-linked list.  */

/* Make an instruction with body PATTERN
   and output it before the instruction BEFORE.  */

rtx
emit_insn_before (pattern, before)
     register rtx pattern, before;
{
  register rtx insn = before;

  if (GET_CODE (pattern) == SEQUENCE)
    {
      register int i;

      for (i = 0; i < XVECLEN (pattern, 0); i++)
	{
	  insn = XVECEXP (pattern, 0, i);
	  add_insn_before (insn, before);
	}
      if (!ggc_p && XVECLEN (pattern, 0) < SEQUENCE_RESULT_SIZE)
	sequence_result[XVECLEN (pattern, 0)] = pattern;
    }
  else
    {
      insn = make_insn_raw (pattern);
      add_insn_before (insn, before);
    }

  return insn;
}

/* Similar to emit_insn_before, but update basic block boundaries as well.  */

rtx
emit_block_insn_before (pattern, before, block)
     rtx pattern, before;
     basic_block block;
{
  rtx prev = PREV_INSN (before);
  rtx r = emit_insn_before (pattern, before);
  if (block && block->head == before)
    block->head = NEXT_INSN (prev);
  return r;
}

/* Make an instruction with body PATTERN and code JUMP_INSN
   and output it before the instruction BEFORE.  */

rtx
emit_jump_insn_before (pattern, before)
     register rtx pattern, before;
{
  register rtx insn;

  if (GET_CODE (pattern) == SEQUENCE)
    insn = emit_insn_before (pattern, before);
  else
    {
      insn = make_jump_insn_raw (pattern);
      add_insn_before (insn, before);
    }

  return insn;
}

/* Make an instruction with body PATTERN and code CALL_INSN
   and output it before the instruction BEFORE.  */

rtx
emit_call_insn_before (pattern, before)
     register rtx pattern, before;
{
  register rtx insn;

  if (GET_CODE (pattern) == SEQUENCE)
    insn = emit_insn_before (pattern, before);
  else
    {
      insn = make_call_insn_raw (pattern);
      add_insn_before (insn, before);
      PUT_CODE (insn, CALL_INSN);
    }

  return insn;
}

/* Make an insn of code BARRIER
   and output it before the insn BEFORE.  */

rtx
emit_barrier_before (before)
     register rtx before;
{
  register rtx insn = rtx_alloc (BARRIER);

  INSN_UID (insn) = cur_insn_uid++;

  add_insn_before (insn, before);
  return insn;
}

/* Emit the label LABEL before the insn BEFORE.  */

rtx
emit_label_before (label, before)
     rtx label, before;
{
  /* This can be called twice for the same label as a result of the
     confusion that follows a syntax error!  So make it harmless.  */
  if (INSN_UID (label) == 0)
    {
      INSN_UID (label) = cur_insn_uid++;
      add_insn_before (label, before);
    }

  return label;
}

/* Emit a note of subtype SUBTYPE before the insn BEFORE.  */

rtx
emit_note_before (subtype, before)
     int subtype;
     rtx before;
{
  register rtx note = rtx_alloc (NOTE);
  INSN_UID (note) = cur_insn_uid++;
  NOTE_SOURCE_FILE (note) = 0;
  NOTE_LINE_NUMBER (note) = subtype;

  add_insn_before (note, before);
  return note;
}

/* Make an insn of code INSN with body PATTERN
   and output it after the insn AFTER.  */

rtx
emit_insn_after (pattern, after)
     register rtx pattern, after;
{
  register rtx insn = after;

  if (GET_CODE (pattern) == SEQUENCE)
    {
      register int i;

      for (i = 0; i < XVECLEN (pattern, 0); i++)
	{
	  insn = XVECEXP (pattern, 0, i);
	  add_insn_after (insn, after);
	  after = insn;
	}
      if (!ggc_p && XVECLEN (pattern, 0) < SEQUENCE_RESULT_SIZE)
	sequence_result[XVECLEN (pattern, 0)] = pattern;
    }
  else
    {
      insn = make_insn_raw (pattern);
      add_insn_after (insn, after);
    }

  return insn;
}

/* Similar to emit_insn_after, except that line notes are to be inserted so
   as to act as if this insn were at FROM.  */

void
emit_insn_after_with_line_notes (pattern, after, from)
     rtx pattern, after, from;
{
  rtx from_line = find_line_note (from);
  rtx after_line = find_line_note (after);
  rtx insn = emit_insn_after (pattern, after);

  if (from_line)
    emit_line_note_after (NOTE_SOURCE_FILE (from_line),
			  NOTE_LINE_NUMBER (from_line),
			  after);

  if (after_line)
    emit_line_note_after (NOTE_SOURCE_FILE (after_line),
			  NOTE_LINE_NUMBER (after_line),
			  insn);
}

/* Similar to emit_insn_after, but update basic block boundaries as well.  */

rtx
emit_block_insn_after (pattern, after, block)
     rtx pattern, after;
     basic_block block;
{
  rtx r = emit_insn_after (pattern, after);
  if (block && block->end == after)
    block->end = r;
  return r;
}

/* Make an insn of code JUMP_INSN with body PATTERN
   and output it after the insn AFTER.  */

rtx
emit_jump_insn_after (pattern, after)
     register rtx pattern, after;
{
  register rtx insn;

  if (GET_CODE (pattern) == SEQUENCE)
    insn = emit_insn_after (pattern, after);
  else
    {
      insn = make_jump_insn_raw (pattern);
      add_insn_after (insn, after);
    }

  return insn;
}

/* Make an insn of code BARRIER
   and output it after the insn AFTER.  */

rtx
emit_barrier_after (after)
     register rtx after;
{
  register rtx insn = rtx_alloc (BARRIER);

  INSN_UID (insn) = cur_insn_uid++;

  add_insn_after (insn, after);
  return insn;
}

/* Emit the label LABEL after the insn AFTER.  */

rtx
emit_label_after (label, after)
     rtx label, after;
{
  /* This can be called twice for the same label
     as a result of the confusion that follows a syntax error!
     So make it harmless.  */
  if (INSN_UID (label) == 0)
    {
      INSN_UID (label) = cur_insn_uid++;
      add_insn_after (label, after);
    }

  return label;
}

/* Emit a note of subtype SUBTYPE after the insn AFTER.  */

rtx
emit_note_after (subtype, after)
     int subtype;
     rtx after;
{
  register rtx note = rtx_alloc (NOTE);
  INSN_UID (note) = cur_insn_uid++;
  NOTE_SOURCE_FILE (note) = 0;
  NOTE_LINE_NUMBER (note) = subtype;
  add_insn_after (note, after);
  return note;
}

/* Emit a line note for FILE and LINE after the insn AFTER.  */

rtx
emit_line_note_after (file, line, after)
     const char *file;
     int line;
     rtx after;
{
  register rtx note;

  if (no_line_numbers && line > 0)
    {
      cur_insn_uid++;
      return 0;
    }

  note  = rtx_alloc (NOTE);
  INSN_UID (note) = cur_insn_uid++;
  NOTE_SOURCE_FILE (note) = file;
  NOTE_LINE_NUMBER (note) = line;
  add_insn_after (note, after);
  return note;
}

/* Make an insn of code INSN with pattern PATTERN
   and add it to the end of the doubly-linked list.
   If PATTERN is a SEQUENCE, take the elements of it
   and emit an insn for each element.

   Returns the last insn emitted.  */

rtx
emit_insn (pattern)
     rtx pattern;
{
  rtx insn = last_insn;

  if (GET_CODE (pattern) == SEQUENCE)
    {
      register int i;

      for (i = 0; i < XVECLEN (pattern, 0); i++)
	{
	  insn = XVECEXP (pattern, 0, i);
	  add_insn (insn);
	}
      if (!ggc_p && XVECLEN (pattern, 0) < SEQUENCE_RESULT_SIZE)
	sequence_result[XVECLEN (pattern, 0)] = pattern;
    }
  else
    {
      insn = make_insn_raw (pattern);
      add_insn (insn);
    }

  return insn;
}

/* Emit the insns in a chain starting with INSN.
   Return the last insn emitted.  */

rtx
emit_insns (insn)
     rtx insn;
{
  rtx last = 0;

  while (insn)
    {
      rtx next = NEXT_INSN (insn);
      add_insn (insn);
      last = insn;
      insn = next;
    }

  return last;
}

/* Emit the insns in a chain starting with INSN and place them in front of
   the insn BEFORE.  Return the last insn emitted.  */

rtx
emit_insns_before (insn, before)
     rtx insn;
     rtx before;
{
  rtx last = 0;

  while (insn)
    {
      rtx next = NEXT_INSN (insn);
      add_insn_before (insn, before);
      last = insn;
      insn = next;
    }

  return last;
}

/* Emit the insns in a chain starting with FIRST and place them in back of
   the insn AFTER.  Return the last insn emitted.  */

rtx
emit_insns_after (first, after)
     register rtx first;
     register rtx after;
{
  register rtx last;
  register rtx after_after;

  if (!after)
    abort ();

  if (!first)
    return first;

  for (last = first; NEXT_INSN (last); last = NEXT_INSN (last))
    continue;

  after_after = NEXT_INSN (after);

  NEXT_INSN (after) = first;
  PREV_INSN (first) = after;
  NEXT_INSN (last) = after_after;
  if (after_after)
    PREV_INSN (after_after) = last;

  if (after == last_insn)
    last_insn = last;
  return last;
}

/* Make an insn of code JUMP_INSN with pattern PATTERN
   and add it to the end of the doubly-linked list.  */

rtx
emit_jump_insn (pattern)
     rtx pattern;
{
  if (GET_CODE (pattern) == SEQUENCE)
    return emit_insn (pattern);
  else
    {
      register rtx insn = make_jump_insn_raw (pattern);
      add_insn (insn);
      return insn;
    }
}

/* Make an insn of code CALL_INSN with pattern PATTERN
   and add it to the end of the doubly-linked list.  */

rtx
emit_call_insn (pattern)
     rtx pattern;
{
  if (GET_CODE (pattern) == SEQUENCE)
    return emit_insn (pattern);
  else
    {
      register rtx insn = make_call_insn_raw (pattern);
      add_insn (insn);
      PUT_CODE (insn, CALL_INSN);
      return insn;
    }
}

/* Add the label LABEL to the end of the doubly-linked list.  */

rtx
emit_label (label)
     rtx label;
{
  /* This can be called twice for the same label
     as a result of the confusion that follows a syntax error!
     So make it harmless.  */
  if (INSN_UID (label) == 0)
    {
      INSN_UID (label) = cur_insn_uid++;
      add_insn (label);
    }
  return label;
}

/* Make an insn of code BARRIER
   and add it to the end of the doubly-linked list.  */

rtx
emit_barrier ()
{
  register rtx barrier = rtx_alloc (BARRIER);
  INSN_UID (barrier) = cur_insn_uid++;
  add_insn (barrier);
  return barrier;
}

/* Make an insn of code NOTE
   with data-fields specified by FILE and LINE
   and add it to the end of the doubly-linked list,
   but only if line-numbers are desired for debugging info.  */

rtx
emit_line_note (file, line)
     const char *file;
     int line;
{
  set_file_and_line_for_stmt (file, line);

#if 0
  if (no_line_numbers)
    return 0;
#endif

  return emit_note (file, line);
}

/* Make an insn of code NOTE
   with data-fields specified by FILE and LINE
   and add it to the end of the doubly-linked list.
   If it is a line-number NOTE, omit it if it matches the previous one.  */

rtx
emit_note (file, line)
     const char *file;
     int line;
{
  register rtx note;

  if (line > 0)
    {
      if (file && last_filename && !strcmp (file, last_filename)
	  && line == last_linenum)
	return 0;
      last_filename = file;
      last_linenum = line;
    }

  if (no_line_numbers && line > 0)
    {
      cur_insn_uid++;
      return 0;
    }

  note = rtx_alloc (NOTE);
  INSN_UID (note) = cur_insn_uid++;
  NOTE_SOURCE_FILE (note) = file;
  NOTE_LINE_NUMBER (note) = line;
  add_insn (note);
  return note;
}

/* Emit a NOTE, and don't omit it even if LINE is the previous note.  */

rtx
emit_line_note_force (file, line)
     const char *file;
     int line;
{
  last_linenum = -1;
  return emit_line_note (file, line);
}

/* Cause next statement to emit a line note even if the line number
   has not changed.  This is used at the beginning of a function.  */

void
force_next_line_note ()
{
  last_linenum = -1;
}

/* Place a note of KIND on insn INSN with DATUM as the datum. If a
   note of this type already exists, remove it first. */

void 
set_unique_reg_note (insn, kind, datum)
     rtx insn;
     enum reg_note kind;
     rtx datum;
{
  rtx note = find_reg_note (insn, kind, NULL_RTX);

  /* First remove the note if there already is one.  */
  if (note) 
    remove_note (insn, note);

  REG_NOTES (insn) = gen_rtx_EXPR_LIST (kind, datum, REG_NOTES (insn));
}

/* Return an indication of which type of insn should have X as a body.
   The value is CODE_LABEL, INSN, CALL_INSN or JUMP_INSN.  */

enum rtx_code
classify_insn (x)
     rtx x;
{
  if (GET_CODE (x) == CODE_LABEL)
    return CODE_LABEL;
  if (GET_CODE (x) == CALL)
    return CALL_INSN;
  if (GET_CODE (x) == RETURN)
    return JUMP_INSN;
  if (GET_CODE (x) == SET)
    {
      if (SET_DEST (x) == pc_rtx)
	return JUMP_INSN;
      else if (GET_CODE (SET_SRC (x)) == CALL)
	return CALL_INSN;
      else
	return INSN;
    }
  if (GET_CODE (x) == PARALLEL)
    {
      register int j;
      for (j = XVECLEN (x, 0) - 1; j >= 0; j--)
	if (GET_CODE (XVECEXP (x, 0, j)) == CALL)
	  return CALL_INSN;
	else if (GET_CODE (XVECEXP (x, 0, j)) == SET
		 && SET_DEST (XVECEXP (x, 0, j)) == pc_rtx)
	  return JUMP_INSN;
	else if (GET_CODE (XVECEXP (x, 0, j)) == SET
		 && GET_CODE (SET_SRC (XVECEXP (x, 0, j))) == CALL)
	  return CALL_INSN;
    }
  return INSN;
}

/* Emit the rtl pattern X as an appropriate kind of insn.
   If X is a label, it is simply added into the insn chain.  */

rtx
emit (x)
     rtx x;
{
  enum rtx_code code = classify_insn (x);

  if (code == CODE_LABEL)
    return emit_label (x);
  else if (code == INSN)
    return emit_insn (x);
  else if (code == JUMP_INSN)
    {
      register rtx insn = emit_jump_insn (x);
      if (simplejump_p (insn) || GET_CODE (x) == RETURN)
	return emit_barrier ();
      return insn;
    }
  else if (code == CALL_INSN)
    return emit_call_insn (x);
  else
    abort ();
}

/* Begin emitting insns to a sequence which can be packaged in an
   RTL_EXPR.  If this sequence will contain something that might cause
   the compiler to pop arguments to function calls (because those
   pops have previously been deferred; see INHIBIT_DEFER_POP for more
   details), use do_pending_stack_adjust before calling this function.
   That will ensure that the deferred pops are not accidentally
   emitted in the middel of this sequence.  */

void
start_sequence ()
{
  struct sequence_stack *tem;

  tem = (struct sequence_stack *) xmalloc (sizeof (struct sequence_stack));

  tem->next = seq_stack;
  tem->first = first_insn;
  tem->last = last_insn;
  tem->sequence_rtl_expr = seq_rtl_expr;

  seq_stack = tem;

  first_insn = 0;
  last_insn = 0;
}

/* Similarly, but indicate that this sequence will be placed in T, an
   RTL_EXPR.  See the documentation for start_sequence for more
   information about how to use this function.  */

void
start_sequence_for_rtl_expr (t)
     tree t;
{
  start_sequence ();

  seq_rtl_expr = t;
}

/* Set up the insn chain starting with FIRST as the current sequence,
   saving the previously current one.  See the documentation for
   start_sequence for more information about how to use this function.  */

void
push_to_sequence (first)
     rtx first;
{
  rtx last;

  start_sequence ();

  for (last = first; last && NEXT_INSN (last); last = NEXT_INSN (last));

  first_insn = first;
  last_insn = last;
}

/* Set up the insn chain from a chain stort in FIRST to LAST.  */

void
push_to_full_sequence (first, last)
     rtx first, last;
{
  start_sequence ();
  first_insn = first;
  last_insn = last;
  /* We really should have the end of the insn chain here.  */
  if (last && NEXT_INSN (last))
    abort ();
}

/* Set up the outer-level insn chain
   as the current sequence, saving the previously current one.  */

void
push_topmost_sequence ()
{
  struct sequence_stack *stack, *top = NULL;

  start_sequence ();

  for (stack = seq_stack; stack; stack = stack->next)
    top = stack;

  first_insn = top->first;
  last_insn = top->last;
  seq_rtl_expr = top->sequence_rtl_expr;
}

/* After emitting to the outer-level insn chain, update the outer-level
   insn chain, and restore the previous saved state.  */

void
pop_topmost_sequence ()
{
  struct sequence_stack *stack, *top = NULL;

  for (stack = seq_stack; stack; stack = stack->next)
    top = stack;

  top->first = first_insn;
  top->last = last_insn;
  /* ??? Why don't we save seq_rtl_expr here?  */

  end_sequence ();
}

/* After emitting to a sequence, restore previous saved state.

   To get the contents of the sequence just made, you must call
   `gen_sequence' *before* calling here.  

   If the compiler might have deferred popping arguments while
   generating this sequence, and this sequence will not be immediately
   inserted into the instruction stream, use do_pending_stack_adjust
   before calling gen_sequence.  That will ensure that the deferred
   pops are inserted into this sequence, and not into some random
   location in the instruction stream.  See INHIBIT_DEFER_POP for more
   information about deferred popping of arguments.  */

void
end_sequence ()
{
  struct sequence_stack *tem = seq_stack;

  first_insn = tem->first;
  last_insn = tem->last;
  seq_rtl_expr = tem->sequence_rtl_expr;
  seq_stack = tem->next;

  free (tem);
}

/* This works like end_sequence, but records the old sequence in FIRST
   and LAST.  */

void
end_full_sequence (first, last)
     rtx *first, *last;
{
  *first = first_insn;
  *last = last_insn;
  end_sequence();
}

/* Return 1 if currently emitting into a sequence.  */

int
in_sequence_p ()
{
  return seq_stack != 0;
}

/* Generate a SEQUENCE rtx containing the insns already emitted
   to the current sequence.

   This is how the gen_... function from a DEFINE_EXPAND
   constructs the SEQUENCE that it returns.  */

rtx
gen_sequence ()
{
  rtx result;
  rtx tem;
  int i;
  int len;

  /* Count the insns in the chain.  */
  len = 0;
  for (tem = first_insn; tem; tem = NEXT_INSN (tem))
    len++;

  /* If only one insn, return it rather than a SEQUENCE.
     (Now that we cache SEQUENCE expressions, it isn't worth special-casing
     the case of an empty list.)     
     We only return the pattern of an insn if its code is INSN and it
     has no notes.  This ensures that no information gets lost.  */
  if (len == 1
      && ! RTX_FRAME_RELATED_P (first_insn)
      && GET_CODE (first_insn) == INSN
      /* Don't throw away any reg notes. */
      && REG_NOTES (first_insn) == 0)
    {
      if (!ggc_p)
	{
	  NEXT_INSN (first_insn) = free_insn;
	  free_insn = first_insn;
	}
      return PATTERN (first_insn);
    }

  /* Put them in a vector.  See if we already have a SEQUENCE of the
     appropriate length around.  */
  if (!ggc_p && len < SEQUENCE_RESULT_SIZE 
      && (result = sequence_result[len]) != 0)
    sequence_result[len] = 0;
  else
    {
      /* Ensure that this rtl goes in saveable_obstack, since we may
	 cache it.  */
      push_obstacks_nochange ();
      rtl_in_saveable_obstack ();
      result = gen_rtx_SEQUENCE (VOIDmode, rtvec_alloc (len));
      pop_obstacks ();
    }

  for (i = 0, tem = first_insn; tem; tem = NEXT_INSN (tem), i++)
    XVECEXP (result, 0, i) = tem;

  return result;
}

/* Put the various virtual registers into REGNO_REG_RTX.  */

void
init_virtual_regs (es)
     struct emit_status *es;
{
  rtx *ptr = es->x_regno_reg_rtx;
  ptr[VIRTUAL_INCOMING_ARGS_REGNUM] = virtual_incoming_args_rtx;
  ptr[VIRTUAL_STACK_VARS_REGNUM] = virtual_stack_vars_rtx;
  ptr[VIRTUAL_STACK_DYNAMIC_REGNUM] = virtual_stack_dynamic_rtx;
  ptr[VIRTUAL_OUTGOING_ARGS_REGNUM] = virtual_outgoing_args_rtx;
  ptr[VIRTUAL_CFA_REGNUM] = virtual_cfa_rtx;
}

void
clear_emit_caches ()
{
  int i;

  /* Clear the start_sequence/gen_sequence cache.  */
  for (i = 0; i < SEQUENCE_RESULT_SIZE; i++)
    sequence_result[i] = 0;
  free_insn = 0;
}

/* Used by copy_insn_1 to avoid copying SCRATCHes more than once.  */
static rtx copy_insn_scratch_in[MAX_RECOG_OPERANDS];
static rtx copy_insn_scratch_out[MAX_RECOG_OPERANDS];
static int copy_insn_n_scratches;

/* When an insn is being copied by copy_insn_1, this is nonzero if we have
   copied an ASM_OPERANDS.
   In that case, it is the original input-operand vector.  */
static rtvec orig_asm_operands_vector;

/* When an insn is being copied by copy_insn_1, this is nonzero if we have
   copied an ASM_OPERANDS.
   In that case, it is the copied input-operand vector.  */
static rtvec copy_asm_operands_vector;

/* Likewise for the constraints vector.  */
static rtvec orig_asm_constraints_vector;
static rtvec copy_asm_constraints_vector;

/* Recursively create a new copy of an rtx for copy_insn.
   This function differs from copy_rtx in that it handles SCRATCHes and
   ASM_OPERANDs properly.
   Normally, this function is not used directly; use copy_insn as front end.
   However, you could first copy an insn pattern with copy_insn and then use
   this function afterwards to properly copy any REG_NOTEs containing
   SCRATCHes.  */

rtx
copy_insn_1 (orig)
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
    case ADDRESSOF:
      return orig;

    case SCRATCH:
      for (i = 0; i < copy_insn_n_scratches; i++)
	if (copy_insn_scratch_in[i] == orig)
	  return copy_insn_scratch_out[i];
      break;

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

  /* We do not copy JUMP, CALL, or FRAME_RELATED for INSNs.  */
  if (GET_RTX_CLASS (code) == 'i')
    {
      copy->jump = 0;
      copy->call = 0;
      copy->frame_related = 0;
    }
  
  format_ptr = GET_RTX_FORMAT (GET_CODE (copy));

  for (i = 0; i < GET_RTX_LENGTH (GET_CODE (copy)); i++)
    {
      copy->fld[i] = orig->fld[i];
      switch (*format_ptr++)
	{
	case 'e':
	  if (XEXP (orig, i) != NULL)
	    XEXP (copy, i) = copy_insn_1 (XEXP (orig, i));
	  break;

	case 'E':
	case 'V':
	  if (XVEC (orig, i) == orig_asm_constraints_vector)
	    XVEC (copy, i) = copy_asm_constraints_vector;
	  else if (XVEC (orig, i) == orig_asm_operands_vector)
	    XVEC (copy, i) = copy_asm_operands_vector;
	  else if (XVEC (orig, i) != NULL)
	    {
	      XVEC (copy, i) = rtvec_alloc (XVECLEN (orig, i));
	      for (j = 0; j < XVECLEN (copy, i); j++)
		XVECEXP (copy, i, j) = copy_insn_1 (XVECEXP (orig, i, j));
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

  if (code == SCRATCH)
    {
      i = copy_insn_n_scratches++;
      if (i >= MAX_RECOG_OPERANDS)
	abort ();
      copy_insn_scratch_in[i] = orig;
      copy_insn_scratch_out[i] = copy;
    }
  else if (code == ASM_OPERANDS)
    {
      orig_asm_operands_vector = XVEC (orig, 3);
      copy_asm_operands_vector = XVEC (copy, 3);
      orig_asm_constraints_vector = XVEC (orig, 4);
      copy_asm_constraints_vector = XVEC (copy, 4);
    }

  return copy;
}

/* Create a new copy of an rtx.
   This function differs from copy_rtx in that it handles SCRATCHes and
   ASM_OPERANDs properly.
   INSN doesn't really have to be a full INSN; it could be just the
   pattern.  */
rtx
copy_insn (insn)
     rtx insn;
{
  copy_insn_n_scratches = 0;
  orig_asm_operands_vector = 0;
  orig_asm_constraints_vector = 0;
  copy_asm_operands_vector = 0;
  copy_asm_constraints_vector = 0;
  return copy_insn_1 (insn);
}

/* Initialize data structures and variables in this file
   before generating rtl for each function.  */

void
init_emit ()
{
  struct function *f = cfun;

  f->emit = (struct emit_status *) xmalloc (sizeof (struct emit_status));
  first_insn = NULL;
  last_insn = NULL;
  seq_rtl_expr = NULL;
  cur_insn_uid = 1;
  reg_rtx_no = LAST_VIRTUAL_REGISTER + 1;
  last_linenum = 0;
  last_filename = 0;
  first_label_num = label_num;
  last_label_num = 0;
  seq_stack = NULL;

  clear_emit_caches ();

  /* Init the tables that describe all the pseudo regs.  */

  f->emit->regno_pointer_flag_length = LAST_VIRTUAL_REGISTER + 101;

  f->emit->regno_pointer_flag 
    = (char *) xcalloc (f->emit->regno_pointer_flag_length, sizeof (char));

  f->emit->regno_pointer_align
    = (char *) xcalloc (f->emit->regno_pointer_flag_length,
			sizeof (char));

  regno_reg_rtx 
    = (rtx *) xcalloc (f->emit->regno_pointer_flag_length * sizeof (rtx),
		       sizeof (rtx));

  /* Put copies of all the virtual register rtx into regno_reg_rtx.  */
  init_virtual_regs (f->emit);

  /* Indicate that the virtual registers and stack locations are
     all pointers.  */
  REGNO_POINTER_FLAG (STACK_POINTER_REGNUM) = 1;
  REGNO_POINTER_FLAG (FRAME_POINTER_REGNUM) = 1;
  REGNO_POINTER_FLAG (HARD_FRAME_POINTER_REGNUM) = 1;
  REGNO_POINTER_FLAG (ARG_POINTER_REGNUM) = 1;

  REGNO_POINTER_FLAG (VIRTUAL_INCOMING_ARGS_REGNUM) = 1;
  REGNO_POINTER_FLAG (VIRTUAL_STACK_VARS_REGNUM) = 1;
  REGNO_POINTER_FLAG (VIRTUAL_STACK_DYNAMIC_REGNUM) = 1;
  REGNO_POINTER_FLAG (VIRTUAL_OUTGOING_ARGS_REGNUM) = 1;
  REGNO_POINTER_FLAG (VIRTUAL_CFA_REGNUM) = 1;

#ifdef STACK_BOUNDARY
  REGNO_POINTER_ALIGN (STACK_POINTER_REGNUM) = STACK_BOUNDARY;
  REGNO_POINTER_ALIGN (FRAME_POINTER_REGNUM) = STACK_BOUNDARY;
  REGNO_POINTER_ALIGN (HARD_FRAME_POINTER_REGNUM) = STACK_BOUNDARY;
  REGNO_POINTER_ALIGN (ARG_POINTER_REGNUM) = STACK_BOUNDARY;

  REGNO_POINTER_ALIGN (VIRTUAL_INCOMING_ARGS_REGNUM) = STACK_BOUNDARY;
  REGNO_POINTER_ALIGN (VIRTUAL_STACK_VARS_REGNUM) = STACK_BOUNDARY;
  REGNO_POINTER_ALIGN (VIRTUAL_STACK_DYNAMIC_REGNUM) = STACK_BOUNDARY;
  REGNO_POINTER_ALIGN (VIRTUAL_OUTGOING_ARGS_REGNUM) = STACK_BOUNDARY;
  REGNO_POINTER_ALIGN (VIRTUAL_CFA_REGNUM) = BITS_PER_WORD;
#endif

#ifdef INIT_EXPANDERS
  INIT_EXPANDERS;
#endif
}

/* Mark SS for GC.  */

static void
mark_sequence_stack (ss)
     struct sequence_stack *ss;
{
  while (ss)
    {
      ggc_mark_rtx (ss->first);
      ggc_mark_tree (ss->sequence_rtl_expr);
      ss = ss->next;
    }
}

/* Mark ES for GC.  */

void
mark_emit_status (es)
     struct emit_status *es;
{
  rtx *r;
  int i;

  if (es == 0)
    return;

  for (i = es->regno_pointer_flag_length, r = es->x_regno_reg_rtx;
       i > 0; --i, ++r)
    ggc_mark_rtx (*r);

  mark_sequence_stack (es->sequence_stack);
  ggc_mark_tree (es->sequence_rtl_expr);
  ggc_mark_rtx (es->x_first_insn);
}

/* Create some permanent unique rtl objects shared between all functions.
   LINE_NUMBERS is nonzero if line numbers are to be generated.  */

void
init_emit_once (line_numbers)
     int line_numbers;
{
  int i;
  enum machine_mode mode;
  enum machine_mode double_mode;

  no_line_numbers = ! line_numbers;

  /* Compute the word and byte modes.  */

  byte_mode = VOIDmode;
  word_mode = VOIDmode;
  double_mode = VOIDmode;

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_INT); mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    {
      if (GET_MODE_BITSIZE (mode) == BITS_PER_UNIT
	  && byte_mode == VOIDmode)
	byte_mode = mode;

      if (GET_MODE_BITSIZE (mode) == BITS_PER_WORD
	  && word_mode == VOIDmode)
	word_mode = mode;
    }

#ifndef DOUBLE_TYPE_SIZE
#define DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_FLOAT); mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    {
      if (GET_MODE_BITSIZE (mode) == DOUBLE_TYPE_SIZE
	  && double_mode == VOIDmode)
	double_mode = mode;
    }

  ptr_mode = mode_for_size (POINTER_SIZE, GET_MODE_CLASS (Pmode), 0);

  /* Assign register numbers to the globally defined register rtx.
     This must be done at runtime because the register number field
     is in a union and some compilers can't initialize unions.  */

  pc_rtx = gen_rtx (PC, VOIDmode);
  cc0_rtx = gen_rtx (CC0, VOIDmode);
  stack_pointer_rtx = gen_rtx_raw_REG (Pmode, STACK_POINTER_REGNUM);
  frame_pointer_rtx = gen_rtx_raw_REG (Pmode, FRAME_POINTER_REGNUM);
  if (hard_frame_pointer_rtx == 0)
    hard_frame_pointer_rtx = gen_rtx_raw_REG (Pmode, 
					      HARD_FRAME_POINTER_REGNUM);
  if (arg_pointer_rtx == 0)
    arg_pointer_rtx = gen_rtx_raw_REG (Pmode, ARG_POINTER_REGNUM);
  virtual_incoming_args_rtx = 
    gen_rtx_raw_REG (Pmode, VIRTUAL_INCOMING_ARGS_REGNUM);
  virtual_stack_vars_rtx = 
    gen_rtx_raw_REG (Pmode, VIRTUAL_STACK_VARS_REGNUM);
  virtual_stack_dynamic_rtx = 
    gen_rtx_raw_REG (Pmode, VIRTUAL_STACK_DYNAMIC_REGNUM);
  virtual_outgoing_args_rtx = 
    gen_rtx_raw_REG (Pmode, VIRTUAL_OUTGOING_ARGS_REGNUM); 
  virtual_cfa_rtx = gen_rtx_raw_REG (Pmode, VIRTUAL_CFA_REGNUM);

  /* These rtx must be roots if GC is enabled.  */
  if (ggc_p)
    ggc_add_rtx_root (global_rtl, GR_MAX);

#ifdef INIT_EXPANDERS
  /* This is to initialize save_machine_status and restore_machine_status before
     the first call to push_function_context_to.  This is needed by the Chill
     front end which calls push_function_context_to before the first cal to
     init_function_start.  */
  INIT_EXPANDERS;
#endif

  /* Create the unique rtx's for certain rtx codes and operand values.  */

  /* Don't use gen_rtx here since gen_rtx in this case
     tries to use these variables.  */
  for (i = - MAX_SAVED_CONST_INT; i <= MAX_SAVED_CONST_INT; i++)
    const_int_rtx[i + MAX_SAVED_CONST_INT] = 
      gen_rtx_raw_CONST_INT (VOIDmode, i);
  if (ggc_p)
    ggc_add_rtx_root (const_int_rtx, 2 * MAX_SAVED_CONST_INT + 1);

  if (STORE_FLAG_VALUE >= - MAX_SAVED_CONST_INT
      && STORE_FLAG_VALUE <= MAX_SAVED_CONST_INT)
    const_true_rtx = const_int_rtx[STORE_FLAG_VALUE + MAX_SAVED_CONST_INT];
  else
    const_true_rtx = gen_rtx_CONST_INT (VOIDmode, STORE_FLAG_VALUE);

  dconst0 = REAL_VALUE_ATOF ("0", double_mode);
  dconst1 = REAL_VALUE_ATOF ("1", double_mode);
  dconst2 = REAL_VALUE_ATOF ("2", double_mode);
  dconstm1 = REAL_VALUE_ATOF ("-1", double_mode);

  for (i = 0; i <= 2; i++)
    {
      for (mode = GET_CLASS_NARROWEST_MODE (MODE_FLOAT); mode != VOIDmode;
	   mode = GET_MODE_WIDER_MODE (mode))
	{
	  rtx tem = rtx_alloc (CONST_DOUBLE);
	  union real_extract u;

	  bzero ((char *) &u, sizeof u);  /* Zero any holes in a structure.  */
	  u.d = i == 0 ? dconst0 : i == 1 ? dconst1 : dconst2;

	  bcopy ((char *) &u, (char *) &CONST_DOUBLE_LOW (tem), sizeof u);
	  CONST_DOUBLE_MEM (tem) = cc0_rtx;
	  PUT_MODE (tem, mode);

	  const_tiny_rtx[i][(int) mode] = tem;
	}

      const_tiny_rtx[i][(int) VOIDmode] = GEN_INT (i);

      for (mode = GET_CLASS_NARROWEST_MODE (MODE_INT); mode != VOIDmode;
	   mode = GET_MODE_WIDER_MODE (mode))
	const_tiny_rtx[i][(int) mode] = GEN_INT (i);

      for (mode = GET_CLASS_NARROWEST_MODE (MODE_PARTIAL_INT);
	   mode != VOIDmode;
	   mode = GET_MODE_WIDER_MODE (mode))
	const_tiny_rtx[i][(int) mode] = GEN_INT (i);
    }

  for (mode = CCmode; mode < MAX_MACHINE_MODE; ++mode)
    if (GET_MODE_CLASS (mode) == MODE_CC)
      const_tiny_rtx[0][(int) mode] = const0_rtx;

  ggc_add_rtx_root (&const_tiny_rtx[0][0], sizeof(const_tiny_rtx)/sizeof(rtx));
  ggc_add_rtx_root (&const_true_rtx, 1);

#ifdef RETURN_ADDRESS_POINTER_REGNUM
  return_address_pointer_rtx
    = gen_rtx_raw_REG (Pmode, RETURN_ADDRESS_POINTER_REGNUM);
#endif

#ifdef STRUCT_VALUE
  struct_value_rtx = STRUCT_VALUE;
#else
  struct_value_rtx = gen_rtx_REG (Pmode, STRUCT_VALUE_REGNUM);
#endif

#ifdef STRUCT_VALUE_INCOMING
  struct_value_incoming_rtx = STRUCT_VALUE_INCOMING;
#else
#ifdef STRUCT_VALUE_INCOMING_REGNUM
  struct_value_incoming_rtx
    = gen_rtx_REG (Pmode, STRUCT_VALUE_INCOMING_REGNUM);
#else
  struct_value_incoming_rtx = struct_value_rtx;
#endif
#endif

#ifdef STATIC_CHAIN_REGNUM
  static_chain_rtx = gen_rtx_REG (Pmode, STATIC_CHAIN_REGNUM);

#ifdef STATIC_CHAIN_INCOMING_REGNUM
  if (STATIC_CHAIN_INCOMING_REGNUM != STATIC_CHAIN_REGNUM)
    static_chain_incoming_rtx
      = gen_rtx_REG (Pmode, STATIC_CHAIN_INCOMING_REGNUM);
  else
#endif
    static_chain_incoming_rtx = static_chain_rtx;
#endif

#ifdef STATIC_CHAIN
  static_chain_rtx = STATIC_CHAIN;

#ifdef STATIC_CHAIN_INCOMING
  static_chain_incoming_rtx = STATIC_CHAIN_INCOMING;
#else
  static_chain_incoming_rtx = static_chain_rtx;
#endif
#endif

#ifdef PIC_OFFSET_TABLE_REGNUM
  pic_offset_table_rtx = gen_rtx_REG (Pmode, PIC_OFFSET_TABLE_REGNUM);
#endif

  ggc_add_rtx_root (&pic_offset_table_rtx, 1);
  ggc_add_rtx_root (&struct_value_rtx, 1);
  ggc_add_rtx_root (&struct_value_incoming_rtx, 1);
  ggc_add_rtx_root (&static_chain_rtx, 1);
  ggc_add_rtx_root (&static_chain_incoming_rtx, 1);
  ggc_add_rtx_root (&return_address_pointer_rtx, 1);

  /* Initialize the CONST_INT hash table.  */
  const_int_htab = htab_create (37, 
				const_int_htab_hash, 
				const_int_htab_eq, 
				NULL);
  ggc_add_root (&const_int_htab, 1, sizeof (const_int_htab), 
		rtx_htab_mark);
}

/* Query and clear/ restore no_line_numbers.  This is used by the
   switch / case handling in stmt.c to give proper line numbers in
   warnings about unreachable code.  */

int
force_line_numbers ()
{
  int old = no_line_numbers;

  no_line_numbers = 0;
  if (old)
    force_next_line_note ();
  return old;
}

void
restore_line_number_status (old_value)
     int old_value;
{
  no_line_numbers = old_value;
}
