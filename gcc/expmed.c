/* Medium-level subroutines: convert bit-field store and extract
   and shifts, multiplies and divides to rtl instructions.
   Copyright (C) 1987, 88, 89, 92-97, 1998 Free Software Foundation, Inc.

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
#include "toplev.h"
#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "insn-flags.h"
#include "insn-codes.h"
#include "insn-config.h"
#include "expr.h"
#include "real.h"
#include "recog.h"

static void store_fixed_bit_field	PROTO((rtx, int, int, int, rtx, int));
static void store_split_bit_field	PROTO((rtx, int, int, rtx, int));
static rtx extract_fixed_bit_field	PROTO((enum machine_mode, rtx, int,
					       int, int, rtx, int, int));
static rtx mask_rtx			PROTO((enum machine_mode, int,
					       int, int));
static rtx lshift_value			PROTO((enum machine_mode, rtx,
					       int, int));
static rtx extract_split_bit_field	PROTO((rtx, int, int, int, int));
static void do_cmp_and_jump		PROTO((rtx, rtx, enum rtx_code,
					       enum machine_mode, rtx));

#define CEIL(x,y) (((x) + (y) - 1) / (y))

/* Non-zero means divides or modulus operations are relatively cheap for
   powers of two, so don't use branches; emit the operation instead. 
   Usually, this will mean that the MD file will emit non-branch
   sequences.  */

static int sdiv_pow2_cheap, smod_pow2_cheap;

#ifndef SLOW_UNALIGNED_ACCESS
#define SLOW_UNALIGNED_ACCESS STRICT_ALIGNMENT
#endif

/* For compilers that support multiple targets with different word sizes,
   MAX_BITS_PER_WORD contains the biggest value of BITS_PER_WORD.  An example
   is the H8/300(H) compiler.  */

#ifndef MAX_BITS_PER_WORD
#define MAX_BITS_PER_WORD BITS_PER_WORD
#endif

/* Cost of various pieces of RTL.  Note that some of these are indexed by shift count,
   and some by mode.  */
static int add_cost, negate_cost, zero_cost;
static int shift_cost[MAX_BITS_PER_WORD];
static int shiftadd_cost[MAX_BITS_PER_WORD];
static int shiftsub_cost[MAX_BITS_PER_WORD];
static int mul_cost[NUM_MACHINE_MODES];
static int div_cost[NUM_MACHINE_MODES];
static int mul_widen_cost[NUM_MACHINE_MODES];
static int mul_highpart_cost[NUM_MACHINE_MODES];

void
init_expmed ()
{
  char *free_point;
  /* This is "some random pseudo register" for purposes of calling recog
     to see what insns exist.  */
  rtx reg = gen_rtx_REG (word_mode, 10000);
  rtx shift_insn, shiftadd_insn, shiftsub_insn;
  int dummy;
  int m;
  enum machine_mode mode, wider_mode;

  start_sequence ();

  /* Since we are on the permanent obstack, we must be sure we save this
     spot AFTER we call start_sequence, since it will reuse the rtl it
     makes.  */
  free_point = (char *) oballoc (0);

  reg = gen_rtx (REG, word_mode, 10000);

  zero_cost = rtx_cost (const0_rtx, 0);
  add_cost = rtx_cost (gen_rtx_PLUS (word_mode, reg, reg), SET);

  shift_insn = emit_insn (gen_rtx_SET (VOIDmode, reg,
				       gen_rtx_ASHIFT (word_mode, reg,
						       const0_rtx)));

  shiftadd_insn
    = emit_insn (gen_rtx_SET (VOIDmode, reg,
			      gen_rtx_PLUS (word_mode,
					    gen_rtx_MULT (word_mode,
							  reg, const0_rtx),
					    reg)));

  shiftsub_insn
    = emit_insn (gen_rtx_SET (VOIDmode, reg,
			      gen_rtx_MINUS (word_mode,
					     gen_rtx_MULT (word_mode,
							   reg, const0_rtx),
					     reg)));

  init_recog ();

  shift_cost[0] = 0;
  shiftadd_cost[0] = shiftsub_cost[0] = add_cost;

  for (m = 1; m < MAX_BITS_PER_WORD; m++)
    {
      shift_cost[m] = shiftadd_cost[m] = shiftsub_cost[m] = 32000;

      XEXP (SET_SRC (PATTERN (shift_insn)), 1) = GEN_INT (m);
      if (recog (PATTERN (shift_insn), shift_insn, &dummy) >= 0)
	shift_cost[m] = rtx_cost (SET_SRC (PATTERN (shift_insn)), SET);

      XEXP (XEXP (SET_SRC (PATTERN (shiftadd_insn)), 0), 1)
	= GEN_INT ((HOST_WIDE_INT) 1 << m);
      if (recog (PATTERN (shiftadd_insn), shiftadd_insn, &dummy) >= 0)
	shiftadd_cost[m] = rtx_cost (SET_SRC (PATTERN (shiftadd_insn)), SET);

      XEXP (XEXP (SET_SRC (PATTERN (shiftsub_insn)), 0), 1)
	= GEN_INT ((HOST_WIDE_INT) 1 << m);
      if (recog (PATTERN (shiftsub_insn), shiftsub_insn, &dummy) >= 0)
	shiftsub_cost[m] = rtx_cost (SET_SRC (PATTERN (shiftsub_insn)), SET);
    }

  negate_cost = rtx_cost (gen_rtx_NEG (word_mode, reg), SET);

  sdiv_pow2_cheap
    = (rtx_cost (gen_rtx_DIV (word_mode, reg, GEN_INT (32)), SET)
       <= 2 * add_cost);
  smod_pow2_cheap
    = (rtx_cost (gen_rtx_MOD (word_mode, reg, GEN_INT (32)), SET)
       <= 2 * add_cost);

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_INT);
       mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    {
      reg = gen_rtx_REG (mode, 10000);
      div_cost[(int) mode] = rtx_cost (gen_rtx_UDIV (mode, reg, reg), SET);
      mul_cost[(int) mode] = rtx_cost (gen_rtx_MULT (mode, reg, reg), SET);
      wider_mode = GET_MODE_WIDER_MODE (mode);
      if (wider_mode != VOIDmode)
	{
	  mul_widen_cost[(int) wider_mode]
	    = rtx_cost (gen_rtx_MULT (wider_mode,
				      gen_rtx_ZERO_EXTEND (wider_mode, reg),
				      gen_rtx_ZERO_EXTEND (wider_mode, reg)),
			SET);
	  mul_highpart_cost[(int) mode]
	    = rtx_cost (gen_rtx_TRUNCATE
			(mode,
			 gen_rtx_LSHIFTRT
			 (wider_mode,
			  gen_rtx_MULT (wider_mode,
					gen_rtx_ZERO_EXTEND (wider_mode, reg),
					gen_rtx_ZERO_EXTEND (wider_mode, reg)),
			  GEN_INT (GET_MODE_BITSIZE (mode)))),
			SET);
	}
    }

  /* Free the objects we just allocated.  */
  end_sequence ();
  obfree (free_point);
}

/* Return an rtx representing minus the value of X.
   MODE is the intended mode of the result,
   useful if X is a CONST_INT.  */

rtx
negate_rtx (mode, x)
     enum machine_mode mode;
     rtx x;
{
  rtx result = simplify_unary_operation (NEG, mode, x, mode);

  if (result == 0)
    result = expand_unop (mode, neg_optab, x, NULL_RTX, 0);

  return result;
}

/* Generate code to store value from rtx VALUE
   into a bit-field within structure STR_RTX
   containing BITSIZE bits starting at bit BITNUM.
   FIELDMODE is the machine-mode of the FIELD_DECL node for this field.
   ALIGN is the alignment that STR_RTX is known to have, measured in bytes.
   TOTAL_SIZE is the size of the structure in bytes, or -1 if varying.  */

/* ??? Note that there are two different ideas here for how
   to determine the size to count bits within, for a register.
   One is BITS_PER_WORD, and the other is the size of operand 3
   of the insv pattern.

   If operand 3 of the insv pattern is VOIDmode, then we will use BITS_PER_WORD
   else, we use the mode of operand 3.  */

rtx
store_bit_field (str_rtx, bitsize, bitnum, fieldmode, value, align, total_size)
     rtx str_rtx;
     register int bitsize;
     int bitnum;
     enum machine_mode fieldmode;
     rtx value;
     int align;
     int total_size;
{
  int unit = (GET_CODE (str_rtx) == MEM) ? BITS_PER_UNIT : BITS_PER_WORD;
  register int offset = bitnum / unit;
  register int bitpos = bitnum % unit;
  register rtx op0 = str_rtx;
#ifdef HAVE_insv
  int insv_bitsize;

  if (insn_operand_mode[(int) CODE_FOR_insv][3] == VOIDmode)
    insv_bitsize = GET_MODE_BITSIZE (word_mode);
  else
    insv_bitsize = GET_MODE_BITSIZE (insn_operand_mode[(int) CODE_FOR_insv][3]);
#endif

  if (GET_CODE (str_rtx) == MEM && ! MEM_IN_STRUCT_P (str_rtx))
    abort ();

  /* Discount the part of the structure before the desired byte.
     We need to know how many bytes are safe to reference after it.  */
  if (total_size >= 0)
    total_size -= (bitpos / BIGGEST_ALIGNMENT
		   * (BIGGEST_ALIGNMENT / BITS_PER_UNIT));

  while (GET_CODE (op0) == SUBREG)
    {
      /* The following line once was done only if WORDS_BIG_ENDIAN,
	 but I think that is a mistake.  WORDS_BIG_ENDIAN is
	 meaningful at a much higher level; when structures are copied
	 between memory and regs, the higher-numbered regs
	 always get higher addresses.  */
      offset += SUBREG_WORD (op0);
      /* We used to adjust BITPOS here, but now we do the whole adjustment
	 right after the loop.  */
      op0 = SUBREG_REG (op0);
    }

  /* Make sure we are playing with integral modes.  Pun with subregs
     if we aren't.  */
  {
    enum machine_mode imode = int_mode_for_mode (GET_MODE (op0));
    if (imode != GET_MODE (op0))
      {
	if (GET_CODE (op0) == MEM)
	  op0 = change_address (op0, imode, NULL_RTX);
	else if (imode != BLKmode)
	  op0 = gen_lowpart (imode, op0);
	else
	  abort ();
      }
  }

  /* If OP0 is a register, BITPOS must count within a word.
     But as we have it, it counts within whatever size OP0 now has.
     On a bigendian machine, these are not the same, so convert.  */
  if (BYTES_BIG_ENDIAN
      && GET_CODE (op0) != MEM
      && unit > GET_MODE_BITSIZE (GET_MODE (op0)))
    bitpos += unit - GET_MODE_BITSIZE (GET_MODE (op0));

  value = protect_from_queue (value, 0);

  if (flag_force_mem)
    value = force_not_mem (value);

  /* Note that the adjustment of BITPOS above has no effect on whether
     BITPOS is 0 in a REG bigger than a word.  */
  if (GET_MODE_SIZE (fieldmode) >= UNITS_PER_WORD
      && (GET_CODE (op0) != MEM
	  || ! SLOW_UNALIGNED_ACCESS
	  || (offset * BITS_PER_UNIT % bitsize == 0
	      && align % GET_MODE_SIZE (fieldmode) == 0))
      && bitpos == 0 && bitsize == GET_MODE_BITSIZE (fieldmode))
    {
      /* Storing in a full-word or multi-word field in a register
	 can be done with just SUBREG.  */
      if (GET_MODE (op0) != fieldmode)
	{
	  if (GET_CODE (op0) == SUBREG)
	    {
	      if (GET_MODE (SUBREG_REG (op0)) == fieldmode
		  || GET_MODE_CLASS (fieldmode) == MODE_INT
		  || GET_MODE_CLASS (fieldmode) == MODE_PARTIAL_INT)
		op0 = SUBREG_REG (op0);
	      else
		/* Else we've got some float mode source being extracted into
		   a different float mode destination -- this combination of
		   subregs results in Severe Tire Damage.  */
		abort ();
	    }
	  if (GET_CODE (op0) == REG)
	    op0 = gen_rtx_SUBREG (fieldmode, op0, offset);
	  else
	    op0 = change_address (op0, fieldmode,
				  plus_constant (XEXP (op0, 0), offset));
	}
      emit_move_insn (op0, value);
      return value;
    }

  /* Storing an lsb-aligned field in a register
     can be done with a movestrict instruction.  */

  if (GET_CODE (op0) != MEM
      && (BYTES_BIG_ENDIAN ? bitpos + bitsize == unit : bitpos == 0)
      && bitsize == GET_MODE_BITSIZE (fieldmode)
      && (GET_MODE (op0) == fieldmode
	  || (movstrict_optab->handlers[(int) fieldmode].insn_code
	      != CODE_FOR_nothing)))
    {
      /* Get appropriate low part of the value being stored.  */
      if (GET_CODE (value) == CONST_INT || GET_CODE (value) == REG)
	value = gen_lowpart (fieldmode, value);
      else if (!(GET_CODE (value) == SYMBOL_REF
		 || GET_CODE (value) == LABEL_REF
		 || GET_CODE (value) == CONST))
	value = convert_to_mode (fieldmode, value, 0);

      if (GET_MODE (op0) == fieldmode)
	emit_move_insn (op0, value);
      else
	{
	  int icode = movstrict_optab->handlers[(int) fieldmode].insn_code;
	  if (! (*insn_operand_predicate[icode][1]) (value, fieldmode))
	    value = copy_to_mode_reg (fieldmode, value);

	  if (GET_CODE (op0) == SUBREG)
	    {
	      if (GET_MODE (SUBREG_REG (op0)) == fieldmode
		  || GET_MODE_CLASS (fieldmode) == MODE_INT
		  || GET_MODE_CLASS (fieldmode) == MODE_PARTIAL_INT)
		op0 = SUBREG_REG (op0);
	      else
		/* Else we've got some float mode source being extracted into
		   a different float mode destination -- this combination of
		   subregs results in Severe Tire Damage.  */
		abort ();
	    }

	  emit_insn (GEN_FCN (icode)
		   (gen_rtx_SUBREG (fieldmode, op0, offset), value));
	}
      return value;
    }

  /* Handle fields bigger than a word.  */

  if (bitsize > BITS_PER_WORD)
    {
      /* Here we transfer the words of the field
	 in the order least significant first.
	 This is because the most significant word is the one which may
	 be less than full.
	 However, only do that if the value is not BLKmode.  */

      int backwards = WORDS_BIG_ENDIAN && fieldmode != BLKmode;

      int nwords = (bitsize + (BITS_PER_WORD - 1)) / BITS_PER_WORD;
      int i;

      /* This is the mode we must force value to, so that there will be enough
	 subwords to extract.  Note that fieldmode will often (always?) be
	 VOIDmode, because that is what store_field uses to indicate that this
	 is a bit field, but passing VOIDmode to operand_subword_force will
	 result in an abort.  */
      fieldmode = mode_for_size (nwords * BITS_PER_WORD, MODE_INT, 0);

      for (i = 0; i < nwords; i++)
	{
	  /* If I is 0, use the low-order word in both field and target;
	     if I is 1, use the next to lowest word; and so on.  */
	  int wordnum = (backwards ? nwords - i - 1 : i);
	  int bit_offset = (backwards
			    ? MAX (bitsize - (i + 1) * BITS_PER_WORD, 0)
			    : i * BITS_PER_WORD);
	  store_bit_field (op0, MIN (BITS_PER_WORD,
				     bitsize - i * BITS_PER_WORD),
			   bitnum + bit_offset, word_mode,
			   operand_subword_force (value, wordnum,
						  (GET_MODE (value) == VOIDmode
						   ? fieldmode
						   : GET_MODE (value))),
			   align, total_size);
	}
      return value;
    }

  /* From here on we can assume that the field to be stored in is
     a full-word (whatever type that is), since it is shorter than a word.  */

  /* OFFSET is the number of words or bytes (UNIT says which)
     from STR_RTX to the first word or byte containing part of the field.  */

  if (GET_CODE (op0) != MEM)
    {
      if (offset != 0
	  || GET_MODE_SIZE (GET_MODE (op0)) > UNITS_PER_WORD)
	{
	  if (GET_CODE (op0) != REG)
	    {
	      /* Since this is a destination (lvalue), we can't copy it to a
		 pseudo.  We can trivially remove a SUBREG that does not
		 change the size of the operand.  Such a SUBREG may have been
		 added above.  Otherwise, abort.  */
	      if (GET_CODE (op0) == SUBREG
		  && (GET_MODE_SIZE (GET_MODE (op0))
		      == GET_MODE_SIZE (GET_MODE (SUBREG_REG (op0)))))
		op0 = SUBREG_REG (op0);
	      else
		abort ();
	    }
	  op0 = gen_rtx_SUBREG (mode_for_size (BITS_PER_WORD, MODE_INT, 0),
		                op0, offset);
	}
      offset = 0;
    }
  else
    {
      op0 = protect_from_queue (op0, 1);
    }

  /* If VALUE is a floating-point mode, access it as an integer of the
     corresponding size.  This can occur on a machine with 64 bit registers
     that uses SFmode for float.  This can also occur for unaligned float
     structure fields.  */
  if (GET_MODE_CLASS (GET_MODE (value)) == MODE_FLOAT)
    {
      if (GET_CODE (value) != REG)
	value = copy_to_reg (value);
      value = gen_rtx_SUBREG (word_mode, value, 0);
    }

  /* Now OFFSET is nonzero only if OP0 is memory
     and is therefore always measured in bytes.  */

#ifdef HAVE_insv
  if (HAVE_insv
      && GET_MODE (value) != BLKmode
      && !(bitsize == 1 && GET_CODE (value) == CONST_INT)
      /* Ensure insv's size is wide enough for this field.  */
      && (insv_bitsize >= bitsize)
      && ! ((GET_CODE (op0) == REG || GET_CODE (op0) == SUBREG)
	    && (bitsize + bitpos > insv_bitsize)))
    {
      int xbitpos = bitpos;
      rtx value1;
      rtx xop0 = op0;
      rtx last = get_last_insn ();
      rtx pat;
      enum machine_mode maxmode;
      int save_volatile_ok = volatile_ok;

      maxmode = insn_operand_mode[(int) CODE_FOR_insv][3];
      if (maxmode == VOIDmode)
	maxmode = word_mode;

      volatile_ok = 1;

      /* If this machine's insv can only insert into a register, copy OP0
	 into a register and save it back later.  */
      /* This used to check flag_force_mem, but that was a serious
	 de-optimization now that flag_force_mem is enabled by -O2.  */
      if (GET_CODE (op0) == MEM
	  && ! ((*insn_operand_predicate[(int) CODE_FOR_insv][0])
		(op0, VOIDmode)))
	{
	  rtx tempreg;
	  enum machine_mode bestmode;

	  /* Get the mode to use for inserting into this field.  If OP0 is
	     BLKmode, get the smallest mode consistent with the alignment. If
	     OP0 is a non-BLKmode object that is no wider than MAXMODE, use its
	     mode. Otherwise, use the smallest mode containing the field.  */

	  if (GET_MODE (op0) == BLKmode
	      || GET_MODE_SIZE (GET_MODE (op0)) > GET_MODE_SIZE (maxmode))
	    bestmode
	      = get_best_mode (bitsize, bitnum, align * BITS_PER_UNIT, maxmode,
			       MEM_VOLATILE_P (op0));
	  else
	    bestmode = GET_MODE (op0);

	  if (bestmode == VOIDmode
	      || (SLOW_UNALIGNED_ACCESS && GET_MODE_SIZE (bestmode) > align))
	    goto insv_loses;

	  /* Adjust address to point to the containing unit of that mode.  */
	  unit = GET_MODE_BITSIZE (bestmode);
	  /* Compute offset as multiple of this unit, counting in bytes.  */
	  offset = (bitnum / unit) * GET_MODE_SIZE (bestmode);
	  bitpos = bitnum % unit;
	  op0 = change_address (op0, bestmode, 
				plus_constant (XEXP (op0, 0), offset));

	  /* Fetch that unit, store the bitfield in it, then store the unit.  */
	  tempreg = copy_to_reg (op0);
	  store_bit_field (tempreg, bitsize, bitpos, fieldmode, value,
			   align, total_size);
	  emit_move_insn (op0, tempreg);
	  return value;
	}
      volatile_ok = save_volatile_ok;

      /* Add OFFSET into OP0's address.  */
      if (GET_CODE (xop0) == MEM)
	xop0 = change_address (xop0, byte_mode,
			       plus_constant (XEXP (xop0, 0), offset));

      /* If xop0 is a register, we need it in MAXMODE
	 to make it acceptable to the format of insv.  */
      if (GET_CODE (xop0) == SUBREG)
	/* We can't just change the mode, because this might clobber op0,
	   and we will need the original value of op0 if insv fails.  */
	xop0 = gen_rtx_SUBREG (maxmode, SUBREG_REG (xop0), SUBREG_WORD (xop0));
      if (GET_CODE (xop0) == REG && GET_MODE (xop0) != maxmode)
	xop0 = gen_rtx_SUBREG (maxmode, xop0, 0);

      /* On big-endian machines, we count bits from the most significant.
	 If the bit field insn does not, we must invert.  */

      if (BITS_BIG_ENDIAN != BYTES_BIG_ENDIAN)
	xbitpos = unit - bitsize - xbitpos;

      /* We have been counting XBITPOS within UNIT.
	 Count instead within the size of the register.  */
      if (BITS_BIG_ENDIAN && GET_CODE (xop0) != MEM)
	xbitpos += GET_MODE_BITSIZE (maxmode) - unit;

      unit = GET_MODE_BITSIZE (maxmode);

      /* Convert VALUE to maxmode (which insv insn wants) in VALUE1.  */
      value1 = value;
      if (GET_MODE (value) != maxmode)
	{
	  if (GET_MODE_BITSIZE (GET_MODE (value)) >= bitsize)
	    {
	      /* Optimization: Don't bother really extending VALUE
		 if it has all the bits we will actually use.  However,
		 if we must narrow it, be sure we do it correctly.  */

	      if (GET_MODE_SIZE (GET_MODE (value)) < GET_MODE_SIZE (maxmode))
		{
		  /* Avoid making subreg of a subreg, or of a mem.  */
		  if (GET_CODE (value1) != REG)
		    value1 = copy_to_reg (value1);
		  value1 = gen_rtx_SUBREG (maxmode, value1, 0);
		}
	      else
		value1 = gen_lowpart (maxmode, value1);
	    }
	  else if (!CONSTANT_P (value))
	    /* Parse phase is supposed to make VALUE's data type
	       match that of the component reference, which is a type
	       at least as wide as the field; so VALUE should have
	       a mode that corresponds to that type.  */
	    abort ();
	}

      /* If this machine's insv insists on a register,
	 get VALUE1 into a register.  */
      if (! ((*insn_operand_predicate[(int) CODE_FOR_insv][3])
	     (value1, maxmode)))
	value1 = force_reg (maxmode, value1);

      pat = gen_insv (xop0, GEN_INT (bitsize), GEN_INT (xbitpos), value1);
      if (pat)
	emit_insn (pat);
      else
        {
	  delete_insns_since (last);
	  store_fixed_bit_field (op0, offset, bitsize, bitpos, value, align);
	}
    }
  else
    insv_loses:
#endif
    /* Insv is not available; store using shifts and boolean ops.  */
    store_fixed_bit_field (op0, offset, bitsize, bitpos, value, align);
  return value;
}

/* Use shifts and boolean operations to store VALUE
   into a bit field of width BITSIZE
   in a memory location specified by OP0 except offset by OFFSET bytes.
     (OFFSET must be 0 if OP0 is a register.)
   The field starts at position BITPOS within the byte.
    (If OP0 is a register, it may be a full word or a narrower mode,
     but BITPOS still counts within a full word,
     which is significant on bigendian machines.)
   STRUCT_ALIGN is the alignment the structure is known to have (in bytes).

   Note that protect_from_queue has already been done on OP0 and VALUE.  */

static void
store_fixed_bit_field (op0, offset, bitsize, bitpos, value, struct_align)
     register rtx op0;
     register int offset, bitsize, bitpos;
     register rtx value;
     int struct_align;
{
  register enum machine_mode mode;
  int total_bits = BITS_PER_WORD;
  rtx subtarget, temp;
  int all_zero = 0;
  int all_one = 0;

  if (! SLOW_UNALIGNED_ACCESS)
    struct_align = BIGGEST_ALIGNMENT / BITS_PER_UNIT;
    
  /* There is a case not handled here:
     a structure with a known alignment of just a halfword
     and a field split across two aligned halfwords within the structure.
     Or likewise a structure with a known alignment of just a byte
     and a field split across two bytes.
     Such cases are not supposed to be able to occur.  */

  if (GET_CODE (op0) == REG || GET_CODE (op0) == SUBREG)
    {
      if (offset != 0)
	abort ();
      /* Special treatment for a bit field split across two registers.  */
      if (bitsize + bitpos > BITS_PER_WORD)
	{
	  store_split_bit_field (op0, bitsize, bitpos,
				 value, BITS_PER_WORD);
	  return;
	}
    }
  else
    {
      /* Get the proper mode to use for this field.  We want a mode that
	 includes the entire field.  If such a mode would be larger than
	 a word, we won't be doing the extraction the normal way.  */

      mode = get_best_mode (bitsize, bitpos + offset * BITS_PER_UNIT,
			    struct_align * BITS_PER_UNIT, word_mode,
			    GET_CODE (op0) == MEM && MEM_VOLATILE_P (op0));

      if (mode == VOIDmode)
	{
	  /* The only way this should occur is if the field spans word
	     boundaries.  */
	  store_split_bit_field (op0,
				 bitsize, bitpos + offset * BITS_PER_UNIT,
				 value, struct_align);
	  return;
	}

      total_bits = GET_MODE_BITSIZE (mode);

      /* Make sure bitpos is valid for the chosen mode.  Adjust BITPOS to
	 be in the range 0 to total_bits-1, and put any excess bytes in
	 OFFSET.  */
      if (bitpos >= total_bits)
	{
	  offset += (bitpos / total_bits) * (total_bits / BITS_PER_UNIT);
	  bitpos -= ((bitpos / total_bits) * (total_bits / BITS_PER_UNIT)
		     * BITS_PER_UNIT);
	}

      /* Get ref to an aligned byte, halfword, or word containing the field.
	 Adjust BITPOS to be position within a word,
	 and OFFSET to be the offset of that word.
	 Then alter OP0 to refer to that word.  */
      bitpos += (offset % (total_bits / BITS_PER_UNIT)) * BITS_PER_UNIT;
      offset -= (offset % (total_bits / BITS_PER_UNIT));
      op0 = change_address (op0, mode,
			    plus_constant (XEXP (op0, 0), offset));
    }

  mode = GET_MODE (op0);

  /* Now MODE is either some integral mode for a MEM as OP0,
     or is a full-word for a REG as OP0.  TOTAL_BITS corresponds.
     The bit field is contained entirely within OP0.
     BITPOS is the starting bit number within OP0.
     (OP0's mode may actually be narrower than MODE.)  */

  if (BYTES_BIG_ENDIAN)
      /* BITPOS is the distance between our msb
	 and that of the containing datum.
	 Convert it to the distance from the lsb.  */
      bitpos = total_bits - bitsize - bitpos;

  /* Now BITPOS is always the distance between our lsb
     and that of OP0.  */

  /* Shift VALUE left by BITPOS bits.  If VALUE is not constant,
     we must first convert its mode to MODE.  */

  if (GET_CODE (value) == CONST_INT)
    {
      register HOST_WIDE_INT v = INTVAL (value);

      if (bitsize < HOST_BITS_PER_WIDE_INT)
	v &= ((HOST_WIDE_INT) 1 << bitsize) - 1;

      if (v == 0)
	all_zero = 1;
      else if ((bitsize < HOST_BITS_PER_WIDE_INT
		&& v == ((HOST_WIDE_INT) 1 << bitsize) - 1)
	       || (bitsize == HOST_BITS_PER_WIDE_INT && v == -1))
	all_one = 1;

      value = lshift_value (mode, value, bitpos, bitsize);
    }
  else
    {
      int must_and = (GET_MODE_BITSIZE (GET_MODE (value)) != bitsize
		      && bitpos + bitsize != GET_MODE_BITSIZE (mode));

      if (GET_MODE (value) != mode)
	{
	  if ((GET_CODE (value) == REG || GET_CODE (value) == SUBREG)
	      && GET_MODE_SIZE (mode) < GET_MODE_SIZE (GET_MODE (value)))
	    value = gen_lowpart (mode, value);
	  else
	    value = convert_to_mode (mode, value, 1);
	}

      if (must_and)
	value = expand_binop (mode, and_optab, value,
			      mask_rtx (mode, 0, bitsize, 0),
			      NULL_RTX, 1, OPTAB_LIB_WIDEN);
      if (bitpos > 0)
	value = expand_shift (LSHIFT_EXPR, mode, value,
			      build_int_2 (bitpos, 0), NULL_RTX, 1);
    }

  /* Now clear the chosen bits in OP0,
     except that if VALUE is -1 we need not bother.  */

  subtarget = (GET_CODE (op0) == REG || ! flag_force_mem) ? op0 : 0;

  if (! all_one)
    {
      temp = expand_binop (mode, and_optab, op0,
			   mask_rtx (mode, bitpos, bitsize, 1),
			   subtarget, 1, OPTAB_LIB_WIDEN);
      subtarget = temp;
    }
  else
    temp = op0;

  /* Now logical-or VALUE into OP0, unless it is zero.  */

  if (! all_zero)
    temp = expand_binop (mode, ior_optab, temp, value,
			 subtarget, 1, OPTAB_LIB_WIDEN);
  if (op0 != temp)
    emit_move_insn (op0, temp);
}

/* Store a bit field that is split across multiple accessible memory objects.

   OP0 is the REG, SUBREG or MEM rtx for the first of the objects.
   BITSIZE is the field width; BITPOS the position of its first bit
   (within the word).
   VALUE is the value to store.
   ALIGN is the known alignment of OP0, measured in bytes.
   This is also the size of the memory objects to be used.

   This does not yet handle fields wider than BITS_PER_WORD.  */

static void
store_split_bit_field (op0, bitsize, bitpos, value, align)
     rtx op0;
     int bitsize, bitpos;
     rtx value;
     int align;
{
  int unit;
  int bitsdone = 0;

  /* Make sure UNIT isn't larger than BITS_PER_WORD, we can only handle that
     much at a time.  */
  if (GET_CODE (op0) == REG || GET_CODE (op0) == SUBREG)
    unit = BITS_PER_WORD;
  else
    unit = MIN (align * BITS_PER_UNIT, BITS_PER_WORD);

  /* If VALUE is a constant other than a CONST_INT, get it into a register in
     WORD_MODE.  If we can do this using gen_lowpart_common, do so.  Note
     that VALUE might be a floating-point constant.  */
  if (CONSTANT_P (value) && GET_CODE (value) != CONST_INT)
    {
      rtx word = gen_lowpart_common (word_mode, value);

      if (word && (value != word))
	value = word;
      else
	value = gen_lowpart_common (word_mode,
				    force_reg (GET_MODE (value) != VOIDmode
					       ? GET_MODE (value)
					       : word_mode, value));
    }
  else if (GET_CODE (value) == ADDRESSOF)
    value = copy_to_reg (value);

  while (bitsdone < bitsize)
    {
      int thissize;
      rtx part, word;
      int thispos;
      int offset;

      offset = (bitpos + bitsdone) / unit;
      thispos = (bitpos + bitsdone) % unit;

      /* THISSIZE must not overrun a word boundary.  Otherwise,
	 store_fixed_bit_field will call us again, and we will mutually
	 recurse forever.  */
      thissize = MIN (bitsize - bitsdone, BITS_PER_WORD);
      thissize = MIN (thissize, unit - thispos);

      if (BYTES_BIG_ENDIAN)
	{
	  int total_bits;

	  /* We must do an endian conversion exactly the same way as it is
	     done in extract_bit_field, so that the two calls to
	     extract_fixed_bit_field will have comparable arguments.  */
	  if (GET_CODE (value) != MEM || GET_MODE (value) == BLKmode)
	    total_bits = BITS_PER_WORD;
	  else
	    total_bits = GET_MODE_BITSIZE (GET_MODE (value));

	  /* Fetch successively less significant portions.  */
	  if (GET_CODE (value) == CONST_INT)
	    part = GEN_INT (((unsigned HOST_WIDE_INT) (INTVAL (value))
			     >> (bitsize - bitsdone - thissize))
			    & (((HOST_WIDE_INT) 1 << thissize) - 1));
	  else
	    /* The args are chosen so that the last part includes the
	       lsb.  Give extract_bit_field the value it needs (with
	       endianness compensation) to fetch the piece we want.

	       ??? We have no idea what the alignment of VALUE is, so
	       we have to use a guess.  */
	    part
	      = extract_fixed_bit_field
		(word_mode, value, 0, thissize,
		 total_bits - bitsize + bitsdone, NULL_RTX, 1,
		 GET_MODE (value) == VOIDmode
		 ? UNITS_PER_WORD
		 : (GET_MODE (value) == BLKmode
		    ? 1
		    : GET_MODE_ALIGNMENT (GET_MODE (value)) / BITS_PER_UNIT));
	}
      else
	{
	  /* Fetch successively more significant portions.  */
	  if (GET_CODE (value) == CONST_INT)
	    part = GEN_INT (((unsigned HOST_WIDE_INT) (INTVAL (value))
			     >> bitsdone)
			    & (((HOST_WIDE_INT) 1 << thissize) - 1));
	  else
	    part
	      = extract_fixed_bit_field
		(word_mode, value, 0, thissize, bitsdone, NULL_RTX, 1,
		 GET_MODE (value) == VOIDmode
		 ? UNITS_PER_WORD
		 : (GET_MODE (value) == BLKmode
		    ? 1
		    : GET_MODE_ALIGNMENT (GET_MODE (value)) / BITS_PER_UNIT));
	}

      /* If OP0 is a register, then handle OFFSET here.

	 When handling multiword bitfields, extract_bit_field may pass
	 down a word_mode SUBREG of a larger REG for a bitfield that actually
	 crosses a word boundary.  Thus, for a SUBREG, we must find
	 the current word starting from the base register.  */
      if (GET_CODE (op0) == SUBREG)
	{
	  word = operand_subword_force (SUBREG_REG (op0),
					SUBREG_WORD (op0) + offset,
					GET_MODE (SUBREG_REG (op0)));
	  offset = 0;
	}
      else if (GET_CODE (op0) == REG)
	{
	  word = operand_subword_force (op0, offset, GET_MODE (op0));
	  offset = 0;
	}
      else
	word = op0;

      /* OFFSET is in UNITs, and UNIT is in bits.
         store_fixed_bit_field wants offset in bytes.  */
      store_fixed_bit_field (word, offset * unit / BITS_PER_UNIT,
			     thissize, thispos, part, align);
      bitsdone += thissize;
    }
}

/* Generate code to extract a byte-field from STR_RTX
   containing BITSIZE bits, starting at BITNUM,
   and put it in TARGET if possible (if TARGET is nonzero).
   Regardless of TARGET, we return the rtx for where the value is placed.
   It may be a QUEUED.

   STR_RTX is the structure containing the byte (a REG or MEM).
   UNSIGNEDP is nonzero if this is an unsigned bit field.
   MODE is the natural mode of the field value once extracted.
   TMODE is the mode the caller would like the value to have;
   but the value may be returned with type MODE instead.

   ALIGN is the alignment that STR_RTX is known to have, measured in bytes.
   TOTAL_SIZE is the size in bytes of the containing structure,
   or -1 if varying.

   If a TARGET is specified and we can store in it at no extra cost,
   we do so, and return TARGET.
   Otherwise, we return a REG of mode TMODE or MODE, with TMODE preferred
   if they are equally easy.  */

rtx
extract_bit_field (str_rtx, bitsize, bitnum, unsignedp,
		   target, mode, tmode, align, total_size)
     rtx str_rtx;
     register int bitsize;
     int bitnum;
     int unsignedp;
     rtx target;
     enum machine_mode mode, tmode;
     int align;
     int total_size;
{
  int unit = (GET_CODE (str_rtx) == MEM) ? BITS_PER_UNIT : BITS_PER_WORD;
  register int offset = bitnum / unit;
  register int bitpos = bitnum % unit;
  register rtx op0 = str_rtx;
  rtx spec_target = target;
  rtx spec_target_subreg = 0;
#ifdef HAVE_extv
  int extv_bitsize;
#endif
#ifdef HAVE_extzv
  int extzv_bitsize;
#endif

#ifdef HAVE_extv
  if (insn_operand_mode[(int) CODE_FOR_extv][0] == VOIDmode)
    extv_bitsize = GET_MODE_BITSIZE (word_mode);
  else
    extv_bitsize = GET_MODE_BITSIZE (insn_operand_mode[(int) CODE_FOR_extv][0]);
#endif

#ifdef HAVE_extzv
  if (insn_operand_mode[(int) CODE_FOR_extzv][0] == VOIDmode)
    extzv_bitsize = GET_MODE_BITSIZE (word_mode);
  else
    extzv_bitsize
      = GET_MODE_BITSIZE (insn_operand_mode[(int) CODE_FOR_extzv][0]);
#endif

  /* Discount the part of the structure before the desired byte.
     We need to know how many bytes are safe to reference after it.  */
  if (total_size >= 0)
    total_size -= (bitpos / BIGGEST_ALIGNMENT
		   * (BIGGEST_ALIGNMENT / BITS_PER_UNIT));

  if (tmode == VOIDmode)
    tmode = mode;
  while (GET_CODE (op0) == SUBREG)
    {
      int outer_size = GET_MODE_BITSIZE (GET_MODE (op0));
      int inner_size = GET_MODE_BITSIZE (GET_MODE (SUBREG_REG (op0)));

      offset += SUBREG_WORD (op0);

      inner_size = MIN (inner_size, BITS_PER_WORD);

      if (BYTES_BIG_ENDIAN && (outer_size < inner_size))
	{
	  bitpos += inner_size - outer_size;
	  if (bitpos > unit)
	    {
	      offset += (bitpos / unit);
	      bitpos %= unit;
	    }
	}

      op0 = SUBREG_REG (op0);
    }

  /* Make sure we are playing with integral modes.  Pun with subregs
     if we aren't.  */
  {
    enum machine_mode imode = int_mode_for_mode (GET_MODE (op0));
    if (imode != GET_MODE (op0))
      {
	if (GET_CODE (op0) == MEM)
	  op0 = change_address (op0, imode, NULL_RTX);
	else if (imode != BLKmode)
	  op0 = gen_lowpart (imode, op0);
	else
	  abort ();
      }
  }

  /* ??? We currently assume TARGET is at least as big as BITSIZE.
     If that's wrong, the solution is to test for it and set TARGET to 0
     if needed.  */
  
  /* If OP0 is a register, BITPOS must count within a word.
     But as we have it, it counts within whatever size OP0 now has.
     On a bigendian machine, these are not the same, so convert.  */
  if (BYTES_BIG_ENDIAN
      && GET_CODE (op0) != MEM
      && unit > GET_MODE_BITSIZE (GET_MODE (op0)))
    bitpos += unit - GET_MODE_BITSIZE (GET_MODE (op0));

  /* Extracting a full-word or multi-word value
     from a structure in a register or aligned memory.
     This can be done with just SUBREG.
     So too extracting a subword value in
     the least significant part of the register.  */

  if (((GET_CODE (op0) != MEM
	&& TRULY_NOOP_TRUNCATION (GET_MODE_BITSIZE (mode),
				  GET_MODE_BITSIZE (GET_MODE (op0))))
       || (GET_CODE (op0) == MEM
	   && (! SLOW_UNALIGNED_ACCESS
	       || (offset * BITS_PER_UNIT % bitsize == 0
		   && align * BITS_PER_UNIT % bitsize == 0))))
      && ((bitsize >= BITS_PER_WORD && bitsize == GET_MODE_BITSIZE (mode)
	   && bitpos % BITS_PER_WORD == 0)
	  || (mode_for_size (bitsize, GET_MODE_CLASS (tmode), 0) != BLKmode
	      /* ??? The big endian test here is wrong.  This is correct
		 if the value is in a register, and if mode_for_size is not
		 the same mode as op0.  This causes us to get unnecessarily
		 inefficient code from the Thumb port when -mbig-endian.  */
	      && (BYTES_BIG_ENDIAN
		  ? bitpos + bitsize == BITS_PER_WORD
		  : bitpos == 0))))
    {
      enum machine_mode mode1
	= mode_for_size (bitsize, GET_MODE_CLASS (tmode), 0);

      if (mode1 != GET_MODE (op0))
	{
	  if (GET_CODE (op0) == SUBREG)
	    {
	      if (GET_MODE (SUBREG_REG (op0)) == mode1
		  || GET_MODE_CLASS (mode1) == MODE_INT
		  || GET_MODE_CLASS (mode1) == MODE_PARTIAL_INT)
		op0 = SUBREG_REG (op0);
	      else
		/* Else we've got some float mode source being extracted into
		   a different float mode destination -- this combination of
		   subregs results in Severe Tire Damage.  */
		abort ();
	    }
	  if (GET_CODE (op0) == REG)
	    op0 = gen_rtx_SUBREG (mode1, op0, offset);
	  else
	    op0 = change_address (op0, mode1,
				  plus_constant (XEXP (op0, 0), offset));
	}
      if (mode1 != mode)
	return convert_to_mode (tmode, op0, unsignedp);
      return op0;
    }

  /* Handle fields bigger than a word.  */
  
  if (bitsize > BITS_PER_WORD)
    {
      /* Here we transfer the words of the field
	 in the order least significant first.
	 This is because the most significant word is the one which may
	 be less than full.  */

      int nwords = (bitsize + (BITS_PER_WORD - 1)) / BITS_PER_WORD;
      int i;

      if (target == 0 || GET_CODE (target) != REG)
	target = gen_reg_rtx (mode);

      /* Indicate for flow that the entire target reg is being set.  */
      emit_insn (gen_rtx_CLOBBER (VOIDmode, target));

      for (i = 0; i < nwords; i++)
	{
	  /* If I is 0, use the low-order word in both field and target;
	     if I is 1, use the next to lowest word; and so on.  */
	  /* Word number in TARGET to use.  */
	  int wordnum = (WORDS_BIG_ENDIAN
			 ? GET_MODE_SIZE (GET_MODE (target)) / UNITS_PER_WORD - i - 1
			 : i);
	  /* Offset from start of field in OP0.  */
	  int bit_offset = (WORDS_BIG_ENDIAN
			    ? MAX (0, bitsize - (i + 1) * BITS_PER_WORD)
			    : i * BITS_PER_WORD);
	  rtx target_part = operand_subword (target, wordnum, 1, VOIDmode);
	  rtx result_part
	    = extract_bit_field (op0, MIN (BITS_PER_WORD,
					   bitsize - i * BITS_PER_WORD),
				 bitnum + bit_offset,
				 1, target_part, mode, word_mode,
				 align, total_size);

	  if (target_part == 0)
	    abort ();

	  if (result_part != target_part)
	    emit_move_insn (target_part, result_part);
	}

      if (unsignedp)
	{
	  /* Unless we've filled TARGET, the upper regs in a multi-reg value
	     need to be zero'd out.  */
	  if (GET_MODE_SIZE (GET_MODE (target)) > nwords * UNITS_PER_WORD)
	    {
	      int i,total_words;

	      total_words = GET_MODE_SIZE (GET_MODE (target)) / UNITS_PER_WORD;
	      for (i = nwords; i < total_words; i++)
		{
		  int wordnum = WORDS_BIG_ENDIAN ? total_words - i - 1 : i;
		  rtx target_part = operand_subword (target, wordnum, 1, VOIDmode);
		  emit_move_insn (target_part, const0_rtx);
		}
	    }
	  return target;
	}

      /* Signed bit field: sign-extend with two arithmetic shifts.  */
      target = expand_shift (LSHIFT_EXPR, mode, target,
			     build_int_2 (GET_MODE_BITSIZE (mode) - bitsize, 0),
			     NULL_RTX, 0);
      return expand_shift (RSHIFT_EXPR, mode, target,
			   build_int_2 (GET_MODE_BITSIZE (mode) - bitsize, 0),
			   NULL_RTX, 0);
    }
  
  /* From here on we know the desired field is smaller than a word
     so we can assume it is an integer.  So we can safely extract it as one
     size of integer, if necessary, and then truncate or extend
     to the size that is wanted.  */

  /* OFFSET is the number of words or bytes (UNIT says which)
     from STR_RTX to the first word or byte containing part of the field.  */

  if (GET_CODE (op0) != MEM)
    {
      if (offset != 0
	  || GET_MODE_SIZE (GET_MODE (op0)) > UNITS_PER_WORD)
	{
	  if (GET_CODE (op0) != REG)
	    op0 = copy_to_reg (op0);
	  op0 = gen_rtx_SUBREG (mode_for_size (BITS_PER_WORD, MODE_INT, 0),
		                op0, offset);
	}
      offset = 0;
    }
  else
    {
      op0 = protect_from_queue (str_rtx, 1);
    }

  /* Now OFFSET is nonzero only for memory operands.  */

  if (unsignedp)
    {
#ifdef HAVE_extzv
      if (HAVE_extzv
	  && (extzv_bitsize >= bitsize)
	  && ! ((GET_CODE (op0) == REG || GET_CODE (op0) == SUBREG)
		&& (bitsize + bitpos > extzv_bitsize)))
	{
	  int xbitpos = bitpos, xoffset = offset;
	  rtx bitsize_rtx, bitpos_rtx;
	  rtx last = get_last_insn ();
	  rtx xop0 = op0;
	  rtx xtarget = target;
	  rtx xspec_target = spec_target;
	  rtx xspec_target_subreg = spec_target_subreg;
	  rtx pat;
	  enum machine_mode maxmode;

	  maxmode = insn_operand_mode[(int) CODE_FOR_extzv][0];
	  if (maxmode == VOIDmode)
	    maxmode = word_mode;

	  if (GET_CODE (xop0) == MEM)
	    {
	      int save_volatile_ok = volatile_ok;
	      volatile_ok = 1;

	      /* Is the memory operand acceptable?  */
	      if (! ((*insn_operand_predicate[(int) CODE_FOR_extzv][1])
		     (xop0, GET_MODE (xop0))))
		{
		  /* No, load into a reg and extract from there.  */
		  enum machine_mode bestmode;

		  /* Get the mode to use for inserting into this field.  If
		     OP0 is BLKmode, get the smallest mode consistent with the
		     alignment. If OP0 is a non-BLKmode object that is no
		     wider than MAXMODE, use its mode. Otherwise, use the
		     smallest mode containing the field.  */

		  if (GET_MODE (xop0) == BLKmode
		      || (GET_MODE_SIZE (GET_MODE (op0))
			  > GET_MODE_SIZE (maxmode)))
		    bestmode = get_best_mode (bitsize, bitnum,
					      align * BITS_PER_UNIT, maxmode,
					      MEM_VOLATILE_P (xop0));
		  else
		    bestmode = GET_MODE (xop0);

		  if (bestmode == VOIDmode
		      || (SLOW_UNALIGNED_ACCESS && GET_MODE_SIZE (bestmode) > align))
		    goto extzv_loses;

		  /* Compute offset as multiple of this unit,
		     counting in bytes.  */
		  unit = GET_MODE_BITSIZE (bestmode);
		  xoffset = (bitnum / unit) * GET_MODE_SIZE (bestmode);
		  xbitpos = bitnum % unit;
		  xop0 = change_address (xop0, bestmode,
					 plus_constant (XEXP (xop0, 0),
							xoffset));
		  /* Fetch it to a register in that size.  */
		  xop0 = force_reg (bestmode, xop0);

		  /* XBITPOS counts within UNIT, which is what is expected.  */
		}
	      else
		/* Get ref to first byte containing part of the field.  */
		xop0 = change_address (xop0, byte_mode,
				       plus_constant (XEXP (xop0, 0), xoffset));

	      volatile_ok = save_volatile_ok;
	    }

	  /* If op0 is a register, we need it in MAXMODE (which is usually
	     SImode). to make it acceptable to the format of extzv.  */
	  if (GET_CODE (xop0) == SUBREG && GET_MODE (xop0) != maxmode)
	    goto extzv_loses;
	  if (GET_CODE (xop0) == REG && GET_MODE (xop0) != maxmode)
	    xop0 = gen_rtx_SUBREG (maxmode, xop0, 0);

	  /* On big-endian machines, we count bits from the most significant.
	     If the bit field insn does not, we must invert.  */
	  if (BITS_BIG_ENDIAN != BYTES_BIG_ENDIAN)
	    xbitpos = unit - bitsize - xbitpos;

	  /* Now convert from counting within UNIT to counting in MAXMODE.  */
	  if (BITS_BIG_ENDIAN && GET_CODE (xop0) != MEM)
	    xbitpos += GET_MODE_BITSIZE (maxmode) - unit;

	  unit = GET_MODE_BITSIZE (maxmode);

	  if (xtarget == 0
	      || (flag_force_mem && GET_CODE (xtarget) == MEM))
	    xtarget = xspec_target = gen_reg_rtx (tmode);

	  if (GET_MODE (xtarget) != maxmode)
	    {
	      if (GET_CODE (xtarget) == REG)
		{
		  int wider = (GET_MODE_SIZE (maxmode)
			       > GET_MODE_SIZE (GET_MODE (xtarget)));
		  xtarget = gen_lowpart (maxmode, xtarget);
		  if (wider)
		    xspec_target_subreg = xtarget;
		}
	      else
		xtarget = gen_reg_rtx (maxmode);
	    }

	  /* If this machine's extzv insists on a register target,
	     make sure we have one.  */
	  if (! ((*insn_operand_predicate[(int) CODE_FOR_extzv][0])
		 (xtarget, maxmode)))
	    xtarget = gen_reg_rtx (maxmode);

	  bitsize_rtx = GEN_INT (bitsize);
	  bitpos_rtx = GEN_INT (xbitpos);

	  pat = gen_extzv (protect_from_queue (xtarget, 1),
			   xop0, bitsize_rtx, bitpos_rtx);
	  if (pat)
	    {
	      emit_insn (pat);
	      target = xtarget;
	      spec_target = xspec_target;
	      spec_target_subreg = xspec_target_subreg;
	    }
	  else
	    {
	      delete_insns_since (last);
	      target = extract_fixed_bit_field (tmode, op0, offset, bitsize,
						bitpos, target, 1, align);
	    }
	}
      else
        extzv_loses:
#endif
	target = extract_fixed_bit_field (tmode, op0, offset, bitsize, bitpos,
					  target, 1, align);
    }
  else
    {
#ifdef HAVE_extv
      if (HAVE_extv
	  && (extv_bitsize >= bitsize)
	  && ! ((GET_CODE (op0) == REG || GET_CODE (op0) == SUBREG)
		&& (bitsize + bitpos > extv_bitsize)))
	{
	  int xbitpos = bitpos, xoffset = offset;
	  rtx bitsize_rtx, bitpos_rtx;
	  rtx last = get_last_insn ();
	  rtx xop0 = op0, xtarget = target;
	  rtx xspec_target = spec_target;
	  rtx xspec_target_subreg = spec_target_subreg;
	  rtx pat;
	  enum machine_mode maxmode;

	  maxmode = insn_operand_mode[(int) CODE_FOR_extv][0];
	  if (maxmode == VOIDmode)
	    maxmode = word_mode;

	  if (GET_CODE (xop0) == MEM)
	    {
	      /* Is the memory operand acceptable?  */
	      if (! ((*insn_operand_predicate[(int) CODE_FOR_extv][1])
		     (xop0, GET_MODE (xop0))))
		{
		  /* No, load into a reg and extract from there.  */
		  enum machine_mode bestmode;

		  /* Get the mode to use for inserting into this field.  If
		     OP0 is BLKmode, get the smallest mode consistent with the
		     alignment. If OP0 is a non-BLKmode object that is no
		     wider than MAXMODE, use its mode. Otherwise, use the
		     smallest mode containing the field.  */

		  if (GET_MODE (xop0) == BLKmode
		      || (GET_MODE_SIZE (GET_MODE (op0))
			  > GET_MODE_SIZE (maxmode)))
		    bestmode = get_best_mode (bitsize, bitnum,
					      align * BITS_PER_UNIT, maxmode,
					      MEM_VOLATILE_P (xop0));
		  else
		    bestmode = GET_MODE (xop0);

		  if (bestmode == VOIDmode
		      || (SLOW_UNALIGNED_ACCESS && GET_MODE_SIZE (bestmode) > align))
		    goto extv_loses;

		  /* Compute offset as multiple of this unit,
		     counting in bytes.  */
		  unit = GET_MODE_BITSIZE (bestmode);
		  xoffset = (bitnum / unit) * GET_MODE_SIZE (bestmode);
		  xbitpos = bitnum % unit;
		  xop0 = change_address (xop0, bestmode,
					 plus_constant (XEXP (xop0, 0),
							xoffset));
		  /* Fetch it to a register in that size.  */
		  xop0 = force_reg (bestmode, xop0);

		  /* XBITPOS counts within UNIT, which is what is expected.  */
		}
	      else
		/* Get ref to first byte containing part of the field.  */
		xop0 = change_address (xop0, byte_mode,
				       plus_constant (XEXP (xop0, 0), xoffset));
	    }

	  /* If op0 is a register, we need it in MAXMODE (which is usually
	     SImode) to make it acceptable to the format of extv.  */
	  if (GET_CODE (xop0) == SUBREG && GET_MODE (xop0) != maxmode)
	    goto extv_loses;
	  if (GET_CODE (xop0) == REG && GET_MODE (xop0) != maxmode)
	    xop0 = gen_rtx_SUBREG (maxmode, xop0, 0);

	  /* On big-endian machines, we count bits from the most significant.
	     If the bit field insn does not, we must invert.  */
	  if (BITS_BIG_ENDIAN != BYTES_BIG_ENDIAN)
	    xbitpos = unit - bitsize - xbitpos;

	  /* XBITPOS counts within a size of UNIT.
	     Adjust to count within a size of MAXMODE.  */
	  if (BITS_BIG_ENDIAN && GET_CODE (xop0) != MEM)
	    xbitpos += (GET_MODE_BITSIZE (maxmode) - unit);

	  unit = GET_MODE_BITSIZE (maxmode);

	  if (xtarget == 0
	      || (flag_force_mem && GET_CODE (xtarget) == MEM))
	    xtarget = xspec_target = gen_reg_rtx (tmode);

	  if (GET_MODE (xtarget) != maxmode)
	    {
	      if (GET_CODE (xtarget) == REG)
		{
		  int wider = (GET_MODE_SIZE (maxmode)
			       > GET_MODE_SIZE (GET_MODE (xtarget)));
		  xtarget = gen_lowpart (maxmode, xtarget);
		  if (wider)
		    xspec_target_subreg = xtarget;
		}
	      else
		xtarget = gen_reg_rtx (maxmode);
	    }

	  /* If this machine's extv insists on a register target,
	     make sure we have one.  */
	  if (! ((*insn_operand_predicate[(int) CODE_FOR_extv][0])
		 (xtarget, maxmode)))
	    xtarget = gen_reg_rtx (maxmode);

	  bitsize_rtx = GEN_INT (bitsize);
	  bitpos_rtx = GEN_INT (xbitpos);

	  pat = gen_extv (protect_from_queue (xtarget, 1),
			  xop0, bitsize_rtx, bitpos_rtx);
	  if (pat)
	    {
	      emit_insn (pat);
	      target = xtarget;
	      spec_target = xspec_target;
	      spec_target_subreg = xspec_target_subreg;
	    }
	  else
	    {
	      delete_insns_since (last);
	      target = extract_fixed_bit_field (tmode, op0, offset, bitsize,
						bitpos, target, 0, align);
	    }
	} 
      else
	extv_loses:
#endif
	target = extract_fixed_bit_field (tmode, op0, offset, bitsize, bitpos,
					  target, 0, align);
    }
  if (target == spec_target)
    return target;
  if (target == spec_target_subreg)
    return spec_target;
  if (GET_MODE (target) != tmode && GET_MODE (target) != mode)
    {
      /* If the target mode is floating-point, first convert to the
	 integer mode of that size and then access it as a floating-point
	 value via a SUBREG.  */
      if (GET_MODE_CLASS (tmode) == MODE_FLOAT)
	{
	  target = convert_to_mode (mode_for_size (GET_MODE_BITSIZE (tmode),
						   MODE_INT, 0),
				    target, unsignedp);
	  if (GET_CODE (target) != REG)
	    target = copy_to_reg (target);
	  return gen_rtx_SUBREG (tmode, target, 0);
	}
      else
	return convert_to_mode (tmode, target, unsignedp);
    }
  return target;
}

/* Extract a bit field using shifts and boolean operations
   Returns an rtx to represent the value.
   OP0 addresses a register (word) or memory (byte).
   BITPOS says which bit within the word or byte the bit field starts in.
   OFFSET says how many bytes farther the bit field starts;
    it is 0 if OP0 is a register.
   BITSIZE says how many bits long the bit field is.
    (If OP0 is a register, it may be narrower than a full word,
     but BITPOS still counts within a full word,
     which is significant on bigendian machines.)

   UNSIGNEDP is nonzero for an unsigned bit field (don't sign-extend value).
   If TARGET is nonzero, attempts to store the value there
   and return TARGET, but this is not guaranteed.
   If TARGET is not used, create a pseudo-reg of mode TMODE for the value.

   ALIGN is the alignment that STR_RTX is known to have, measured in bytes.  */

static rtx
extract_fixed_bit_field (tmode, op0, offset, bitsize, bitpos,
			 target, unsignedp, align)
     enum machine_mode tmode;
     register rtx op0, target;
     register int offset, bitsize, bitpos;
     int unsignedp;
     int align;
{
  int total_bits = BITS_PER_WORD;
  enum machine_mode mode;

  if (GET_CODE (op0) == SUBREG || GET_CODE (op0) == REG)
    {
      /* Special treatment for a bit field split across two registers.  */
      if (bitsize + bitpos > BITS_PER_WORD)
	return extract_split_bit_field (op0, bitsize, bitpos,
					unsignedp, align);
    }
  else
    {
      /* Get the proper mode to use for this field.  We want a mode that
	 includes the entire field.  If such a mode would be larger than
	 a word, we won't be doing the extraction the normal way.  */

      mode = get_best_mode (bitsize, bitpos + offset * BITS_PER_UNIT,
			    align * BITS_PER_UNIT, word_mode,
			    GET_CODE (op0) == MEM && MEM_VOLATILE_P (op0));

      if (mode == VOIDmode)
	/* The only way this should occur is if the field spans word
	   boundaries.  */
	return extract_split_bit_field (op0, bitsize,
					bitpos + offset * BITS_PER_UNIT,
					unsignedp, align);

      total_bits = GET_MODE_BITSIZE (mode);

      /* Make sure bitpos is valid for the chosen mode.  Adjust BITPOS to
	 be in the range 0 to total_bits-1, and put any excess bytes in
	 OFFSET.  */
      if (bitpos >= total_bits)
	{
	  offset += (bitpos / total_bits) * (total_bits / BITS_PER_UNIT);
	  bitpos -= ((bitpos / total_bits) * (total_bits / BITS_PER_UNIT)
		     * BITS_PER_UNIT);
	}

      /* Get ref to an aligned byte, halfword, or word containing the field.
	 Adjust BITPOS to be position within a word,
	 and OFFSET to be the offset of that word.
	 Then alter OP0 to refer to that word.  */
      bitpos += (offset % (total_bits / BITS_PER_UNIT)) * BITS_PER_UNIT;
      offset -= (offset % (total_bits / BITS_PER_UNIT));
      op0 = change_address (op0, mode,
			    plus_constant (XEXP (op0, 0), offset));
    }

  mode = GET_MODE (op0);

  if (BYTES_BIG_ENDIAN)
    {
      /* BITPOS is the distance between our msb and that of OP0.
	 Convert it to the distance from the lsb.  */

      bitpos = total_bits - bitsize - bitpos;
    }

  /* Now BITPOS is always the distance between the field's lsb and that of OP0.
     We have reduced the big-endian case to the little-endian case.  */

  if (unsignedp)
    {
      if (bitpos)
	{
	  /* If the field does not already start at the lsb,
	     shift it so it does.  */
	  tree amount = build_int_2 (bitpos, 0);
	  /* Maybe propagate the target for the shift.  */
	  /* But not if we will return it--could confuse integrate.c.  */
	  rtx subtarget = (target != 0 && GET_CODE (target) == REG
			   && !REG_FUNCTION_VALUE_P (target)
			   ? target : 0);
	  if (tmode != mode) subtarget = 0;
	  op0 = expand_shift (RSHIFT_EXPR, mode, op0, amount, subtarget, 1);
	}
      /* Convert the value to the desired mode.  */
      if (mode != tmode)
	op0 = convert_to_mode (tmode, op0, 1);

      /* Unless the msb of the field used to be the msb when we shifted,
	 mask out the upper bits.  */

      if (GET_MODE_BITSIZE (mode) != bitpos + bitsize
#if 0
#ifdef SLOW_ZERO_EXTEND
	  /* Always generate an `and' if
	     we just zero-extended op0 and SLOW_ZERO_EXTEND, since it
	     will combine fruitfully with the zero-extend.  */
	  || tmode != mode
#endif
#endif
	  )
	return expand_binop (GET_MODE (op0), and_optab, op0,
			     mask_rtx (GET_MODE (op0), 0, bitsize, 0),
			     target, 1, OPTAB_LIB_WIDEN);
      return op0;
    }

  /* To extract a signed bit-field, first shift its msb to the msb of the word,
     then arithmetic-shift its lsb to the lsb of the word.  */
  op0 = force_reg (mode, op0);
  if (mode != tmode)
    target = 0;

  /* Find the narrowest integer mode that contains the field.  */

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_INT); mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    if (GET_MODE_BITSIZE (mode) >= bitsize + bitpos)
      {
	op0 = convert_to_mode (mode, op0, 0);
	break;
      }

  if (GET_MODE_BITSIZE (mode) != (bitsize + bitpos))
    {
      tree amount = build_int_2 (GET_MODE_BITSIZE (mode) - (bitsize + bitpos), 0);
      /* Maybe propagate the target for the shift.  */
      /* But not if we will return the result--could confuse integrate.c.  */
      rtx subtarget = (target != 0 && GET_CODE (target) == REG
		       && ! REG_FUNCTION_VALUE_P (target)
		       ? target : 0);
      op0 = expand_shift (LSHIFT_EXPR, mode, op0, amount, subtarget, 1);
    }

  return expand_shift (RSHIFT_EXPR, mode, op0,
		       build_int_2 (GET_MODE_BITSIZE (mode) - bitsize, 0), 
		       target, 0);
}

/* Return a constant integer (CONST_INT or CONST_DOUBLE) mask value
   of mode MODE with BITSIZE ones followed by BITPOS zeros, or the
   complement of that if COMPLEMENT.  The mask is truncated if
   necessary to the width of mode MODE.  The mask is zero-extended if
   BITSIZE+BITPOS is too small for MODE.  */

static rtx
mask_rtx (mode, bitpos, bitsize, complement)
     enum machine_mode mode;
     int bitpos, bitsize, complement;
{
  HOST_WIDE_INT masklow, maskhigh;

  if (bitpos < HOST_BITS_PER_WIDE_INT)
    masklow = (HOST_WIDE_INT) -1 << bitpos;
  else
    masklow = 0;

  if (bitpos + bitsize < HOST_BITS_PER_WIDE_INT)
    masklow &= ((unsigned HOST_WIDE_INT) -1
		>> (HOST_BITS_PER_WIDE_INT - bitpos - bitsize));
  
  if (bitpos <= HOST_BITS_PER_WIDE_INT)
    maskhigh = -1;
  else
    maskhigh = (HOST_WIDE_INT) -1 << (bitpos - HOST_BITS_PER_WIDE_INT);

  if (bitpos + bitsize > HOST_BITS_PER_WIDE_INT)
    maskhigh &= ((unsigned HOST_WIDE_INT) -1
		 >> (2 * HOST_BITS_PER_WIDE_INT - bitpos - bitsize));
  else
    maskhigh = 0;

  if (complement)
    {
      maskhigh = ~maskhigh;
      masklow = ~masklow;
    }

  return immed_double_const (masklow, maskhigh, mode);
}

/* Return a constant integer (CONST_INT or CONST_DOUBLE) rtx with the value
   VALUE truncated to BITSIZE bits and then shifted left BITPOS bits.  */

static rtx
lshift_value (mode, value, bitpos, bitsize)
     enum machine_mode mode;
     rtx value;
     int bitpos, bitsize;
{
  unsigned HOST_WIDE_INT v = INTVAL (value);
  HOST_WIDE_INT low, high;

  if (bitsize < HOST_BITS_PER_WIDE_INT)
    v &= ~((HOST_WIDE_INT) -1 << bitsize);

  if (bitpos < HOST_BITS_PER_WIDE_INT)
    {
      low = v << bitpos;
      high = (bitpos > 0 ? (v >> (HOST_BITS_PER_WIDE_INT - bitpos)) : 0);
    }
  else
    {
      low = 0;
      high = v << (bitpos - HOST_BITS_PER_WIDE_INT);
    }

  return immed_double_const (low, high, mode);
}

/* Extract a bit field that is split across two words
   and return an RTX for the result.

   OP0 is the REG, SUBREG or MEM rtx for the first of the two words.
   BITSIZE is the field width; BITPOS, position of its first bit, in the word.
   UNSIGNEDP is 1 if should zero-extend the contents; else sign-extend.

   ALIGN is the known alignment of OP0, measured in bytes.
   This is also the size of the memory objects to be used.  */

static rtx
extract_split_bit_field (op0, bitsize, bitpos, unsignedp, align)
     rtx op0;
     int bitsize, bitpos, unsignedp, align;
{
  int unit;
  int bitsdone = 0;
  rtx result = NULL_RTX;
  int first = 1;

  /* Make sure UNIT isn't larger than BITS_PER_WORD, we can only handle that
     much at a time.  */
  if (GET_CODE (op0) == REG || GET_CODE (op0) == SUBREG)
    unit = BITS_PER_WORD;
  else
    unit = MIN (align * BITS_PER_UNIT, BITS_PER_WORD);

  while (bitsdone < bitsize)
    {
      int thissize;
      rtx part, word;
      int thispos;
      int offset;

      offset = (bitpos + bitsdone) / unit;
      thispos = (bitpos + bitsdone) % unit;

      /* THISSIZE must not overrun a word boundary.  Otherwise,
	 extract_fixed_bit_field will call us again, and we will mutually
	 recurse forever.  */
      thissize = MIN (bitsize - bitsdone, BITS_PER_WORD);
      thissize = MIN (thissize, unit - thispos);

      /* If OP0 is a register, then handle OFFSET here.

	 When handling multiword bitfields, extract_bit_field may pass
	 down a word_mode SUBREG of a larger REG for a bitfield that actually
	 crosses a word boundary.  Thus, for a SUBREG, we must find
	 the current word starting from the base register.  */
      if (GET_CODE (op0) == SUBREG)
	{
	  word = operand_subword_force (SUBREG_REG (op0),
					SUBREG_WORD (op0) + offset,
					GET_MODE (SUBREG_REG (op0)));
	  offset = 0;
	}
      else if (GET_CODE (op0) == REG)
	{
	  word = operand_subword_force (op0, offset, GET_MODE (op0));
	  offset = 0;
	}
      else
	word = op0;

      /* Extract the parts in bit-counting order,
	 whose meaning is determined by BYTES_PER_UNIT.
	 OFFSET is in UNITs, and UNIT is in bits.
	 extract_fixed_bit_field wants offset in bytes.  */
      part = extract_fixed_bit_field (word_mode, word,
				      offset * unit / BITS_PER_UNIT,
				      thissize, thispos, 0, 1, align);
      bitsdone += thissize;

      /* Shift this part into place for the result.  */
      if (BYTES_BIG_ENDIAN)
	{
	  if (bitsize != bitsdone)
	    part = expand_shift (LSHIFT_EXPR, word_mode, part,
				 build_int_2 (bitsize - bitsdone, 0), 0, 1);
	}
      else
	{
	  if (bitsdone != thissize)
	    part = expand_shift (LSHIFT_EXPR, word_mode, part,
				 build_int_2 (bitsdone - thissize, 0), 0, 1);
	}

      if (first)
	result = part;
      else
	/* Combine the parts with bitwise or.  This works
	   because we extracted each part as an unsigned bit field.  */
	result = expand_binop (word_mode, ior_optab, part, result, NULL_RTX, 1,
			       OPTAB_LIB_WIDEN);

      first = 0;
    }

  /* Unsigned bit field: we are done.  */
  if (unsignedp)
    return result;
  /* Signed bit field: sign-extend with two arithmetic shifts.  */
  result = expand_shift (LSHIFT_EXPR, word_mode, result,
			 build_int_2 (BITS_PER_WORD - bitsize, 0),
			 NULL_RTX, 0);
  return expand_shift (RSHIFT_EXPR, word_mode, result,
		       build_int_2 (BITS_PER_WORD - bitsize, 0), NULL_RTX, 0);
}

/* Add INC into TARGET.  */

void
expand_inc (target, inc)
     rtx target, inc;
{
  rtx value = expand_binop (GET_MODE (target), add_optab,
			    target, inc,
			    target, 0, OPTAB_LIB_WIDEN);
  if (value != target)
    emit_move_insn (target, value);
}

/* Subtract DEC from TARGET.  */

void
expand_dec (target, dec)
     rtx target, dec;
{
  rtx value = expand_binop (GET_MODE (target), sub_optab,
			    target, dec,
			    target, 0, OPTAB_LIB_WIDEN);
  if (value != target)
    emit_move_insn (target, value);
}

/* Output a shift instruction for expression code CODE,
   with SHIFTED being the rtx for the value to shift,
   and AMOUNT the tree for the amount to shift by.
   Store the result in the rtx TARGET, if that is convenient.
   If UNSIGNEDP is nonzero, do a logical shift; otherwise, arithmetic.
   Return the rtx for where the value is.  */

rtx
expand_shift (code, mode, shifted, amount, target, unsignedp)
     enum tree_code code;
     register enum machine_mode mode;
     rtx shifted;
     tree amount;
     register rtx target;
     int unsignedp;
{
  register rtx op1, temp = 0;
  register int left = (code == LSHIFT_EXPR || code == LROTATE_EXPR);
  register int rotate = (code == LROTATE_EXPR || code == RROTATE_EXPR);
  int try;

  /* Previously detected shift-counts computed by NEGATE_EXPR
     and shifted in the other direction; but that does not work
     on all machines.  */

  op1 = expand_expr (amount, NULL_RTX, VOIDmode, 0);

#ifdef SHIFT_COUNT_TRUNCATED
  if (SHIFT_COUNT_TRUNCATED)
    {
      if (GET_CODE (op1) == CONST_INT
          && ((unsigned HOST_WIDE_INT) INTVAL (op1) >=
	      (unsigned HOST_WIDE_INT) GET_MODE_BITSIZE (mode)))
        op1 = GEN_INT ((unsigned HOST_WIDE_INT) INTVAL (op1)
		       % GET_MODE_BITSIZE (mode));
      else if (GET_CODE (op1) == SUBREG
	       && SUBREG_WORD (op1) == 0)
	op1 = SUBREG_REG (op1);
    }
#endif

  if (op1 == const0_rtx)
    return shifted;

  for (try = 0; temp == 0 && try < 3; try++)
    {
      enum optab_methods methods;

      if (try == 0)
	methods = OPTAB_DIRECT;
      else if (try == 1)
	methods = OPTAB_WIDEN;
      else
	methods = OPTAB_LIB_WIDEN;

      if (rotate)
	{
	  /* Widening does not work for rotation.  */
	  if (methods == OPTAB_WIDEN)
	    continue;
	  else if (methods == OPTAB_LIB_WIDEN)
	    {
	      /* If we have been unable to open-code this by a rotation,
		 do it as the IOR of two shifts.  I.e., to rotate A
		 by N bits, compute (A << N) | ((unsigned) A >> (C - N))
		 where C is the bitsize of A.

		 It is theoretically possible that the target machine might
		 not be able to perform either shift and hence we would
		 be making two libcalls rather than just the one for the
		 shift (similarly if IOR could not be done).  We will allow
		 this extremely unlikely lossage to avoid complicating the
		 code below.  */

	      rtx subtarget = target == shifted ? 0 : target;
	      rtx temp1;
	      tree type = TREE_TYPE (amount);
	      tree new_amount = make_tree (type, op1);
	      tree other_amount
		= fold (build (MINUS_EXPR, type,
			       convert (type,
					build_int_2 (GET_MODE_BITSIZE (mode),
						     0)),
			       amount));

	      shifted = force_reg (mode, shifted);

	      temp = expand_shift (left ? LSHIFT_EXPR : RSHIFT_EXPR,
				   mode, shifted, new_amount, subtarget, 1);
	      temp1 = expand_shift (left ? RSHIFT_EXPR : LSHIFT_EXPR,
				    mode, shifted, other_amount, 0, 1);
	      return expand_binop (mode, ior_optab, temp, temp1, target,
				   unsignedp, methods);
	    }

	  temp = expand_binop (mode,
			       left ? rotl_optab : rotr_optab,
			       shifted, op1, target, unsignedp, methods);

	  /* If we don't have the rotate, but we are rotating by a constant
	     that is in range, try a rotate in the opposite direction.  */

	  if (temp == 0 && GET_CODE (op1) == CONST_INT
	      && INTVAL (op1) > 0 && INTVAL (op1) < GET_MODE_BITSIZE (mode))
	    temp = expand_binop (mode,
				 left ? rotr_optab : rotl_optab,
				 shifted, 
				 GEN_INT (GET_MODE_BITSIZE (mode)
					  - INTVAL (op1)),
				 target, unsignedp, methods);
	}
      else if (unsignedp)
	temp = expand_binop (mode,
			     left ? ashl_optab : lshr_optab,
			     shifted, op1, target, unsignedp, methods);

      /* Do arithmetic shifts.
	 Also, if we are going to widen the operand, we can just as well
	 use an arithmetic right-shift instead of a logical one.  */
      if (temp == 0 && ! rotate
	  && (! unsignedp || (! left && methods == OPTAB_WIDEN)))
	{
	  enum optab_methods methods1 = methods;

	  /* If trying to widen a log shift to an arithmetic shift,
	     don't accept an arithmetic shift of the same size.  */
	  if (unsignedp)
	    methods1 = OPTAB_MUST_WIDEN;

	  /* Arithmetic shift */

	  temp = expand_binop (mode,
			       left ? ashl_optab : ashr_optab,
			       shifted, op1, target, unsignedp, methods1);
	}

      /* We used to try extzv here for logical right shifts, but that was
	 only useful for one machine, the VAX, and caused poor code 
	 generation there for lshrdi3, so the code was deleted and a
	 define_expand for lshrsi3 was added to vax.md.  */
    }

  if (temp == 0)
    abort ();
  return temp;
}

enum alg_code { alg_zero, alg_m, alg_shift,
		  alg_add_t_m2, alg_sub_t_m2,
		  alg_add_factor, alg_sub_factor,
		  alg_add_t2_m, alg_sub_t2_m,
		  alg_add, alg_subtract, alg_factor, alg_shiftop };

/* This structure records a sequence of operations.
   `ops' is the number of operations recorded.
   `cost' is their total cost.
   The operations are stored in `op' and the corresponding
   logarithms of the integer coefficients in `log'.

   These are the operations:
   alg_zero		total := 0;
   alg_m		total := multiplicand;
   alg_shift		total := total * coeff
   alg_add_t_m2		total := total + multiplicand * coeff;
   alg_sub_t_m2		total := total - multiplicand * coeff;
   alg_add_factor	total := total * coeff + total;
   alg_sub_factor	total := total * coeff - total;
   alg_add_t2_m		total := total * coeff + multiplicand;
   alg_sub_t2_m		total := total * coeff - multiplicand;

   The first operand must be either alg_zero or alg_m.  */

struct algorithm
{
  short cost;
  short ops;
  /* The size of the OP and LOG fields are not directly related to the
     word size, but the worst-case algorithms will be if we have few
     consecutive ones or zeros, i.e., a multiplicand like 10101010101...
     In that case we will generate shift-by-2, add, shift-by-2, add,...,
     in total wordsize operations.  */
  enum alg_code op[MAX_BITS_PER_WORD];
  char log[MAX_BITS_PER_WORD];
};

static void synth_mult			PROTO((struct algorithm *,
					       unsigned HOST_WIDE_INT,
					       int));
static unsigned HOST_WIDE_INT choose_multiplier PROTO((unsigned HOST_WIDE_INT,
						       int, int,
						       unsigned HOST_WIDE_INT *,
						       int *, int *));
static unsigned HOST_WIDE_INT invert_mod2n	PROTO((unsigned HOST_WIDE_INT,
						       int));
/* Compute and return the best algorithm for multiplying by T.
   The algorithm must cost less than cost_limit
   If retval.cost >= COST_LIMIT, no algorithm was found and all
   other field of the returned struct are undefined.  */

static void
synth_mult (alg_out, t, cost_limit)
     struct algorithm *alg_out;
     unsigned HOST_WIDE_INT t;
     int cost_limit;
{
  int m;
  struct algorithm *alg_in, *best_alg;
  int cost;
  unsigned HOST_WIDE_INT q;

  /* Indicate that no algorithm is yet found.  If no algorithm
     is found, this value will be returned and indicate failure.  */
  alg_out->cost = cost_limit;

  if (cost_limit <= 0)
    return;

  /* t == 1 can be done in zero cost.  */
  if (t == 1)
    {
      alg_out->ops = 1;
      alg_out->cost = 0;
      alg_out->op[0] = alg_m;
      return;
    }

  /* t == 0 sometimes has a cost.  If it does and it exceeds our limit,
     fail now.  */
  if (t == 0)
    {
      if (zero_cost >= cost_limit)
	return;
      else
	{
	  alg_out->ops = 1;
	  alg_out->cost = zero_cost;
	  alg_out->op[0] = alg_zero;
	  return;
	}
    }

  /* We'll be needing a couple extra algorithm structures now.  */

  alg_in = (struct algorithm *)alloca (sizeof (struct algorithm));
  best_alg = (struct algorithm *)alloca (sizeof (struct algorithm));

  /* If we have a group of zero bits at the low-order part of T, try
     multiplying by the remaining bits and then doing a shift.  */

  if ((t & 1) == 0)
    {
      m = floor_log2 (t & -t);	/* m = number of low zero bits */
      q = t >> m;
      cost = shift_cost[m];
      synth_mult (alg_in, q, cost_limit - cost);

      cost += alg_in->cost;
      if (cost < cost_limit)
	{
	  struct algorithm *x;
	  x = alg_in, alg_in = best_alg, best_alg = x;
	  best_alg->log[best_alg->ops] = m;
	  best_alg->op[best_alg->ops] = alg_shift;
	  cost_limit = cost;
	}
    }

  /* If we have an odd number, add or subtract one.  */
  if ((t & 1) != 0)
    {
      unsigned HOST_WIDE_INT w;

      for (w = 1; (w & t) != 0; w <<= 1)
	;
      /* If T was -1, then W will be zero after the loop.  This is another
	 case where T ends with ...111.  Handling this with (T + 1) and 
	 subtract 1 produces slightly better code and results in algorithm
	 selection much faster than treating it like the ...0111 case
	 below.  */
      if (w == 0
	  || (w > 2
	      /* Reject the case where t is 3.
		 Thus we prefer addition in that case.  */
	      && t != 3))
	{
	  /* T ends with ...111.  Multiply by (T + 1) and subtract 1.  */

	  cost = add_cost;
	  synth_mult (alg_in, t + 1, cost_limit - cost);

	  cost += alg_in->cost;
	  if (cost < cost_limit)
	    {
	      struct algorithm *x;
	      x = alg_in, alg_in = best_alg, best_alg = x;
	      best_alg->log[best_alg->ops] = 0;
	      best_alg->op[best_alg->ops] = alg_sub_t_m2;
	      cost_limit = cost;
	    }
	}
      else
	{
	  /* T ends with ...01 or ...011.  Multiply by (T - 1) and add 1.  */

	  cost = add_cost;
	  synth_mult (alg_in, t - 1, cost_limit - cost);

	  cost += alg_in->cost;
	  if (cost < cost_limit)
	    {
	      struct algorithm *x;
	      x = alg_in, alg_in = best_alg, best_alg = x;
	      best_alg->log[best_alg->ops] = 0;
	      best_alg->op[best_alg->ops] = alg_add_t_m2;
	      cost_limit = cost;
	    }
	}
    }

  /* Look for factors of t of the form
     t = q(2**m +- 1), 2 <= m <= floor(log2(t - 1)).
     If we find such a factor, we can multiply by t using an algorithm that
     multiplies by q, shift the result by m and add/subtract it to itself.

     We search for large factors first and loop down, even if large factors
     are less probable than small; if we find a large factor we will find a
     good sequence quickly, and therefore be able to prune (by decreasing
     COST_LIMIT) the search.  */

  for (m = floor_log2 (t - 1); m >= 2; m--)
    {
      unsigned HOST_WIDE_INT d;

      d = ((unsigned HOST_WIDE_INT) 1 << m) + 1;
      if (t % d == 0 && t > d)
	{
	  cost = MIN (shiftadd_cost[m], add_cost + shift_cost[m]);
	  synth_mult (alg_in, t / d, cost_limit - cost);

	  cost += alg_in->cost;
	  if (cost < cost_limit)
	    {
	      struct algorithm *x;
	      x = alg_in, alg_in = best_alg, best_alg = x;
	      best_alg->log[best_alg->ops] = m;
	      best_alg->op[best_alg->ops] = alg_add_factor;
	      cost_limit = cost;
	    }
	  /* Other factors will have been taken care of in the recursion.  */
	  break;
	}

      d = ((unsigned HOST_WIDE_INT) 1 << m) - 1;
      if (t % d == 0 && t > d)
	{
	  cost = MIN (shiftsub_cost[m], add_cost + shift_cost[m]);
	  synth_mult (alg_in, t / d, cost_limit - cost);

	  cost += alg_in->cost;
	  if (cost < cost_limit)
	    {
	      struct algorithm *x;
	      x = alg_in, alg_in = best_alg, best_alg = x;
	      best_alg->log[best_alg->ops] = m;
	      best_alg->op[best_alg->ops] = alg_sub_factor;
	      cost_limit = cost;
	    }
	  break;
	}
    }

  /* Try shift-and-add (load effective address) instructions,
     i.e. do a*3, a*5, a*9.  */
  if ((t & 1) != 0)
    {
      q = t - 1;
      q = q & -q;
      m = exact_log2 (q);
      if (m >= 0)
	{
	  cost = shiftadd_cost[m];
	  synth_mult (alg_in, (t - 1) >> m, cost_limit - cost);

	  cost += alg_in->cost;
	  if (cost < cost_limit)
	    {
	      struct algorithm *x;
	      x = alg_in, alg_in = best_alg, best_alg = x;
	      best_alg->log[best_alg->ops] = m;
	      best_alg->op[best_alg->ops] = alg_add_t2_m;
	      cost_limit = cost;
	    }
	}

      q = t + 1;
      q = q & -q;
      m = exact_log2 (q);
      if (m >= 0)
	{
	  cost = shiftsub_cost[m];
	  synth_mult (alg_in, (t + 1) >> m, cost_limit - cost);

	  cost += alg_in->cost;
	  if (cost < cost_limit)
	    {
	      struct algorithm *x;
	      x = alg_in, alg_in = best_alg, best_alg = x;
	      best_alg->log[best_alg->ops] = m;
	      best_alg->op[best_alg->ops] = alg_sub_t2_m;
	      cost_limit = cost;
	    }
	}
    }

  /* If cost_limit has not decreased since we stored it in alg_out->cost,
     we have not found any algorithm.  */
  if (cost_limit == alg_out->cost)
    return;

  /* If we are getting a too long sequence for `struct algorithm'
     to record, make this search fail.  */
  if (best_alg->ops == MAX_BITS_PER_WORD)
    return;

  /* Copy the algorithm from temporary space to the space at alg_out.
     We avoid using structure assignment because the majority of
     best_alg is normally undefined, and this is a critical function.  */
  alg_out->ops = best_alg->ops + 1;
  alg_out->cost = cost_limit;
  bcopy ((char *) best_alg->op, (char *) alg_out->op,
	 alg_out->ops * sizeof *alg_out->op);
  bcopy ((char *) best_alg->log, (char *) alg_out->log,
	 alg_out->ops * sizeof *alg_out->log);
}

/* Perform a multiplication and return an rtx for the result.
   MODE is mode of value; OP0 and OP1 are what to multiply (rtx's);
   TARGET is a suggestion for where to store the result (an rtx).

   We check specially for a constant integer as OP1.
   If you want this check for OP0 as well, then before calling
   you should swap the two operands if OP0 would be constant.  */

rtx
expand_mult (mode, op0, op1, target, unsignedp)
     enum machine_mode mode;
     register rtx op0, op1, target;
     int unsignedp;
{
  rtx const_op1 = op1;

  /* synth_mult does an `unsigned int' multiply.  As long as the mode is
     less than or equal in size to `unsigned int' this doesn't matter.
     If the mode is larger than `unsigned int', then synth_mult works only
     if the constant value exactly fits in an `unsigned int' without any
     truncation.  This means that multiplying by negative values does
     not work; results are off by 2^32 on a 32 bit machine.  */

  /* If we are multiplying in DImode, it may still be a win
     to try to work with shifts and adds.  */
  if (GET_CODE (op1) == CONST_DOUBLE
      && GET_MODE_CLASS (GET_MODE (op1)) == MODE_INT
      && HOST_BITS_PER_INT >= BITS_PER_WORD
      && CONST_DOUBLE_HIGH (op1) == 0)
    const_op1 = GEN_INT (CONST_DOUBLE_LOW (op1));
  else if (HOST_BITS_PER_INT < GET_MODE_BITSIZE (mode)
	   && GET_CODE (op1) == CONST_INT
	   && INTVAL (op1) < 0)
    const_op1 = 0;

  /* We used to test optimize here, on the grounds that it's better to
     produce a smaller program when -O is not used.
     But this causes such a terrible slowdown sometimes
     that it seems better to use synth_mult always.  */

  if (const_op1 && GET_CODE (const_op1) == CONST_INT)
    {
      struct algorithm alg;
      struct algorithm alg2;
      HOST_WIDE_INT val = INTVAL (op1);
      HOST_WIDE_INT val_so_far;
      rtx insn;
      int mult_cost;
      enum {basic_variant, negate_variant, add_variant} variant = basic_variant;

      /* Try to do the computation three ways: multiply by the negative of OP1
	 and then negate, do the multiplication directly, or do multiplication
	 by OP1 - 1.  */

      mult_cost = rtx_cost (gen_rtx_MULT (mode, op0, op1), SET);
      mult_cost = MIN (12 * add_cost, mult_cost);

      synth_mult (&alg, val, mult_cost);

      /* This works only if the inverted value actually fits in an
	 `unsigned int' */
      if (HOST_BITS_PER_INT >= GET_MODE_BITSIZE (mode))
	{
	  synth_mult (&alg2, - val,
		      (alg.cost < mult_cost ? alg.cost : mult_cost) - negate_cost);
	  if (alg2.cost + negate_cost < alg.cost)
	    alg = alg2, variant = negate_variant;
	}

      /* This proves very useful for division-by-constant.  */
      synth_mult (&alg2, val - 1,
		  (alg.cost < mult_cost ? alg.cost : mult_cost) - add_cost);
      if (alg2.cost + add_cost < alg.cost)
	alg = alg2, variant = add_variant;

      if (alg.cost < mult_cost)
	{
	  /* We found something cheaper than a multiply insn.  */
	  int opno;
	  rtx accum, tem;

	  op0 = protect_from_queue (op0, 0);

	  /* Avoid referencing memory over and over.
	     For speed, but also for correctness when mem is volatile.  */
	  if (GET_CODE (op0) == MEM)
	    op0 = force_reg (mode, op0);

	  /* ACCUM starts out either as OP0 or as a zero, depending on
	     the first operation.  */

	  if (alg.op[0] == alg_zero)
	    {
	      accum = copy_to_mode_reg (mode, const0_rtx);
	      val_so_far = 0;
	    }
	  else if (alg.op[0] == alg_m)
	    {
	      accum = copy_to_mode_reg (mode, op0);
	      val_so_far = 1;
	    }
	  else
	    abort ();

	  for (opno = 1; opno < alg.ops; opno++)
	    {
	      int log = alg.log[opno];
	      int preserve = preserve_subexpressions_p ();
	      rtx shift_subtarget = preserve ? 0 : accum;
	      rtx add_target
		= (opno == alg.ops - 1 && target != 0 && variant != add_variant
		   && ! preserve)
		  ? target : 0;
	      rtx accum_target = preserve ? 0 : accum;
	      
	      switch (alg.op[opno])
		{
		case alg_shift:
		  accum = expand_shift (LSHIFT_EXPR, mode, accum,
					build_int_2 (log, 0), NULL_RTX, 0);
		  val_so_far <<= log;
		  break;

		case alg_add_t_m2:
		  tem = expand_shift (LSHIFT_EXPR, mode, op0,
				      build_int_2 (log, 0), NULL_RTX, 0);
		  accum = force_operand (gen_rtx_PLUS (mode, accum, tem),
					 add_target ? add_target : accum_target);
		  val_so_far += (HOST_WIDE_INT) 1 << log;
		  break;

		case alg_sub_t_m2:
		  tem = expand_shift (LSHIFT_EXPR, mode, op0,
				      build_int_2 (log, 0), NULL_RTX, 0);
		  accum = force_operand (gen_rtx_MINUS (mode, accum, tem),
					 add_target ? add_target : accum_target);
		  val_so_far -= (HOST_WIDE_INT) 1 << log;
		  break;

		case alg_add_t2_m:
		  accum = expand_shift (LSHIFT_EXPR, mode, accum,
					build_int_2 (log, 0), shift_subtarget,
					0);
		  accum = force_operand (gen_rtx_PLUS (mode, accum, op0),
					 add_target ? add_target : accum_target);
		  val_so_far = (val_so_far << log) + 1;
		  break;

		case alg_sub_t2_m:
		  accum = expand_shift (LSHIFT_EXPR, mode, accum,
					build_int_2 (log, 0), shift_subtarget,
					0);
		  accum = force_operand (gen_rtx_MINUS (mode, accum, op0),
					 add_target ? add_target : accum_target);
		  val_so_far = (val_so_far << log) - 1;
		  break;

		case alg_add_factor:
		  tem = expand_shift (LSHIFT_EXPR, mode, accum,
				      build_int_2 (log, 0), NULL_RTX, 0);
		  accum = force_operand (gen_rtx_PLUS (mode, accum, tem),
					 add_target ? add_target : accum_target);
		  val_so_far += val_so_far << log;
		  break;

		case alg_sub_factor:
		  tem = expand_shift (LSHIFT_EXPR, mode, accum,
				      build_int_2 (log, 0), NULL_RTX, 0);
		  accum = force_operand (gen_rtx_MINUS (mode, tem, accum),
					 (add_target ? add_target
					  : preserve ? 0 : tem));
		  val_so_far = (val_so_far << log) - val_so_far;
		  break;

		default:
		  abort ();;
		}

	      /* Write a REG_EQUAL note on the last insn so that we can cse
		 multiplication sequences.  */

	      insn = get_last_insn ();
	      set_unique_reg_note (insn, 
	      			   REG_EQUAL,
				   gen_rtx_MULT (mode, op0, 
				   	         GEN_INT (val_so_far)));
	    }

	  if (variant == negate_variant)
	    {
	      val_so_far = - val_so_far;
	      accum = expand_unop (mode, neg_optab, accum, target, 0);
	    }
	  else if (variant == add_variant)
	    {
	      val_so_far = val_so_far + 1;
	      accum = force_operand (gen_rtx_PLUS (mode, accum, op0), target);
	    }

	  if (val != val_so_far)
	    abort ();

	  return accum;
	}
    }

  /* This used to use umul_optab if unsigned, but for non-widening multiply
     there is no difference between signed and unsigned.  */
  op0 = expand_binop (mode, smul_optab,
		      op0, op1, target, unsignedp, OPTAB_LIB_WIDEN);
  if (op0 == 0)
    abort ();
  return op0;
}

/* Return the smallest n such that 2**n >= X.  */

int
ceil_log2 (x)
     unsigned HOST_WIDE_INT x;
{
  return floor_log2 (x - 1) + 1;
}

/* Choose a minimal N + 1 bit approximation to 1/D that can be used to
   replace division by D, and put the least significant N bits of the result
   in *MULTIPLIER_PTR and return the most significant bit.

   The width of operations is N (should be <= HOST_BITS_PER_WIDE_INT), the
   needed precision is in PRECISION (should be <= N).

   PRECISION should be as small as possible so this function can choose
   multiplier more freely.

   The rounded-up logarithm of D is placed in *lgup_ptr.  A shift count that
   is to be used for a final right shift is placed in *POST_SHIFT_PTR.

   Using this function, x/D will be equal to (x * m) >> (*POST_SHIFT_PTR),
   where m is the full HOST_BITS_PER_WIDE_INT + 1 bit multiplier.  */

static
unsigned HOST_WIDE_INT
choose_multiplier (d, n, precision, multiplier_ptr, post_shift_ptr, lgup_ptr)
     unsigned HOST_WIDE_INT d;
     int n;
     int precision;
     unsigned HOST_WIDE_INT *multiplier_ptr;
     int *post_shift_ptr;
     int *lgup_ptr;
{
  unsigned HOST_WIDE_INT mhigh_hi, mhigh_lo;
  unsigned HOST_WIDE_INT mlow_hi, mlow_lo;
  int lgup, post_shift;
  int pow, pow2;
  unsigned HOST_WIDE_INT nh, nl, dummy1, dummy2;

  /* lgup = ceil(log2(divisor)); */
  lgup = ceil_log2 (d);

  if (lgup > n)
    abort ();

  pow = n + lgup;
  pow2 = n + lgup - precision;

  if (pow == 2 * HOST_BITS_PER_WIDE_INT)
    {
      /* We could handle this with some effort, but this case is much better
	 handled directly with a scc insn, so rely on caller using that.  */
      abort ();
    }

  /* mlow = 2^(N + lgup)/d */
 if (pow >= HOST_BITS_PER_WIDE_INT)
    {
      nh = (unsigned HOST_WIDE_INT) 1 << (pow - HOST_BITS_PER_WIDE_INT);
      nl = 0;
    }
  else
    {
      nh = 0;
      nl = (unsigned HOST_WIDE_INT) 1 << pow;
    }
  div_and_round_double (TRUNC_DIV_EXPR, 1, nl, nh, d, (HOST_WIDE_INT) 0,
			&mlow_lo, &mlow_hi, &dummy1, &dummy2);

  /* mhigh = (2^(N + lgup) + 2^N + lgup - precision)/d */
  if (pow2 >= HOST_BITS_PER_WIDE_INT)
    nh |= (unsigned HOST_WIDE_INT) 1 << (pow2 - HOST_BITS_PER_WIDE_INT);
  else
    nl |= (unsigned HOST_WIDE_INT) 1 << pow2;
  div_and_round_double (TRUNC_DIV_EXPR, 1, nl, nh, d, (HOST_WIDE_INT) 0,
			&mhigh_lo, &mhigh_hi, &dummy1, &dummy2);

  if (mhigh_hi && nh - d >= d)
    abort ();
  if (mhigh_hi > 1 || mlow_hi > 1)
    abort ();
  /* assert that mlow < mhigh.  */
  if (! (mlow_hi < mhigh_hi || (mlow_hi == mhigh_hi && mlow_lo < mhigh_lo)))
    abort();

  /* If precision == N, then mlow, mhigh exceed 2^N
     (but they do not exceed 2^(N+1)).  */

  /* Reduce to lowest terms */
  for (post_shift = lgup; post_shift > 0; post_shift--)
    {
      unsigned HOST_WIDE_INT ml_lo = (mlow_hi << (HOST_BITS_PER_WIDE_INT - 1)) | (mlow_lo >> 1);
      unsigned HOST_WIDE_INT mh_lo = (mhigh_hi << (HOST_BITS_PER_WIDE_INT - 1)) | (mhigh_lo >> 1);
      if (ml_lo >= mh_lo)
	break;

      mlow_hi = 0;
      mlow_lo = ml_lo;
      mhigh_hi = 0;
      mhigh_lo = mh_lo;
    }

  *post_shift_ptr = post_shift;
  *lgup_ptr = lgup;
  if (n < HOST_BITS_PER_WIDE_INT)
    {
      unsigned HOST_WIDE_INT mask = ((unsigned HOST_WIDE_INT) 1 << n) - 1;
      *multiplier_ptr = mhigh_lo & mask;
      return mhigh_lo >= mask;
    }
  else
    {
      *multiplier_ptr = mhigh_lo;
      return mhigh_hi;
    }
}

/* Compute the inverse of X mod 2**n, i.e., find Y such that X * Y is
   congruent to 1 (mod 2**N).  */

static unsigned HOST_WIDE_INT
invert_mod2n (x, n)
     unsigned HOST_WIDE_INT x;
     int n;
{
  /* Solve x*y == 1 (mod 2^n), where x is odd.  Return y.  */

  /* The algorithm notes that the choice y = x satisfies
     x*y == 1 mod 2^3, since x is assumed odd.
     Each iteration doubles the number of bits of significance in y.  */

  unsigned HOST_WIDE_INT mask;
  unsigned HOST_WIDE_INT y = x;
  int nbit = 3;

  mask = (n == HOST_BITS_PER_WIDE_INT
	  ? ~(unsigned HOST_WIDE_INT) 0
	  : ((unsigned HOST_WIDE_INT) 1 << n) - 1);

  while (nbit < n)
    {
      y = y * (2 - x*y) & mask;		/* Modulo 2^N */
      nbit *= 2;
    }
  return y;
}

/* Emit code to adjust ADJ_OPERAND after multiplication of wrong signedness
   flavor of OP0 and OP1.  ADJ_OPERAND is already the high half of the
   product OP0 x OP1.  If UNSIGNEDP is nonzero, adjust the signed product
   to become unsigned, if UNSIGNEDP is zero, adjust the unsigned product to
   become signed.

   The result is put in TARGET if that is convenient.

   MODE is the mode of operation.  */

rtx
expand_mult_highpart_adjust (mode, adj_operand, op0, op1, target, unsignedp)
     enum machine_mode mode;
     register rtx adj_operand, op0, op1, target;
     int unsignedp;
{
  rtx tem;
  enum rtx_code adj_code = unsignedp ? PLUS : MINUS;

  tem = expand_shift (RSHIFT_EXPR, mode, op0,
		      build_int_2 (GET_MODE_BITSIZE (mode) - 1, 0),
		      NULL_RTX, 0);
  tem = expand_and (tem, op1, NULL_RTX);
  adj_operand
    = force_operand (gen_rtx_fmt_ee (adj_code, mode, adj_operand, tem),
		     adj_operand);

  tem = expand_shift (RSHIFT_EXPR, mode, op1,
		      build_int_2 (GET_MODE_BITSIZE (mode) - 1, 0),
		      NULL_RTX, 0);
  tem = expand_and (tem, op0, NULL_RTX);
  target = force_operand (gen_rtx_fmt_ee (adj_code, mode, adj_operand, tem),
			  target);

  return target;
}

/* Emit code to multiply OP0 and CNST1, putting the high half of the result
   in TARGET if that is convenient, and return where the result is.  If the
   operation can not be performed, 0 is returned.

   MODE is the mode of operation and result.

   UNSIGNEDP nonzero means unsigned multiply.

   MAX_COST is the total allowed cost for the expanded RTL.  */

rtx
expand_mult_highpart (mode, op0, cnst1, target, unsignedp, max_cost)
     enum machine_mode mode;
     register rtx op0, target;
     unsigned HOST_WIDE_INT cnst1;
     int unsignedp;
     int max_cost;
{
  enum machine_mode wider_mode = GET_MODE_WIDER_MODE (mode);
  optab mul_highpart_optab;
  optab moptab;
  rtx tem;
  int size = GET_MODE_BITSIZE (mode);
  rtx op1, wide_op1;

  /* We can't support modes wider than HOST_BITS_PER_INT.  */
  if (size > HOST_BITS_PER_WIDE_INT)
    abort ();

  op1 = GEN_INT (cnst1);

  if (GET_MODE_BITSIZE (wider_mode) <= HOST_BITS_PER_INT)
    wide_op1 = op1;
  else
    wide_op1
      = immed_double_const (cnst1,
			    (unsignedp
			     ? (HOST_WIDE_INT) 0
			     : -(cnst1 >> (HOST_BITS_PER_WIDE_INT - 1))),
			    wider_mode);

  /* expand_mult handles constant multiplication of word_mode
     or narrower.  It does a poor job for large modes.  */
  if (size < BITS_PER_WORD
      && mul_cost[(int) wider_mode] + shift_cost[size-1] < max_cost)
    {
      /* We have to do this, since expand_binop doesn't do conversion for
	 multiply.  Maybe change expand_binop to handle widening multiply?  */
      op0 = convert_to_mode (wider_mode, op0, unsignedp);

      tem = expand_mult (wider_mode, op0, wide_op1, NULL_RTX, unsignedp);
      tem = expand_shift (RSHIFT_EXPR, wider_mode, tem,
			  build_int_2 (size, 0), NULL_RTX, 1);
      return convert_modes (mode, wider_mode, tem, unsignedp);
    }

  if (target == 0)
    target = gen_reg_rtx (mode);

  /* Firstly, try using a multiplication insn that only generates the needed
     high part of the product, and in the sign flavor of unsignedp.  */
  if (mul_highpart_cost[(int) mode] < max_cost)
    {
      mul_highpart_optab = unsignedp ? umul_highpart_optab : smul_highpart_optab;
      target = expand_binop (mode, mul_highpart_optab,
			     op0, wide_op1, target, unsignedp, OPTAB_DIRECT);
      if (target)
	return target;
    }

  /* Secondly, same as above, but use sign flavor opposite of unsignedp.
     Need to adjust the result after the multiplication.  */
  if (mul_highpart_cost[(int) mode] + 2 * shift_cost[size-1] + 4 * add_cost < max_cost)
    {
      mul_highpart_optab = unsignedp ? smul_highpart_optab : umul_highpart_optab;
      target = expand_binop (mode, mul_highpart_optab,
			     op0, wide_op1, target, unsignedp, OPTAB_DIRECT);
      if (target)
	/* We used the wrong signedness.  Adjust the result.  */
	return expand_mult_highpart_adjust (mode, target, op0,
					    op1, target, unsignedp);
    }

  /* Try widening multiplication.  */
  moptab = unsignedp ? umul_widen_optab : smul_widen_optab;
  if (moptab->handlers[(int) wider_mode].insn_code != CODE_FOR_nothing
      && mul_widen_cost[(int) wider_mode] < max_cost)
    {
      op1 = force_reg (mode, op1);
      goto try;
    } 

  /* Try widening the mode and perform a non-widening multiplication.  */
  moptab = smul_optab;
  if (smul_optab->handlers[(int) wider_mode].insn_code != CODE_FOR_nothing
      && mul_cost[(int) wider_mode] + shift_cost[size-1] < max_cost)
    {
      op1 = wide_op1;
      goto try;
    }

  /* Try widening multiplication of opposite signedness, and adjust.  */
  moptab = unsignedp ? smul_widen_optab : umul_widen_optab;
  if (moptab->handlers[(int) wider_mode].insn_code != CODE_FOR_nothing
      && (mul_widen_cost[(int) wider_mode]
	  + 2 * shift_cost[size-1] + 4 * add_cost < max_cost))
    {
      rtx regop1 = force_reg (mode, op1);
      tem = expand_binop (wider_mode, moptab, op0, regop1,
			  NULL_RTX, ! unsignedp, OPTAB_WIDEN);
      if (tem != 0)
	{
	  /* Extract the high half of the just generated product.  */
	  tem = expand_shift (RSHIFT_EXPR, wider_mode, tem,
			      build_int_2 (size, 0), NULL_RTX, 1);
	  tem = convert_modes (mode, wider_mode, tem, unsignedp);
	  /* We used the wrong signedness.  Adjust the result.  */
	  return expand_mult_highpart_adjust (mode, tem, op0, op1,
					      target, unsignedp);
	}
    }

  return 0;

 try:
  /* Pass NULL_RTX as target since TARGET has wrong mode.  */
  tem = expand_binop (wider_mode, moptab, op0, op1,
		      NULL_RTX, unsignedp, OPTAB_WIDEN);
  if (tem == 0)
    return 0;

  /* Extract the high half of the just generated product.  */
  if (mode == word_mode)
    {
      return gen_highpart (mode, tem);
    }
  else
    {
      tem = expand_shift (RSHIFT_EXPR, wider_mode, tem,
			  build_int_2 (size, 0), NULL_RTX, 1);
      return convert_modes (mode, wider_mode, tem, unsignedp);
    }
}

/* Emit the code to divide OP0 by OP1, putting the result in TARGET
   if that is convenient, and returning where the result is.
   You may request either the quotient or the remainder as the result;
   specify REM_FLAG nonzero to get the remainder.

   CODE is the expression code for which kind of division this is;
   it controls how rounding is done.  MODE is the machine mode to use.
   UNSIGNEDP nonzero means do unsigned division.  */

/* ??? For CEIL_MOD_EXPR, can compute incorrect remainder with ANDI
   and then correct it by or'ing in missing high bits
   if result of ANDI is nonzero.
   For ROUND_MOD_EXPR, can use ANDI and then sign-extend the result.
   This could optimize to a bfexts instruction.
   But C doesn't use these operations, so their optimizations are
   left for later.  */
/* ??? For modulo, we don't actually need the highpart of the first product,
   the low part will do nicely.  And for small divisors, the second multiply
   can also be a low-part only multiply or even be completely left out.
   E.g. to calculate the remainder of a division by 3 with a 32 bit
   multiply, multiply with 0x55555556 and extract the upper two bits;
   the result is exact for inputs up to 0x1fffffff.
   The input range can be reduced by using cross-sum rules.
   For odd divisors >= 3, the following table gives right shift counts
   so that if an number is shifted by an integer multiple of the given
   amount, the remainder stays the same:
   2, 4, 3, 6, 10, 12, 4, 8, 18, 6, 11, 20, 18, 0, 5, 10, 12, 0, 12, 20,
   14, 12, 23, 21, 8, 0, 20, 18, 0, 0, 6, 12, 0, 22, 0, 18, 20, 30, 0, 0,
   0, 8, 0, 11, 12, 10, 36, 0, 30, 0, 0, 12, 0, 0, 0, 0, 44, 12, 24, 0,
   20, 0, 7, 14, 0, 18, 36, 0, 0, 46, 60, 0, 42, 0, 15, 24, 20, 0, 0, 33,
   0, 20, 0, 0, 18, 0, 60, 0, 0, 0, 0, 0, 40, 18, 0, 0, 12

   Cross-sum rules for even numbers can be derived by leaving as many bits
   to the right alone as the divisor has zeros to the right.
   E.g. if x is an unsigned 32 bit number:
   (x mod 12) == (((x & 1023) + ((x >> 8) & ~3)) * 0x15555558 >> 2 * 3) >> 28
   */

#define EXACT_POWER_OF_2_OR_ZERO_P(x) (((x) & ((x) - 1)) == 0)

rtx
expand_divmod (rem_flag, code, mode, op0, op1, target, unsignedp)
     int rem_flag;
     enum tree_code code;
     enum machine_mode mode;
     register rtx op0, op1, target;
     int unsignedp;
{
  enum machine_mode compute_mode;
  register rtx tquotient;
  rtx quotient = 0, remainder = 0;
  rtx last;
  int size;
  rtx insn, set;
  optab optab1, optab2;
  int op1_is_constant, op1_is_pow2;
  int max_cost, extra_cost;
  static HOST_WIDE_INT last_div_const = 0;

  op1_is_constant = GET_CODE (op1) == CONST_INT;
  op1_is_pow2 = (op1_is_constant
		 && ((EXACT_POWER_OF_2_OR_ZERO_P (INTVAL (op1))
		      || (! unsignedp && EXACT_POWER_OF_2_OR_ZERO_P (-INTVAL (op1))))));

  /*
     This is the structure of expand_divmod:

     First comes code to fix up the operands so we can perform the operations
     correctly and efficiently.

     Second comes a switch statement with code specific for each rounding mode.
     For some special operands this code emits all RTL for the desired
     operation, for other cases, it generates only a quotient and stores it in
     QUOTIENT.  The case for trunc division/remainder might leave quotient = 0,
     to indicate that it has not done anything.

     Last comes code that finishes the operation.  If QUOTIENT is set and
     REM_FLAG is set, the remainder is computed as OP0 - QUOTIENT * OP1.  If
     QUOTIENT is not set, it is computed using trunc rounding.

     We try to generate special code for division and remainder when OP1 is a
     constant.  If |OP1| = 2**n we can use shifts and some other fast
     operations.  For other values of OP1, we compute a carefully selected
     fixed-point approximation m = 1/OP1, and generate code that multiplies OP0
     by m.

     In all cases but EXACT_DIV_EXPR, this multiplication requires the upper
     half of the product.  Different strategies for generating the product are
     implemented in expand_mult_highpart.

     If what we actually want is the remainder, we generate that by another
     by-constant multiplication and a subtraction.  */

  /* We shouldn't be called with OP1 == const1_rtx, but some of the
     code below will malfunction if we are, so check here and handle
     the special case if so.  */
  if (op1 == const1_rtx)
    return rem_flag ? const0_rtx : op0;

  if (target
      /* Don't use the function value register as a target
	 since we have to read it as well as write it,
	 and function-inlining gets confused by this.  */
      && ((REG_P (target) && REG_FUNCTION_VALUE_P (target))
	  /* Don't clobber an operand while doing a multi-step calculation.  */
	  || ((rem_flag || op1_is_constant)
	      && (reg_mentioned_p (target, op0)
		  || (GET_CODE (op0) == MEM && GET_CODE (target) == MEM)))
	  || reg_mentioned_p (target, op1)
	  || (GET_CODE (op1) == MEM && GET_CODE (target) == MEM)))
    target = 0;

  /* Get the mode in which to perform this computation.  Normally it will
     be MODE, but sometimes we can't do the desired operation in MODE.
     If so, pick a wider mode in which we can do the operation.  Convert
     to that mode at the start to avoid repeated conversions.

     First see what operations we need.  These depend on the expression
     we are evaluating.  (We assume that divxx3 insns exist under the
     same conditions that modxx3 insns and that these insns don't normally
     fail.  If these assumptions are not correct, we may generate less
     efficient code in some cases.)

     Then see if we find a mode in which we can open-code that operation
     (either a division, modulus, or shift).  Finally, check for the smallest
     mode for which we can do the operation with a library call.  */

  /* We might want to refine this now that we have division-by-constant
     optimization.  Since expand_mult_highpart tries so many variants, it is
     not straightforward to generalize this.  Maybe we should make an array
     of possible modes in init_expmed?  Save this for GCC 2.7.  */

  optab1 = (op1_is_pow2 ? (unsignedp ? lshr_optab : ashr_optab)
	    : (unsignedp ? udiv_optab : sdiv_optab));
  optab2 = (op1_is_pow2 ? optab1 : (unsignedp ? udivmod_optab : sdivmod_optab));

  for (compute_mode = mode; compute_mode != VOIDmode;
       compute_mode = GET_MODE_WIDER_MODE (compute_mode))
    if (optab1->handlers[(int) compute_mode].insn_code != CODE_FOR_nothing
	|| optab2->handlers[(int) compute_mode].insn_code != CODE_FOR_nothing)
      break;

  if (compute_mode == VOIDmode)
    for (compute_mode = mode; compute_mode != VOIDmode;
	 compute_mode = GET_MODE_WIDER_MODE (compute_mode))
      if (optab1->handlers[(int) compute_mode].libfunc
	  || optab2->handlers[(int) compute_mode].libfunc)
	break;

  /* If we still couldn't find a mode, use MODE, but we'll probably abort
     in expand_binop.  */
  if (compute_mode == VOIDmode)
    compute_mode = mode;

  if (target && GET_MODE (target) == compute_mode)
    tquotient = target;
  else
    tquotient = gen_reg_rtx (compute_mode);

  size = GET_MODE_BITSIZE (compute_mode);
#if 0
  /* It should be possible to restrict the precision to GET_MODE_BITSIZE
     (mode), and thereby get better code when OP1 is a constant.  Do that
     later.  It will require going over all usages of SIZE below.  */
  size = GET_MODE_BITSIZE (mode);
#endif

  /* Only deduct something for a REM if the last divide done was
     for a different constant.   Then set the constant of the last
     divide.  */
  max_cost = div_cost[(int) compute_mode]
    - (rem_flag && ! (last_div_const != 0 && op1_is_constant
		      && INTVAL (op1) == last_div_const)
       ? mul_cost[(int) compute_mode] + add_cost : 0);

  last_div_const = ! rem_flag && op1_is_constant ? INTVAL (op1) : 0;

  /* Now convert to the best mode to use.  */
  if (compute_mode != mode)
    {
      op0 = convert_modes (compute_mode, mode, op0, unsignedp);
      op1 = convert_modes (compute_mode, mode, op1, unsignedp);

      /* convert_modes may have placed op1 into a register, so we
	 must recompute the following.  */
      op1_is_constant = GET_CODE (op1) == CONST_INT;
      op1_is_pow2 = (op1_is_constant
		     && ((EXACT_POWER_OF_2_OR_ZERO_P (INTVAL (op1))
			  || (! unsignedp
			      && EXACT_POWER_OF_2_OR_ZERO_P (-INTVAL (op1)))))) ;
    }

  /* If one of the operands is a volatile MEM, copy it into a register.  */

  if (GET_CODE (op0) == MEM && MEM_VOLATILE_P (op0))
    op0 = force_reg (compute_mode, op0);
  if (GET_CODE (op1) == MEM && MEM_VOLATILE_P (op1))
    op1 = force_reg (compute_mode, op1);

  /* If we need the remainder or if OP1 is constant, we need to
     put OP0 in a register in case it has any queued subexpressions.  */
  if (rem_flag || op1_is_constant)
    op0 = force_reg (compute_mode, op0);

  last = get_last_insn ();

  /* Promote floor rounding to trunc rounding for unsigned operations.  */
  if (unsignedp)
    {
      if (code == FLOOR_DIV_EXPR)
	code = TRUNC_DIV_EXPR;
      if (code == FLOOR_MOD_EXPR)
	code = TRUNC_MOD_EXPR;
      if (code == EXACT_DIV_EXPR && op1_is_pow2)
	code = TRUNC_DIV_EXPR;
    }

  if (op1 != const0_rtx)
    switch (code)
      {
      case TRUNC_MOD_EXPR:
      case TRUNC_DIV_EXPR:
	if (op1_is_constant)
	  {
	    if (unsignedp)
	      {
		unsigned HOST_WIDE_INT mh, ml;
		int pre_shift, post_shift;
		int dummy;
		unsigned HOST_WIDE_INT d = INTVAL (op1);

		if (EXACT_POWER_OF_2_OR_ZERO_P (d))
		  {
		    pre_shift = floor_log2 (d);
		    if (rem_flag)
		      {
			remainder
			  = expand_binop (compute_mode, and_optab, op0,
					  GEN_INT (((HOST_WIDE_INT) 1 << pre_shift) - 1),
					  remainder, 1,
					  OPTAB_LIB_WIDEN);
			if (remainder)
			  return gen_lowpart (mode, remainder);
		      }
		    quotient = expand_shift (RSHIFT_EXPR, compute_mode, op0,
					     build_int_2 (pre_shift, 0),
					     tquotient, 1);
		  }
		else if (size <= HOST_BITS_PER_WIDE_INT)
		  {
		    if (d >= ((unsigned HOST_WIDE_INT) 1 << (size - 1)))
		      {
			/* Most significant bit of divisor is set; emit an scc
			   insn.  */
			quotient = emit_store_flag (tquotient, GEU, op0, op1,
						    compute_mode, 1, 1);
			if (quotient == 0)
			  goto fail1;
		      }
		    else
		      {
			/* Find a suitable multiplier and right shift count
			   instead of multiplying with D.  */

			mh = choose_multiplier (d, size, size,
						&ml, &post_shift, &dummy);

			/* If the suggested multiplier is more than SIZE bits,
			   we can do better for even divisors, using an
			   initial right shift.  */
			if (mh != 0 && (d & 1) == 0)
			  {
			    pre_shift = floor_log2 (d & -d);
			    mh = choose_multiplier (d >> pre_shift, size,
						    size - pre_shift,
						    &ml, &post_shift, &dummy);
			    if (mh)
			      abort ();
			  }
			else
			  pre_shift = 0;

			if (mh != 0)
			  {
			    rtx t1, t2, t3, t4;

			    extra_cost = (shift_cost[post_shift - 1]
					  + shift_cost[1] + 2 * add_cost);
			    t1 = expand_mult_highpart (compute_mode, op0, ml,
						       NULL_RTX, 1,
						       max_cost - extra_cost);
			    if (t1 == 0)
			      goto fail1;
			    t2 = force_operand (gen_rtx_MINUS (compute_mode,
							       op0, t1),
						NULL_RTX);
			    t3 = expand_shift (RSHIFT_EXPR, compute_mode, t2,
					       build_int_2 (1, 0), NULL_RTX,1);
			    t4 = force_operand (gen_rtx_PLUS (compute_mode,
							      t1, t3),
						NULL_RTX);
			    quotient
			      = expand_shift (RSHIFT_EXPR, compute_mode, t4,
					      build_int_2 (post_shift - 1, 0),
					      tquotient, 1);
			  }
			else
			  {
			    rtx t1, t2;

			    t1 = expand_shift (RSHIFT_EXPR, compute_mode, op0,
					       build_int_2 (pre_shift, 0),
					       NULL_RTX, 1);
			    extra_cost = (shift_cost[pre_shift]
					  + shift_cost[post_shift]);
			    t2 = expand_mult_highpart (compute_mode, t1, ml,
						       NULL_RTX, 1,
						       max_cost - extra_cost);
			    if (t2 == 0)
			      goto fail1;
			    quotient
			      = expand_shift (RSHIFT_EXPR, compute_mode, t2,
					      build_int_2 (post_shift, 0),
					      tquotient, 1);
			  }
		      }
		  }
		else		/* Too wide mode to use tricky code */
		  break;

		insn = get_last_insn ();
		if (insn != last
		    && (set = single_set (insn)) != 0
		    && SET_DEST (set) == quotient)
		  set_unique_reg_note (insn, 
		  		       REG_EQUAL,
				       gen_rtx_UDIV (compute_mode, op0, op1));
	      }
	    else		/* TRUNC_DIV, signed */
	      {
		unsigned HOST_WIDE_INT ml;
		int lgup, post_shift;
		HOST_WIDE_INT d = INTVAL (op1);
		unsigned HOST_WIDE_INT abs_d = d >= 0 ? d : -d;

		/* n rem d = n rem -d */
		if (rem_flag && d < 0)
		  {
		    d = abs_d;
		    op1 = GEN_INT (abs_d);
		  }

		if (d == 1)
		  quotient = op0;
		else if (d == -1)
		  quotient = expand_unop (compute_mode, neg_optab, op0,
					  tquotient, 0);
		else if (abs_d == (unsigned HOST_WIDE_INT) 1 << (size - 1))
		  {
		    /* This case is not handled correctly below.  */
		    quotient = emit_store_flag (tquotient, EQ, op0, op1,
						compute_mode, 1, 1);
		    if (quotient == 0)
		      goto fail1;
		  }
		else if (EXACT_POWER_OF_2_OR_ZERO_P (d)
			 && (rem_flag ? smod_pow2_cheap : sdiv_pow2_cheap))
		  ;
		else if (EXACT_POWER_OF_2_OR_ZERO_P (abs_d))
		  {
		    lgup = floor_log2 (abs_d);
		    if (abs_d != 2 && BRANCH_COST < 3)
		      {
			rtx label = gen_label_rtx ();
			rtx t1;

			t1 = copy_to_mode_reg (compute_mode, op0);
			do_cmp_and_jump (t1, const0_rtx, GE,
					 compute_mode, label);
			expand_inc (t1, GEN_INT (abs_d - 1));
			emit_label (label);
			quotient = expand_shift (RSHIFT_EXPR, compute_mode, t1,
						 build_int_2 (lgup, 0),
						 tquotient, 0);
		      }
		    else
		      {
			rtx t1, t2, t3;
			t1 = expand_shift (RSHIFT_EXPR, compute_mode, op0,
					   build_int_2 (size - 1, 0),
					   NULL_RTX, 0);
			t2 = expand_shift (RSHIFT_EXPR, compute_mode, t1,
					   build_int_2 (size - lgup, 0),
					   NULL_RTX, 1);
			t3 = force_operand (gen_rtx_PLUS (compute_mode,
							  op0, t2),
					    NULL_RTX);
			quotient = expand_shift (RSHIFT_EXPR, compute_mode, t3,
						 build_int_2 (lgup, 0),
						 tquotient, 0);
		      }

		    /* We have computed OP0 / abs(OP1).  If OP1 is negative, negate
		       the quotient.  */
		    if (d < 0)
		      {
			insn = get_last_insn ();
			if (insn != last
			    && (set = single_set (insn)) != 0
			    && SET_DEST (set) == quotient
			    && abs_d < ((unsigned HOST_WIDE_INT) 1
					<< (HOST_BITS_PER_WIDE_INT - 1)))
			  set_unique_reg_note (insn, 
			  		       REG_EQUAL,
					       gen_rtx_DIV (compute_mode,
							    op0,
							    GEN_INT (abs_d)));

			quotient = expand_unop (compute_mode, neg_optab,
						quotient, quotient, 0);
		      }
		  }
		else if (size <= HOST_BITS_PER_WIDE_INT)
		  {
		    choose_multiplier (abs_d, size, size - 1,
				       &ml, &post_shift, &lgup);
		    if (ml < (unsigned HOST_WIDE_INT) 1 << (size - 1))
		      {
			rtx t1, t2, t3;

			extra_cost = (shift_cost[post_shift]
				      + shift_cost[size - 1] + add_cost);
			t1 = expand_mult_highpart (compute_mode, op0, ml,
						   NULL_RTX, 0,
						   max_cost - extra_cost);
			if (t1 == 0)
			  goto fail1;
			t2 = expand_shift (RSHIFT_EXPR, compute_mode, t1,
					   build_int_2 (post_shift, 0), NULL_RTX, 0);
			t3 = expand_shift (RSHIFT_EXPR, compute_mode, op0,
					   build_int_2 (size - 1, 0), NULL_RTX, 0);
			if (d < 0)
			  quotient = force_operand (gen_rtx_MINUS (compute_mode, t3, t2),
						    tquotient);
			else
			  quotient = force_operand (gen_rtx_MINUS (compute_mode, t2, t3),
						    tquotient);
		      }
		    else
		      {
			rtx t1, t2, t3, t4;

			ml |= (~(unsigned HOST_WIDE_INT) 0) << (size - 1);
			extra_cost = (shift_cost[post_shift]
				      + shift_cost[size - 1] + 2 * add_cost);
			t1 = expand_mult_highpart (compute_mode, op0, ml,
						   NULL_RTX, 0,
						   max_cost - extra_cost);
			if (t1 == 0)
			  goto fail1;
			t2 = force_operand (gen_rtx_PLUS (compute_mode, t1, op0),
					    NULL_RTX);
			t3 = expand_shift (RSHIFT_EXPR, compute_mode, t2,
					   build_int_2 (post_shift, 0), NULL_RTX, 0);
			t4 = expand_shift (RSHIFT_EXPR, compute_mode, op0,
					   build_int_2 (size - 1, 0), NULL_RTX, 0);
			if (d < 0)
			  quotient = force_operand (gen_rtx_MINUS (compute_mode, t4, t3),
						    tquotient);
			else
			  quotient = force_operand (gen_rtx_MINUS (compute_mode, t3, t4),
						    tquotient);
		      }
		  }
		else		/* Too wide mode to use tricky code */
		  break;

		insn = get_last_insn ();
		if (insn != last
		    && (set = single_set (insn)) != 0
		    && SET_DEST (set) == quotient)
		  set_unique_reg_note (insn, 
		  		       REG_EQUAL,
				       gen_rtx_DIV (compute_mode, op0, op1));
	      }
	    break;
	  }
      fail1:
	delete_insns_since (last);
	break;

      case FLOOR_DIV_EXPR:
      case FLOOR_MOD_EXPR:
      /* We will come here only for signed operations.  */
	if (op1_is_constant && HOST_BITS_PER_WIDE_INT >= size)
	  {
	    unsigned HOST_WIDE_INT mh, ml;
	    int pre_shift, lgup, post_shift;
	    HOST_WIDE_INT d = INTVAL (op1);

	    if (d > 0)
	      {
		/* We could just as easily deal with negative constants here,
		   but it does not seem worth the trouble for GCC 2.6.  */
		if (EXACT_POWER_OF_2_OR_ZERO_P (d))
		  {
		    pre_shift = floor_log2 (d);
		    if (rem_flag)
		      {
			remainder = expand_binop (compute_mode, and_optab, op0,
						  GEN_INT (((HOST_WIDE_INT) 1 << pre_shift) - 1),
						  remainder, 0, OPTAB_LIB_WIDEN);
			if (remainder)
			  return gen_lowpart (mode, remainder);
		      }
		    quotient = expand_shift (RSHIFT_EXPR, compute_mode, op0,
					     build_int_2 (pre_shift, 0),
					     tquotient, 0);
		  }
		else
		  {
		    rtx t1, t2, t3, t4;

		    mh = choose_multiplier (d, size, size - 1,
					    &ml, &post_shift, &lgup);
		    if (mh)
		      abort ();

		    t1 = expand_shift (RSHIFT_EXPR, compute_mode, op0,
				       build_int_2 (size - 1, 0), NULL_RTX, 0);
		    t2 = expand_binop (compute_mode, xor_optab, op0, t1,
				       NULL_RTX, 0, OPTAB_WIDEN);
		    extra_cost = (shift_cost[post_shift]
				  + shift_cost[size - 1] + 2 * add_cost);
		    t3 = expand_mult_highpart (compute_mode, t2, ml,
					       NULL_RTX, 1,
					       max_cost - extra_cost);
		    if (t3 != 0)
		      {
			t4 = expand_shift (RSHIFT_EXPR, compute_mode, t3,
					   build_int_2 (post_shift, 0),
					   NULL_RTX, 1);
			quotient = expand_binop (compute_mode, xor_optab,
						 t4, t1, tquotient, 0,
						 OPTAB_WIDEN);
		      }
		  }
	      }
	    else
	      {
		rtx nsign, t1, t2, t3, t4;
		t1 = force_operand (gen_rtx_PLUS (compute_mode,
						  op0, constm1_rtx), NULL_RTX);
		t2 = expand_binop (compute_mode, ior_optab, op0, t1, NULL_RTX,
				   0, OPTAB_WIDEN);
		nsign = expand_shift (RSHIFT_EXPR, compute_mode, t2,
				      build_int_2 (size - 1, 0), NULL_RTX, 0);
		t3 = force_operand (gen_rtx_MINUS (compute_mode, t1, nsign),
				    NULL_RTX);
		t4 = expand_divmod (0, TRUNC_DIV_EXPR, compute_mode, t3, op1,
				    NULL_RTX, 0);
		if (t4)
		  {
		    rtx t5;
		    t5 = expand_unop (compute_mode, one_cmpl_optab, nsign,
				      NULL_RTX, 0);
		    quotient = force_operand (gen_rtx_PLUS (compute_mode,
							    t4, t5),
					      tquotient);
		  }
	      }
	  }

	if (quotient != 0)
	  break;
	delete_insns_since (last);

	/* Try using an instruction that produces both the quotient and
	   remainder, using truncation.  We can easily compensate the quotient
	   or remainder to get floor rounding, once we have the remainder.
	   Notice that we compute also the final remainder value here,
	   and return the result right away.  */
	if (target == 0 || GET_MODE (target) != compute_mode)
	  target = gen_reg_rtx (compute_mode);

	if (rem_flag)
	  {
	    remainder
	      = GET_CODE (target) == REG ? target : gen_reg_rtx (compute_mode);
	    quotient = gen_reg_rtx (compute_mode);
	  }
	else
	  {
	    quotient
	      = GET_CODE (target) == REG ? target : gen_reg_rtx (compute_mode);
	    remainder = gen_reg_rtx (compute_mode);
	  }

	if (expand_twoval_binop (sdivmod_optab, op0, op1,
				 quotient, remainder, 0))
	  {
	    /* This could be computed with a branch-less sequence.
	       Save that for later.  */
	    rtx tem;
	    rtx label = gen_label_rtx ();
	    do_cmp_and_jump (remainder, const0_rtx, EQ, compute_mode, label);
	    tem = expand_binop (compute_mode, xor_optab, op0, op1,
				NULL_RTX, 0, OPTAB_WIDEN);
	    do_cmp_and_jump (tem, const0_rtx, GE, compute_mode, label);
	    expand_dec (quotient, const1_rtx);
	    expand_inc (remainder, op1);
	    emit_label (label);
	    return gen_lowpart (mode, rem_flag ? remainder : quotient);
	  }

	/* No luck with division elimination or divmod.  Have to do it
	   by conditionally adjusting op0 *and* the result.  */
	{
	  rtx label1, label2, label3, label4, label5;
	  rtx adjusted_op0;
	  rtx tem;

	  quotient = gen_reg_rtx (compute_mode);
	  adjusted_op0 = copy_to_mode_reg (compute_mode, op0);
	  label1 = gen_label_rtx ();
	  label2 = gen_label_rtx ();
	  label3 = gen_label_rtx ();
	  label4 = gen_label_rtx ();
	  label5 = gen_label_rtx ();
	  do_cmp_and_jump (op1, const0_rtx, LT, compute_mode, label2);
	  do_cmp_and_jump (adjusted_op0, const0_rtx, LT, compute_mode, label1);
	  tem = expand_binop (compute_mode, sdiv_optab, adjusted_op0, op1,
			      quotient, 0, OPTAB_LIB_WIDEN);
	  if (tem != quotient)
	    emit_move_insn (quotient, tem);
	  emit_jump_insn (gen_jump (label5));
	  emit_barrier ();
	  emit_label (label1);
	  expand_inc (adjusted_op0, const1_rtx);
	  emit_jump_insn (gen_jump (label4));
	  emit_barrier ();
	  emit_label (label2);
	  do_cmp_and_jump (adjusted_op0, const0_rtx, GT, compute_mode, label3);
	  tem = expand_binop (compute_mode, sdiv_optab, adjusted_op0, op1,
			      quotient, 0, OPTAB_LIB_WIDEN);
	  if (tem != quotient)
	    emit_move_insn (quotient, tem);
	  emit_jump_insn (gen_jump (label5));
	  emit_barrier ();
	  emit_label (label3);
	  expand_dec (adjusted_op0, const1_rtx);
	  emit_label (label4);
	  tem = expand_binop (compute_mode, sdiv_optab, adjusted_op0, op1,
			      quotient, 0, OPTAB_LIB_WIDEN);
	  if (tem != quotient)
	    emit_move_insn (quotient, tem);
	  expand_dec (quotient, const1_rtx);
	  emit_label (label5);
	}
	break;

      case CEIL_DIV_EXPR:
      case CEIL_MOD_EXPR:
	if (unsignedp)
	  {
	    if (op1_is_constant && EXACT_POWER_OF_2_OR_ZERO_P (INTVAL (op1)))
	      {
		rtx t1, t2, t3;
		unsigned HOST_WIDE_INT d = INTVAL (op1);
		t1 = expand_shift (RSHIFT_EXPR, compute_mode, op0,
				   build_int_2 (floor_log2 (d), 0),
				   tquotient, 1);
		t2 = expand_binop (compute_mode, and_optab, op0,
				   GEN_INT (d - 1),
				   NULL_RTX, 1, OPTAB_LIB_WIDEN);
		t3 = gen_reg_rtx (compute_mode);
		t3 = emit_store_flag (t3, NE, t2, const0_rtx,
				      compute_mode, 1, 1);
		if (t3 == 0)
		  {
		    rtx lab;
		    lab = gen_label_rtx ();
		    do_cmp_and_jump (t2, const0_rtx, EQ, compute_mode, lab);
		    expand_inc (t1, const1_rtx);
		    emit_label (lab);
		    quotient = t1;
		  }
		else
		  quotient = force_operand (gen_rtx_PLUS (compute_mode,
							  t1, t3),
					    tquotient);
		break;
	      }

	    /* Try using an instruction that produces both the quotient and
	       remainder, using truncation.  We can easily compensate the
	       quotient or remainder to get ceiling rounding, once we have the
	       remainder.  Notice that we compute also the final remainder
	       value here, and return the result right away.  */
	    if (target == 0 || GET_MODE (target) != compute_mode)
	      target = gen_reg_rtx (compute_mode);

	    if (rem_flag)
	      {
		remainder = (GET_CODE (target) == REG
			     ? target : gen_reg_rtx (compute_mode));
		quotient = gen_reg_rtx (compute_mode);
	      }
	    else
	      {
		quotient = (GET_CODE (target) == REG
			    ? target : gen_reg_rtx (compute_mode));
		remainder = gen_reg_rtx (compute_mode);
	      }

	    if (expand_twoval_binop (udivmod_optab, op0, op1, quotient,
				     remainder, 1))
	      {
		/* This could be computed with a branch-less sequence.
		   Save that for later.  */
		rtx label = gen_label_rtx ();
		do_cmp_and_jump (remainder, const0_rtx, EQ,
				 compute_mode, label);
		expand_inc (quotient, const1_rtx);
		expand_dec (remainder, op1);
		emit_label (label);
		return gen_lowpart (mode, rem_flag ? remainder : quotient);
	      }

	    /* No luck with division elimination or divmod.  Have to do it
	       by conditionally adjusting op0 *and* the result.  */
	    {
	      rtx label1, label2;
	      rtx adjusted_op0, tem;

	      quotient = gen_reg_rtx (compute_mode);
	      adjusted_op0 = copy_to_mode_reg (compute_mode, op0);
	      label1 = gen_label_rtx ();
	      label2 = gen_label_rtx ();
	      do_cmp_and_jump (adjusted_op0, const0_rtx, NE,
			       compute_mode, label1);
	      emit_move_insn  (quotient, const0_rtx);
	      emit_jump_insn (gen_jump (label2));
	      emit_barrier ();
	      emit_label (label1);
	      expand_dec (adjusted_op0, const1_rtx);
	      tem = expand_binop (compute_mode, udiv_optab, adjusted_op0, op1,
				  quotient, 1, OPTAB_LIB_WIDEN);
	      if (tem != quotient)
		emit_move_insn (quotient, tem);
	      expand_inc (quotient, const1_rtx);
	      emit_label (label2);
	    }
	  }
	else /* signed */
	  {
	    if (op1_is_constant && EXACT_POWER_OF_2_OR_ZERO_P (INTVAL (op1))
		&& INTVAL (op1) >= 0)
	      {
		/* This is extremely similar to the code for the unsigned case
		   above.  For 2.7 we should merge these variants, but for
		   2.6.1 I don't want to touch the code for unsigned since that
		   get used in C.  The signed case will only be used by other
		   languages (Ada).  */

		rtx t1, t2, t3;
		unsigned HOST_WIDE_INT d = INTVAL (op1);
		t1 = expand_shift (RSHIFT_EXPR, compute_mode, op0,
				   build_int_2 (floor_log2 (d), 0),
				   tquotient, 0);
		t2 = expand_binop (compute_mode, and_optab, op0,
				   GEN_INT (d - 1),
				   NULL_RTX, 1, OPTAB_LIB_WIDEN);
		t3 = gen_reg_rtx (compute_mode);
		t3 = emit_store_flag (t3, NE, t2, const0_rtx,
				      compute_mode, 1, 1);
		if (t3 == 0)
		  {
		    rtx lab;
		    lab = gen_label_rtx ();
		    do_cmp_and_jump (t2, const0_rtx, EQ, compute_mode, lab);
		    expand_inc (t1, const1_rtx);
		    emit_label (lab);
		    quotient = t1;
		  }
		else
		  quotient = force_operand (gen_rtx_PLUS (compute_mode,
							  t1, t3),
					    tquotient);
		break;
	      }

	    /* Try using an instruction that produces both the quotient and
	       remainder, using truncation.  We can easily compensate the
	       quotient or remainder to get ceiling rounding, once we have the
	       remainder.  Notice that we compute also the final remainder
	       value here, and return the result right away.  */
	    if (target == 0 || GET_MODE (target) != compute_mode)
	      target = gen_reg_rtx (compute_mode);
	    if (rem_flag)
	      {
		remainder= (GET_CODE (target) == REG
			    ? target : gen_reg_rtx (compute_mode));
		quotient = gen_reg_rtx (compute_mode);
	      }
	    else
	      {
		quotient = (GET_CODE (target) == REG
			    ? target : gen_reg_rtx (compute_mode));
		remainder = gen_reg_rtx (compute_mode);
	      }

	    if (expand_twoval_binop (sdivmod_optab, op0, op1, quotient,
				     remainder, 0))
	      {
		/* This could be computed with a branch-less sequence.
		   Save that for later.  */
		rtx tem;
		rtx label = gen_label_rtx ();
		do_cmp_and_jump (remainder, const0_rtx, EQ,
				 compute_mode, label);
		tem = expand_binop (compute_mode, xor_optab, op0, op1,
				    NULL_RTX, 0, OPTAB_WIDEN);
		do_cmp_and_jump (tem, const0_rtx, LT, compute_mode, label);
		expand_inc (quotient, const1_rtx);
		expand_dec (remainder, op1);
		emit_label (label);
		return gen_lowpart (mode, rem_flag ? remainder : quotient);
	      }

	    /* No luck with division elimination or divmod.  Have to do it
	       by conditionally adjusting op0 *and* the result.  */
	    {
	      rtx label1, label2, label3, label4, label5;
	      rtx adjusted_op0;
	      rtx tem;

	      quotient = gen_reg_rtx (compute_mode);
	      adjusted_op0 = copy_to_mode_reg (compute_mode, op0);
	      label1 = gen_label_rtx ();
	      label2 = gen_label_rtx ();
	      label3 = gen_label_rtx ();
	      label4 = gen_label_rtx ();
	      label5 = gen_label_rtx ();
	      do_cmp_and_jump (op1, const0_rtx, LT, compute_mode, label2);
	      do_cmp_and_jump (adjusted_op0, const0_rtx, GT,
			       compute_mode, label1);
	      tem = expand_binop (compute_mode, sdiv_optab, adjusted_op0, op1,
				  quotient, 0, OPTAB_LIB_WIDEN);
	      if (tem != quotient)
		emit_move_insn (quotient, tem);
	      emit_jump_insn (gen_jump (label5));
	      emit_barrier ();
	      emit_label (label1);
	      expand_dec (adjusted_op0, const1_rtx);
	      emit_jump_insn (gen_jump (label4));
	      emit_barrier ();
	      emit_label (label2);
	      do_cmp_and_jump (adjusted_op0, const0_rtx, LT,
			       compute_mode, label3);
	      tem = expand_binop (compute_mode, sdiv_optab, adjusted_op0, op1,
				  quotient, 0, OPTAB_LIB_WIDEN);
	      if (tem != quotient)
		emit_move_insn (quotient, tem);
	      emit_jump_insn (gen_jump (label5));
	      emit_barrier ();
	      emit_label (label3);
	      expand_inc (adjusted_op0, const1_rtx);
	      emit_label (label4);
	      tem = expand_binop (compute_mode, sdiv_optab, adjusted_op0, op1,
				  quotient, 0, OPTAB_LIB_WIDEN);
	      if (tem != quotient)
		emit_move_insn (quotient, tem);
	      expand_inc (quotient, const1_rtx);
	      emit_label (label5);
	    }
	  }
	break;

      case EXACT_DIV_EXPR:
	if (op1_is_constant && HOST_BITS_PER_WIDE_INT >= size)
	  {
	    HOST_WIDE_INT d = INTVAL (op1);
	    unsigned HOST_WIDE_INT ml;
	    int post_shift;
	    rtx t1;

	    post_shift = floor_log2 (d & -d);
	    ml = invert_mod2n (d >> post_shift, size);
	    t1 = expand_mult (compute_mode, op0, GEN_INT (ml), NULL_RTX,
			      unsignedp);
	    quotient = expand_shift (RSHIFT_EXPR, compute_mode, t1,
				     build_int_2 (post_shift, 0),
				     NULL_RTX, unsignedp);

	    insn = get_last_insn ();
	    set_unique_reg_note (insn,
	    			 REG_EQUAL,
				 gen_rtx_fmt_ee (unsignedp ? UDIV : DIV,
						 compute_mode,
						 op0, op1));
	  }
	break;

      case ROUND_DIV_EXPR:
      case ROUND_MOD_EXPR:
	if (unsignedp)
	  {
	    rtx tem;
	    rtx label;
	    label = gen_label_rtx ();
	    quotient = gen_reg_rtx (compute_mode);
	    remainder = gen_reg_rtx (compute_mode);
	    if (expand_twoval_binop (udivmod_optab, op0, op1, quotient, remainder, 1) == 0)
	      {
		rtx tem;
		quotient = expand_binop (compute_mode, udiv_optab, op0, op1,
					 quotient, 1, OPTAB_LIB_WIDEN);
		tem = expand_mult (compute_mode, quotient, op1, NULL_RTX, 1);
		remainder = expand_binop (compute_mode, sub_optab, op0, tem,
					  remainder, 1, OPTAB_LIB_WIDEN);
	      }
	    tem = plus_constant (op1, -1);
	    tem = expand_shift (RSHIFT_EXPR, compute_mode, tem,
				build_int_2 (1, 0), NULL_RTX, 1);
	    do_cmp_and_jump (remainder, tem, LEU, compute_mode, label);
	    expand_inc (quotient, const1_rtx);
	    expand_dec (remainder, op1);
	    emit_label (label);
	  }
	else
	  {
	    rtx abs_rem, abs_op1, tem, mask;
	    rtx label;
	    label = gen_label_rtx ();
	    quotient = gen_reg_rtx (compute_mode);
	    remainder = gen_reg_rtx (compute_mode);
	    if (expand_twoval_binop (sdivmod_optab, op0, op1, quotient, remainder, 0) == 0)
	      {
		rtx tem;
		quotient = expand_binop (compute_mode, sdiv_optab, op0, op1,
					 quotient, 0, OPTAB_LIB_WIDEN);
		tem = expand_mult (compute_mode, quotient, op1, NULL_RTX, 0);
		remainder = expand_binop (compute_mode, sub_optab, op0, tem,
					  remainder, 0, OPTAB_LIB_WIDEN);
	      }
	    abs_rem = expand_abs (compute_mode, remainder, NULL_RTX, 0);
	    abs_op1 = expand_abs (compute_mode, op1, NULL_RTX, 0);
	    tem = expand_shift (LSHIFT_EXPR, compute_mode, abs_rem,
				build_int_2 (1, 0), NULL_RTX, 1);
	    do_cmp_and_jump (tem, abs_op1, LTU, compute_mode, label);
	    tem = expand_binop (compute_mode, xor_optab, op0, op1,
				NULL_RTX, 0, OPTAB_WIDEN);
	    mask = expand_shift (RSHIFT_EXPR, compute_mode, tem,
				build_int_2 (size - 1, 0), NULL_RTX, 0);
	    tem = expand_binop (compute_mode, xor_optab, mask, const1_rtx,
				NULL_RTX, 0, OPTAB_WIDEN);
	    tem = expand_binop (compute_mode, sub_optab, tem, mask,
				NULL_RTX, 0, OPTAB_WIDEN);
	    expand_inc (quotient, tem);
	    tem = expand_binop (compute_mode, xor_optab, mask, op1,
				NULL_RTX, 0, OPTAB_WIDEN);
	    tem = expand_binop (compute_mode, sub_optab, tem, mask,
				NULL_RTX, 0, OPTAB_WIDEN);
	    expand_dec (remainder, tem);
	    emit_label (label);
	  }
	return gen_lowpart (mode, rem_flag ? remainder : quotient);
	
      default:
	abort ();
      }

  if (quotient == 0)
    {
      if (target && GET_MODE (target) != compute_mode)
	target = 0;

      if (rem_flag)
	{
	  /* Try to produce the remainder without producing the quotient.
	     If we seem to have a divmod patten that does not require widening,
	     don't try windening here.  We should really have an WIDEN argument
	     to expand_twoval_binop, since what we'd really like to do here is
	     1) try a mod insn in compute_mode
	     2) try a divmod insn in compute_mode
	     3) try a div insn in compute_mode and multiply-subtract to get
	        remainder
	     4) try the same things with widening allowed.  */
	  remainder
	    = sign_expand_binop (compute_mode, umod_optab, smod_optab,
				 op0, op1, target,
				 unsignedp,
				 ((optab2->handlers[(int) compute_mode].insn_code
				   != CODE_FOR_nothing)
				  ? OPTAB_DIRECT : OPTAB_WIDEN));
	  if (remainder == 0)
	    {
	      /* No luck there.  Can we do remainder and divide at once
		 without a library call?  */
	      remainder = gen_reg_rtx (compute_mode);
	      if (! expand_twoval_binop ((unsignedp
					  ? udivmod_optab
					  : sdivmod_optab),
					 op0, op1,
					 NULL_RTX, remainder, unsignedp))
		remainder = 0;
	    }

	  if (remainder)
	    return gen_lowpart (mode, remainder);
	}

      /* Produce the quotient.  Try a quotient insn, but not a library call.
	 If we have a divmod in this mode, use it in preference to widening
	 the div (for this test we assume it will not fail). Note that optab2
	 is set to the one of the two optabs that the call below will use.  */
      quotient
	= sign_expand_binop (compute_mode, udiv_optab, sdiv_optab,
			     op0, op1, rem_flag ? NULL_RTX : target,
			     unsignedp,
			     ((optab2->handlers[(int) compute_mode].insn_code
			       != CODE_FOR_nothing)
			      ? OPTAB_DIRECT : OPTAB_WIDEN));

      if (quotient == 0)
	{
	  /* No luck there.  Try a quotient-and-remainder insn,
	     keeping the quotient alone.  */
	  quotient = gen_reg_rtx (compute_mode);
	  if (! expand_twoval_binop (unsignedp ? udivmod_optab : sdivmod_optab,
				     op0, op1,
				     quotient, NULL_RTX, unsignedp))
	    {
	      quotient = 0;
	      if (! rem_flag)
		/* Still no luck.  If we are not computing the remainder,
		   use a library call for the quotient.  */
		quotient = sign_expand_binop (compute_mode,
					      udiv_optab, sdiv_optab,
					      op0, op1, target,
					      unsignedp, OPTAB_LIB_WIDEN);
	    }
	}
    }

  if (rem_flag)
    {
      if (target && GET_MODE (target) != compute_mode)
	target = 0;

      if (quotient == 0)
	/* No divide instruction either.  Use library for remainder.  */
	remainder = sign_expand_binop (compute_mode, umod_optab, smod_optab,
				       op0, op1, target,
				       unsignedp, OPTAB_LIB_WIDEN);
      else
	{
	  /* We divided.  Now finish doing X - Y * (X / Y).  */
	  remainder = expand_mult (compute_mode, quotient, op1,
				   NULL_RTX, unsignedp);
	  remainder = expand_binop (compute_mode, sub_optab, op0,
				    remainder, target, unsignedp,
				    OPTAB_LIB_WIDEN);
	}
    }

  return gen_lowpart (mode, rem_flag ? remainder : quotient);
}

/* Return a tree node with data type TYPE, describing the value of X.
   Usually this is an RTL_EXPR, if there is no obvious better choice.
   X may be an expression, however we only support those expressions
   generated by loop.c.   */

tree
make_tree (type, x)
     tree type;
     rtx x;
{
  tree t;

  switch (GET_CODE (x))
    {
    case CONST_INT:
      t = build_int_2 (INTVAL (x),
		       (TREE_UNSIGNED (type)
			&& (GET_MODE_BITSIZE (TYPE_MODE (type)) < HOST_BITS_PER_WIDE_INT))
		       || INTVAL (x) >= 0 ? 0 : -1);
      TREE_TYPE (t) = type;
      return t;

    case CONST_DOUBLE:
      if (GET_MODE (x) == VOIDmode)
	{
	  t = build_int_2 (CONST_DOUBLE_LOW (x), CONST_DOUBLE_HIGH (x));
	  TREE_TYPE (t) = type;
	}
      else
	{
	  REAL_VALUE_TYPE d;

	  REAL_VALUE_FROM_CONST_DOUBLE (d, x);
	  t = build_real (type, d);
	}

      return t;
	  
    case PLUS:
      return fold (build (PLUS_EXPR, type, make_tree (type, XEXP (x, 0)),
			  make_tree (type, XEXP (x, 1))));
						       
    case MINUS:
      return fold (build (MINUS_EXPR, type, make_tree (type, XEXP (x, 0)),
			  make_tree (type, XEXP (x, 1))));
						       
    case NEG:
      return fold (build1 (NEGATE_EXPR, type, make_tree (type, XEXP (x, 0))));

    case MULT:
      return fold (build (MULT_EXPR, type, make_tree (type, XEXP (x, 0)),
			  make_tree (type, XEXP (x, 1))));
						      
    case ASHIFT:
      return fold (build (LSHIFT_EXPR, type, make_tree (type, XEXP (x, 0)),
			  make_tree (type, XEXP (x, 1))));
						      
    case LSHIFTRT:
      return fold (convert (type,
			    build (RSHIFT_EXPR, unsigned_type (type),
				   make_tree (unsigned_type (type),
					      XEXP (x, 0)),
				   make_tree (type, XEXP (x, 1)))));
						      
    case ASHIFTRT:
      return fold (convert (type,
			    build (RSHIFT_EXPR, signed_type (type),
				   make_tree (signed_type (type), XEXP (x, 0)),
				   make_tree (type, XEXP (x, 1)))));
						      
    case DIV:
      if (TREE_CODE (type) != REAL_TYPE)
	t = signed_type (type);
      else
	t = type;

      return fold (convert (type,
			    build (TRUNC_DIV_EXPR, t,
				   make_tree (t, XEXP (x, 0)),
				   make_tree (t, XEXP (x, 1)))));
    case UDIV:
      t = unsigned_type (type);
      return fold (convert (type,
			    build (TRUNC_DIV_EXPR, t,
				   make_tree (t, XEXP (x, 0)),
				   make_tree (t, XEXP (x, 1)))));
   default:
      t = make_node (RTL_EXPR);
      TREE_TYPE (t) = type;
      RTL_EXPR_RTL (t) = x;
      /* There are no insns to be output
	 when this rtl_expr is used.  */
      RTL_EXPR_SEQUENCE (t) = 0;
      return t;
    }
}

/* Return an rtx representing the value of X * MULT + ADD.
   TARGET is a suggestion for where to store the result (an rtx).
   MODE is the machine mode for the computation.
   X and MULT must have mode MODE.  ADD may have a different mode.
   So can X (defaults to same as MODE).
   UNSIGNEDP is non-zero to do unsigned multiplication.
   This may emit insns.  */

rtx
expand_mult_add (x, target, mult, add, mode, unsignedp)
     rtx x, target, mult, add;
     enum machine_mode mode;
     int unsignedp;
{
  tree type = type_for_mode (mode, unsignedp);
  tree add_type = (GET_MODE (add) == VOIDmode
		   ? type : type_for_mode (GET_MODE (add), unsignedp));
  tree result =  fold (build (PLUS_EXPR, type,
			      fold (build (MULT_EXPR, type,
					   make_tree (type, x),
					   make_tree (type, mult))),
			      make_tree (add_type, add)));

  return expand_expr (result, target, VOIDmode, 0);
}

/* Compute the logical-and of OP0 and OP1, storing it in TARGET
   and returning TARGET.

   If TARGET is 0, a pseudo-register or constant is returned.  */

rtx
expand_and (op0, op1, target)
     rtx op0, op1, target;
{
  enum machine_mode mode = VOIDmode;
  rtx tem;

  if (GET_MODE (op0) != VOIDmode)
    mode = GET_MODE (op0);
  else if (GET_MODE (op1) != VOIDmode)
    mode = GET_MODE (op1);

  if (mode != VOIDmode)
    tem = expand_binop (mode, and_optab, op0, op1, target, 0, OPTAB_LIB_WIDEN);
  else if (GET_CODE (op0) == CONST_INT && GET_CODE (op1) == CONST_INT)
    tem = GEN_INT (INTVAL (op0) & INTVAL (op1));
  else
    abort ();

  if (target == 0)
    target = tem;
  else if (tem != target)
    emit_move_insn (target, tem);
  return target;
}

/* Emit a store-flags instruction for comparison CODE on OP0 and OP1
   and storing in TARGET.  Normally return TARGET.
   Return 0 if that cannot be done.

   MODE is the mode to use for OP0 and OP1 should they be CONST_INTs.  If
   it is VOIDmode, they cannot both be CONST_INT.  

   UNSIGNEDP is for the case where we have to widen the operands
   to perform the operation.  It says to use zero-extension.

   NORMALIZEP is 1 if we should convert the result to be either zero
   or one.  Normalize is -1 if we should convert the result to be
   either zero or -1.  If NORMALIZEP is zero, the result will be left
   "raw" out of the scc insn.  */

rtx
emit_store_flag (target, code, op0, op1, mode, unsignedp, normalizep)
     rtx target;
     enum rtx_code code;
     rtx op0, op1;
     enum machine_mode mode;
     int unsignedp;
     int normalizep;
{
  rtx subtarget;
  enum insn_code icode;
  enum machine_mode compare_mode;
  enum machine_mode target_mode = GET_MODE (target);
  rtx tem;
  rtx last = get_last_insn ();
  rtx pattern, comparison;

  /* If one operand is constant, make it the second one.  Only do this
     if the other operand is not constant as well.  */

  if ((CONSTANT_P (op0) && ! CONSTANT_P (op1))
      || (GET_CODE (op0) == CONST_INT && GET_CODE (op1) != CONST_INT))
    {
      tem = op0;
      op0 = op1;
      op1 = tem;
      code = swap_condition (code);
    }

  if (mode == VOIDmode)
    mode = GET_MODE (op0);

  /* For some comparisons with 1 and -1, we can convert this to 
     comparisons with zero.  This will often produce more opportunities for
     store-flag insns.  */

  switch (code)
    {
    case LT:
      if (op1 == const1_rtx)
	op1 = const0_rtx, code = LE;
      break;
    case LE:
      if (op1 == constm1_rtx)
	op1 = const0_rtx, code = LT;
      break;
    case GE:
      if (op1 == const1_rtx)
	op1 = const0_rtx, code = GT;
      break;
    case GT:
      if (op1 == constm1_rtx)
	op1 = const0_rtx, code = GE;
      break;
    case GEU:
      if (op1 == const1_rtx)
	op1 = const0_rtx, code = NE;
      break;
    case LTU:
      if (op1 == const1_rtx)
	op1 = const0_rtx, code = EQ;
      break;
    default:
      break;
    }

  /* From now on, we won't change CODE, so set ICODE now.  */
  icode = setcc_gen_code[(int) code];

  /* If this is A < 0 or A >= 0, we can do this by taking the ones
     complement of A (for GE) and shifting the sign bit to the low bit.  */
  if (op1 == const0_rtx && (code == LT || code == GE)
      && GET_MODE_CLASS (mode) == MODE_INT
      && (normalizep || STORE_FLAG_VALUE == 1
	  || (GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT
	      && ((STORE_FLAG_VALUE & GET_MODE_MASK (mode))
		  == (HOST_WIDE_INT) 1 << (GET_MODE_BITSIZE (mode) - 1)))))
    {
      subtarget = target;

      /* If the result is to be wider than OP0, it is best to convert it
	 first.  If it is to be narrower, it is *incorrect* to convert it
	 first.  */
      if (GET_MODE_SIZE (target_mode) > GET_MODE_SIZE (mode))
	{
	  op0 = protect_from_queue (op0, 0);
	  op0 = convert_modes (target_mode, mode, op0, 0);
	  mode = target_mode;
	}

      if (target_mode != mode)
	subtarget = 0;

      if (code == GE)
	op0 = expand_unop (mode, one_cmpl_optab, op0,
			   ((STORE_FLAG_VALUE == 1 || normalizep)
			    ? 0 : subtarget), 0);

      if (STORE_FLAG_VALUE == 1 || normalizep)
	/* If we are supposed to produce a 0/1 value, we want to do
	   a logical shift from the sign bit to the low-order bit; for
	   a -1/0 value, we do an arithmetic shift.  */
	op0 = expand_shift (RSHIFT_EXPR, mode, op0,
			    size_int (GET_MODE_BITSIZE (mode) - 1),
			    subtarget, normalizep != -1);

      if (mode != target_mode)
	op0 = convert_modes (target_mode, mode, op0, 0);

      return op0;
    }

  if (icode != CODE_FOR_nothing)
    {
      /* We think we may be able to do this with a scc insn.  Emit the
	 comparison and then the scc insn.

	 compare_from_rtx may call emit_queue, which would be deleted below
	 if the scc insn fails.  So call it ourselves before setting LAST.  */

      emit_queue ();
      last = get_last_insn ();

      comparison
	= compare_from_rtx (op0, op1, code, unsignedp, mode, NULL_RTX, 0);
      if (GET_CODE (comparison) == CONST_INT)
	return (comparison == const0_rtx ? const0_rtx
		: normalizep == 1 ? const1_rtx
		: normalizep == -1 ? constm1_rtx
		: const_true_rtx);

      /* If the code of COMPARISON doesn't match CODE, something is
	 wrong; we can no longer be sure that we have the operation.  
	 We could handle this case, but it should not happen.  */

      if (GET_CODE (comparison) != code)
	abort ();

      /* Get a reference to the target in the proper mode for this insn.  */
      compare_mode = insn_operand_mode[(int) icode][0];
      subtarget = target;
      if (preserve_subexpressions_p ()
	  || ! (*insn_operand_predicate[(int) icode][0]) (subtarget, compare_mode))
	subtarget = gen_reg_rtx (compare_mode);

      pattern = GEN_FCN (icode) (subtarget);
      if (pattern)
	{
	  emit_insn (pattern);

	  /* If we are converting to a wider mode, first convert to
	     TARGET_MODE, then normalize.  This produces better combining
	     opportunities on machines that have a SIGN_EXTRACT when we are
	     testing a single bit.  This mostly benefits the 68k.

	     If STORE_FLAG_VALUE does not have the sign bit set when
	     interpreted in COMPARE_MODE, we can do this conversion as
	     unsigned, which is usually more efficient.  */
	  if (GET_MODE_SIZE (target_mode) > GET_MODE_SIZE (compare_mode))
	    {
	      convert_move (target, subtarget,
			    (GET_MODE_BITSIZE (compare_mode)
			     <= HOST_BITS_PER_WIDE_INT)
			    && 0 == (STORE_FLAG_VALUE
				     & ((HOST_WIDE_INT) 1
					<< (GET_MODE_BITSIZE (compare_mode) -1))));
	      op0 = target;
	      compare_mode = target_mode;
	    }
	  else
	    op0 = subtarget;

	  /* If we want to keep subexpressions around, don't reuse our
	     last target.  */

	  if (preserve_subexpressions_p ())
	    subtarget = 0;

	  /* Now normalize to the proper value in COMPARE_MODE.  Sometimes
	     we don't have to do anything.  */
	  if (normalizep == 0 || normalizep == STORE_FLAG_VALUE)
	    ;
	  else if (normalizep == - STORE_FLAG_VALUE)
	    op0 = expand_unop (compare_mode, neg_optab, op0, subtarget, 0);

	  /* We don't want to use STORE_FLAG_VALUE < 0 below since this
	     makes it hard to use a value of just the sign bit due to
	     ANSI integer constant typing rules.  */
	  else if (GET_MODE_BITSIZE (compare_mode) <= HOST_BITS_PER_WIDE_INT
		   && (STORE_FLAG_VALUE
		       & ((HOST_WIDE_INT) 1
			  << (GET_MODE_BITSIZE (compare_mode) - 1))))
	    op0 = expand_shift (RSHIFT_EXPR, compare_mode, op0,
				size_int (GET_MODE_BITSIZE (compare_mode) - 1),
				subtarget, normalizep == 1);
	  else if (STORE_FLAG_VALUE & 1)
	    {
	      op0 = expand_and (op0, const1_rtx, subtarget);
	      if (normalizep == -1)
		op0 = expand_unop (compare_mode, neg_optab, op0, op0, 0);
	    }
	  else
	    abort ();

	  /* If we were converting to a smaller mode, do the 
	     conversion now.  */
	  if (target_mode != compare_mode)
	    {
	      convert_move (target, op0, 0);
	      return target;
	    }
	  else
	    return op0;
	}
    }

  delete_insns_since (last);

  /* If expensive optimizations, use different pseudo registers for each
     insn, instead of reusing the same pseudo.  This leads to better CSE,
     but slows down the compiler, since there are more pseudos */
  subtarget = (!flag_expensive_optimizations
	       && (target_mode == mode)) ? target : NULL_RTX;

  /* If we reached here, we can't do this with a scc insn.  However, there
     are some comparisons that can be done directly.  For example, if
     this is an equality comparison of integers, we can try to exclusive-or
     (or subtract) the two operands and use a recursive call to try the
     comparison with zero.  Don't do any of these cases if branches are
     very cheap.  */

  if (BRANCH_COST > 0
      && GET_MODE_CLASS (mode) == MODE_INT && (code == EQ || code == NE)
      && op1 != const0_rtx)
    {
      tem = expand_binop (mode, xor_optab, op0, op1, subtarget, 1,
			  OPTAB_WIDEN);

      if (tem == 0)
	tem = expand_binop (mode, sub_optab, op0, op1, subtarget, 1,
			    OPTAB_WIDEN);
      if (tem != 0)
	tem = emit_store_flag (target, code, tem, const0_rtx,
			       mode, unsignedp, normalizep);
      if (tem == 0)
	delete_insns_since (last);
      return tem;
    }

  /* Some other cases we can do are EQ, NE, LE, and GT comparisons with 
     the constant zero.  Reject all other comparisons at this point.  Only
     do LE and GT if branches are expensive since they are expensive on
     2-operand machines.  */

  if (BRANCH_COST == 0
      || GET_MODE_CLASS (mode) != MODE_INT || op1 != const0_rtx
      || (code != EQ && code != NE
	  && (BRANCH_COST <= 1 || (code != LE && code != GT))))
    return 0;

  /* See what we need to return.  We can only return a 1, -1, or the
     sign bit.  */

  if (normalizep == 0)
    {
      if (STORE_FLAG_VALUE == 1 || STORE_FLAG_VALUE == -1)
	normalizep = STORE_FLAG_VALUE;

      else if (GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT
	       && ((STORE_FLAG_VALUE & GET_MODE_MASK (mode))
		   == (unsigned HOST_WIDE_INT) 1 << (GET_MODE_BITSIZE (mode) - 1)))
	;
      else
	return 0;
    }

  /* Try to put the result of the comparison in the sign bit.  Assume we can't
     do the necessary operation below.  */

  tem = 0;

  /* To see if A <= 0, compute (A | (A - 1)).  A <= 0 iff that result has
     the sign bit set.  */

  if (code == LE)
    {
      /* This is destructive, so SUBTARGET can't be OP0.  */
      if (rtx_equal_p (subtarget, op0))
	subtarget = 0;

      tem = expand_binop (mode, sub_optab, op0, const1_rtx, subtarget, 0,
			  OPTAB_WIDEN);
      if (tem)
	tem = expand_binop (mode, ior_optab, op0, tem, subtarget, 0,
			    OPTAB_WIDEN);
    }

  /* To see if A > 0, compute (((signed) A) << BITS) - A, where BITS is the
     number of bits in the mode of OP0, minus one.  */

  if (code == GT)
    {
      if (rtx_equal_p (subtarget, op0))
	subtarget = 0;

      tem = expand_shift (RSHIFT_EXPR, mode, op0,
			  size_int (GET_MODE_BITSIZE (mode) - 1),
			  subtarget, 0);
      tem = expand_binop (mode, sub_optab, tem, op0, subtarget, 0,
			  OPTAB_WIDEN);
    }
				    
  if (code == EQ || code == NE)
    {
      /* For EQ or NE, one way to do the comparison is to apply an operation
	 that converts the operand into a positive number if it is non-zero
	 or zero if it was originally zero.  Then, for EQ, we subtract 1 and
	 for NE we negate.  This puts the result in the sign bit.  Then we
	 normalize with a shift, if needed. 

	 Two operations that can do the above actions are ABS and FFS, so try
	 them.  If that doesn't work, and MODE is smaller than a full word,
	 we can use zero-extension to the wider mode (an unsigned conversion)
	 as the operation.  */

      if (abs_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
	tem = expand_unop (mode, abs_optab, op0, subtarget, 1);
      else if (ffs_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
	tem = expand_unop (mode, ffs_optab, op0, subtarget, 1);
      else if (GET_MODE_SIZE (mode) < UNITS_PER_WORD)
	{
	  op0 = protect_from_queue (op0, 0);
	  tem = convert_modes (word_mode, mode, op0, 1);
	  mode = word_mode;
	}

      if (tem != 0)
	{
	  if (code == EQ)
	    tem = expand_binop (mode, sub_optab, tem, const1_rtx, subtarget,
				0, OPTAB_WIDEN);
	  else
	    tem = expand_unop (mode, neg_optab, tem, subtarget, 0);
	}

      /* If we couldn't do it that way, for NE we can "or" the two's complement
	 of the value with itself.  For EQ, we take the one's complement of
	 that "or", which is an extra insn, so we only handle EQ if branches
	 are expensive.  */

      if (tem == 0 && (code == NE || BRANCH_COST > 1))
	{
	  if (rtx_equal_p (subtarget, op0))
	    subtarget = 0;

	  tem = expand_unop (mode, neg_optab, op0, subtarget, 0);
	  tem = expand_binop (mode, ior_optab, tem, op0, subtarget, 0,
			      OPTAB_WIDEN);

	  if (tem && code == EQ)
	    tem = expand_unop (mode, one_cmpl_optab, tem, subtarget, 0);
	}
    }

  if (tem && normalizep)
    tem = expand_shift (RSHIFT_EXPR, mode, tem,
			size_int (GET_MODE_BITSIZE (mode) - 1),
			subtarget, normalizep == 1);

  if (tem)
    {
      if (GET_MODE (tem) != target_mode)
	{
	  convert_move (target, tem, 0);
	  tem = target;
	}
      else if (!subtarget)
	{
	  emit_move_insn (target, tem);
	  tem = target;
	}
    }
  else
    delete_insns_since (last);

  return tem;
}

/* Like emit_store_flag, but always succeeds.  */

rtx
emit_store_flag_force (target, code, op0, op1, mode, unsignedp, normalizep)
     rtx target;
     enum rtx_code code;
     rtx op0, op1;
     enum machine_mode mode;
     int unsignedp;
     int normalizep;
{
  rtx tem, label;

  /* First see if emit_store_flag can do the job.  */
  tem = emit_store_flag (target, code, op0, op1, mode, unsignedp, normalizep);
  if (tem != 0)
    return tem;

  if (normalizep == 0)
    normalizep = 1;

  /* If this failed, we have to do this with set/compare/jump/set code.  */

  if (GET_CODE (target) != REG
      || reg_mentioned_p (target, op0) || reg_mentioned_p (target, op1))
    target = gen_reg_rtx (GET_MODE (target));

  emit_move_insn (target, const1_rtx);
  tem = compare_from_rtx (op0, op1, code, unsignedp, mode, NULL_RTX, 0);
  if (GET_CODE (tem) == CONST_INT)
    return tem;

  label = gen_label_rtx ();
  if (bcc_gen_fctn[(int) code] == 0)
    abort ();

  emit_jump_insn ((*bcc_gen_fctn[(int) code]) (label));
  emit_move_insn (target, const0_rtx);
  emit_label (label);

  return target;
}

/* Perform possibly multi-word comparison and conditional jump to LABEL
   if ARG1 OP ARG2 true where ARG1 and ARG2 are of mode MODE

   The algorithm is based on the code in expr.c:do_jump.

   Note that this does not perform a general comparison.  Only variants
   generated within expmed.c are correctly handled, others abort (but could
   be handled if needed).  */

static void
do_cmp_and_jump (arg1, arg2, op, mode, label)
     rtx arg1, arg2, label;
    enum rtx_code op;
    enum machine_mode mode;
{
  /* If this mode is an integer too wide to compare properly,
     compare word by word.  Rely on cse to optimize constant cases.  */

  if (GET_MODE_CLASS (mode) == MODE_INT && !can_compare_p (mode))
    {
      rtx label2 = gen_label_rtx ();

      switch (op)
	{
	case LTU:
	  do_jump_by_parts_greater_rtx (mode, 1, arg2, arg1, label2, label);
	  break;

	case LEU:
	  do_jump_by_parts_greater_rtx (mode, 1, arg1, arg2, label, label2);
	  break;

	case LT:
	  do_jump_by_parts_greater_rtx (mode, 0, arg2, arg1, label2, label);
	  break;

	case GT:
	  do_jump_by_parts_greater_rtx (mode, 0, arg1, arg2, label2, label);
	  break;

	case GE:
	  do_jump_by_parts_greater_rtx (mode, 0, arg2, arg1, label, label2);
	  break;

	  /* do_jump_by_parts_equality_rtx compares with zero.  Luckily
	     that's the only equality operations we do */
	case EQ:
	  if (arg2 != const0_rtx || mode != GET_MODE(arg1))
	    abort();
	  do_jump_by_parts_equality_rtx (arg1, label2, label);
	  break;

	case NE:
	  if (arg2 != const0_rtx || mode != GET_MODE(arg1))
	    abort();
	  do_jump_by_parts_equality_rtx (arg1, label, label2);
	  break;

	default:
	  abort();
	}

      emit_label (label2);
    }
  else
    {
      emit_cmp_and_jump_insns (arg1, arg2, op, NULL_RTX, mode, 0, 0, label);
    }
}
