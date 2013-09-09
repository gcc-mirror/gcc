/* Medium-level subroutines: convert bit-field store and extract
   and shifts, multiplies and divides to rtl instructions.
   Copyright (C) 1987-2013 Free Software Foundation, Inc.

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


#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "diagnostic-core.h"
#include "rtl.h"
#include "tree.h"
#include "tm_p.h"
#include "flags.h"
#include "insn-config.h"
#include "expr.h"
#include "optabs.h"
#include "recog.h"
#include "langhooks.h"
#include "df.h"
#include "target.h"
#include "expmed.h"

struct target_expmed default_target_expmed;
#if SWITCHABLE_TARGET
struct target_expmed *this_target_expmed = &default_target_expmed;
#endif

static void store_fixed_bit_field (rtx, unsigned HOST_WIDE_INT,
				   unsigned HOST_WIDE_INT,
				   unsigned HOST_WIDE_INT,
				   unsigned HOST_WIDE_INT,
				   rtx);
static void store_split_bit_field (rtx, unsigned HOST_WIDE_INT,
				   unsigned HOST_WIDE_INT,
				   unsigned HOST_WIDE_INT,
				   unsigned HOST_WIDE_INT,
				   rtx);
static rtx extract_fixed_bit_field (enum machine_mode, rtx,
				    unsigned HOST_WIDE_INT,
				    unsigned HOST_WIDE_INT, rtx, int, bool);
static rtx mask_rtx (enum machine_mode, int, int, int);
static rtx lshift_value (enum machine_mode, rtx, int, int);
static rtx extract_split_bit_field (rtx, unsigned HOST_WIDE_INT,
				    unsigned HOST_WIDE_INT, int);
static void do_cmp_and_jump (rtx, rtx, enum rtx_code, enum machine_mode, rtx);
static rtx expand_smod_pow2 (enum machine_mode, rtx, HOST_WIDE_INT);
static rtx expand_sdiv_pow2 (enum machine_mode, rtx, HOST_WIDE_INT);

/* Test whether a value is zero of a power of two.  */
#define EXACT_POWER_OF_2_OR_ZERO_P(x) \
  (((x) & ((x) - (unsigned HOST_WIDE_INT) 1)) == 0)

struct init_expmed_rtl
{
  struct rtx_def reg;
  struct rtx_def plus;
  struct rtx_def neg;
  struct rtx_def mult;
  struct rtx_def sdiv;
  struct rtx_def udiv;
  struct rtx_def sdiv_32;
  struct rtx_def smod_32;
  struct rtx_def wide_mult;
  struct rtx_def wide_lshr;
  struct rtx_def wide_trunc;
  struct rtx_def shift;
  struct rtx_def shift_mult;
  struct rtx_def shift_add;
  struct rtx_def shift_sub0;
  struct rtx_def shift_sub1;
  struct rtx_def zext;
  struct rtx_def trunc;

  rtx pow2[MAX_BITS_PER_WORD];
  rtx cint[MAX_BITS_PER_WORD];
};

static void
init_expmed_one_conv (struct init_expmed_rtl *all, enum machine_mode to_mode,
		      enum machine_mode from_mode, bool speed)
{
  int to_size, from_size;
  rtx which;

  /* We're given no information about the true size of a partial integer,
     only the size of the "full" integer it requires for storage.  For
     comparison purposes here, reduce the bit size by one in that case.  */
  to_size = (GET_MODE_BITSIZE (to_mode)
	     - (GET_MODE_CLASS (to_mode) == MODE_PARTIAL_INT));
  from_size = (GET_MODE_BITSIZE (from_mode)
	       - (GET_MODE_CLASS (from_mode) == MODE_PARTIAL_INT));
  
  /* Assume cost of zero-extend and sign-extend is the same.  */
  which = (to_size < from_size ? &all->trunc : &all->zext);

  PUT_MODE (&all->reg, from_mode);
  set_convert_cost (to_mode, from_mode, speed, set_src_cost (which, speed));
}

static void
init_expmed_one_mode (struct init_expmed_rtl *all,
		      enum machine_mode mode, int speed)
{
  int m, n, mode_bitsize;
  enum machine_mode mode_from;

  mode_bitsize = GET_MODE_UNIT_BITSIZE (mode);

  PUT_MODE (&all->reg, mode);
  PUT_MODE (&all->plus, mode);
  PUT_MODE (&all->neg, mode);
  PUT_MODE (&all->mult, mode);
  PUT_MODE (&all->sdiv, mode);
  PUT_MODE (&all->udiv, mode);
  PUT_MODE (&all->sdiv_32, mode);
  PUT_MODE (&all->smod_32, mode);
  PUT_MODE (&all->wide_trunc, mode);
  PUT_MODE (&all->shift, mode);
  PUT_MODE (&all->shift_mult, mode);
  PUT_MODE (&all->shift_add, mode);
  PUT_MODE (&all->shift_sub0, mode);
  PUT_MODE (&all->shift_sub1, mode);
  PUT_MODE (&all->zext, mode);
  PUT_MODE (&all->trunc, mode);

  set_add_cost (speed, mode, set_src_cost (&all->plus, speed));
  set_neg_cost (speed, mode, set_src_cost (&all->neg, speed));
  set_mul_cost (speed, mode, set_src_cost (&all->mult, speed));
  set_sdiv_cost (speed, mode, set_src_cost (&all->sdiv, speed));
  set_udiv_cost (speed, mode, set_src_cost (&all->udiv, speed));

  set_sdiv_pow2_cheap (speed, mode, (set_src_cost (&all->sdiv_32, speed)
				     <= 2 * add_cost (speed, mode)));
  set_smod_pow2_cheap (speed, mode, (set_src_cost (&all->smod_32, speed)
				     <= 4 * add_cost (speed, mode)));

  set_shift_cost (speed, mode, 0, 0);
  {
    int cost = add_cost (speed, mode);
    set_shiftadd_cost (speed, mode, 0, cost);
    set_shiftsub0_cost (speed, mode, 0, cost);
    set_shiftsub1_cost (speed, mode, 0, cost);
  }

  n = MIN (MAX_BITS_PER_WORD, mode_bitsize);
  for (m = 1; m < n; m++)
    {
      XEXP (&all->shift, 1) = all->cint[m];
      XEXP (&all->shift_mult, 1) = all->pow2[m];

      set_shift_cost (speed, mode, m, set_src_cost (&all->shift, speed));
      set_shiftadd_cost (speed, mode, m, set_src_cost (&all->shift_add, speed));
      set_shiftsub0_cost (speed, mode, m, set_src_cost (&all->shift_sub0, speed));
      set_shiftsub1_cost (speed, mode, m, set_src_cost (&all->shift_sub1, speed));
    }

  if (SCALAR_INT_MODE_P (mode))
    {
      for (mode_from = MIN_MODE_INT; mode_from <= MAX_MODE_INT;
	   mode_from = (enum machine_mode)(mode_from + 1))
	init_expmed_one_conv (all, mode, mode_from, speed);
    }
  if (GET_MODE_CLASS (mode) == MODE_INT)
    {
      enum machine_mode  wider_mode = GET_MODE_WIDER_MODE (mode);
      if (wider_mode != VOIDmode)
	{
	  PUT_MODE (&all->zext, wider_mode);
	  PUT_MODE (&all->wide_mult, wider_mode);
	  PUT_MODE (&all->wide_lshr, wider_mode);
	  XEXP (&all->wide_lshr, 1) = GEN_INT (mode_bitsize);

	  set_mul_widen_cost (speed, wider_mode,
			      set_src_cost (&all->wide_mult, speed));
	  set_mul_highpart_cost (speed, mode,
				 set_src_cost (&all->wide_trunc, speed));
	}
    }
}

void
init_expmed (void)
{
  struct init_expmed_rtl all;
  enum machine_mode mode;
  int m, speed;

  memset (&all, 0, sizeof all);
  for (m = 1; m < MAX_BITS_PER_WORD; m++)
    {
      all.pow2[m] = GEN_INT ((HOST_WIDE_INT) 1 << m);
      all.cint[m] = GEN_INT (m);
    }

  PUT_CODE (&all.reg, REG);
  /* Avoid using hard regs in ways which may be unsupported.  */
  SET_REGNO (&all.reg, LAST_VIRTUAL_REGISTER + 1);

  PUT_CODE (&all.plus, PLUS);
  XEXP (&all.plus, 0) = &all.reg;
  XEXP (&all.plus, 1) = &all.reg;

  PUT_CODE (&all.neg, NEG);
  XEXP (&all.neg, 0) = &all.reg;

  PUT_CODE (&all.mult, MULT);
  XEXP (&all.mult, 0) = &all.reg;
  XEXP (&all.mult, 1) = &all.reg;

  PUT_CODE (&all.sdiv, DIV);
  XEXP (&all.sdiv, 0) = &all.reg;
  XEXP (&all.sdiv, 1) = &all.reg;

  PUT_CODE (&all.udiv, UDIV);
  XEXP (&all.udiv, 0) = &all.reg;
  XEXP (&all.udiv, 1) = &all.reg;

  PUT_CODE (&all.sdiv_32, DIV);
  XEXP (&all.sdiv_32, 0) = &all.reg;
  XEXP (&all.sdiv_32, 1) = 32 < MAX_BITS_PER_WORD ? all.cint[32] : GEN_INT (32);

  PUT_CODE (&all.smod_32, MOD);
  XEXP (&all.smod_32, 0) = &all.reg;
  XEXP (&all.smod_32, 1) = XEXP (&all.sdiv_32, 1);

  PUT_CODE (&all.zext, ZERO_EXTEND);
  XEXP (&all.zext, 0) = &all.reg;

  PUT_CODE (&all.wide_mult, MULT);
  XEXP (&all.wide_mult, 0) = &all.zext;
  XEXP (&all.wide_mult, 1) = &all.zext;

  PUT_CODE (&all.wide_lshr, LSHIFTRT);
  XEXP (&all.wide_lshr, 0) = &all.wide_mult;

  PUT_CODE (&all.wide_trunc, TRUNCATE);
  XEXP (&all.wide_trunc, 0) = &all.wide_lshr;

  PUT_CODE (&all.shift, ASHIFT);
  XEXP (&all.shift, 0) = &all.reg;

  PUT_CODE (&all.shift_mult, MULT);
  XEXP (&all.shift_mult, 0) = &all.reg;

  PUT_CODE (&all.shift_add, PLUS);
  XEXP (&all.shift_add, 0) = &all.shift_mult;
  XEXP (&all.shift_add, 1) = &all.reg;

  PUT_CODE (&all.shift_sub0, MINUS);
  XEXP (&all.shift_sub0, 0) = &all.shift_mult;
  XEXP (&all.shift_sub0, 1) = &all.reg;

  PUT_CODE (&all.shift_sub1, MINUS);
  XEXP (&all.shift_sub1, 0) = &all.reg;
  XEXP (&all.shift_sub1, 1) = &all.shift_mult;

  PUT_CODE (&all.trunc, TRUNCATE);
  XEXP (&all.trunc, 0) = &all.reg;

  for (speed = 0; speed < 2; speed++)
    {
      crtl->maybe_hot_insn_p = speed;
      set_zero_cost (speed, set_src_cost (const0_rtx, speed));

      for (mode = MIN_MODE_INT; mode <= MAX_MODE_INT;
	   mode = (enum machine_mode)(mode + 1))
	init_expmed_one_mode (&all, mode, speed);

      if (MIN_MODE_PARTIAL_INT != VOIDmode)
	for (mode = MIN_MODE_PARTIAL_INT; mode <= MAX_MODE_PARTIAL_INT;
	     mode = (enum machine_mode)(mode + 1))
	  init_expmed_one_mode (&all, mode, speed);

      if (MIN_MODE_VECTOR_INT != VOIDmode)
	for (mode = MIN_MODE_VECTOR_INT; mode <= MAX_MODE_VECTOR_INT;
	     mode = (enum machine_mode)(mode + 1))
	  init_expmed_one_mode (&all, mode, speed);
    }

  if (alg_hash_used_p ())
    {
      struct alg_hash_entry *p = alg_hash_entry_ptr (0);
      memset (p, 0, sizeof (*p) * NUM_ALG_HASH_ENTRIES);
    }
  else
    set_alg_hash_used_p (true);
  default_rtl_profile ();
}

/* Return an rtx representing minus the value of X.
   MODE is the intended mode of the result,
   useful if X is a CONST_INT.  */

rtx
negate_rtx (enum machine_mode mode, rtx x)
{
  rtx result = simplify_unary_operation (NEG, mode, x, mode);

  if (result == 0)
    result = expand_unop (mode, neg_optab, x, NULL_RTX, 0);

  return result;
}

/* Adjust bitfield memory MEM so that it points to the first unit of mode
   MODE that contains a bitfield of size BITSIZE at bit position BITNUM.
   If MODE is BLKmode, return a reference to every byte in the bitfield.
   Set *NEW_BITNUM to the bit position of the field within the new memory.  */

static rtx
narrow_bit_field_mem (rtx mem, enum machine_mode mode,
		      unsigned HOST_WIDE_INT bitsize,
		      unsigned HOST_WIDE_INT bitnum,
		      unsigned HOST_WIDE_INT *new_bitnum)
{
  if (mode == BLKmode)
    {
      *new_bitnum = bitnum % BITS_PER_UNIT;
      HOST_WIDE_INT offset = bitnum / BITS_PER_UNIT;
      HOST_WIDE_INT size = ((*new_bitnum + bitsize + BITS_PER_UNIT - 1)
			    / BITS_PER_UNIT);
      return adjust_bitfield_address_size (mem, mode, offset, size);
    }
  else
    {
      unsigned int unit = GET_MODE_BITSIZE (mode);
      *new_bitnum = bitnum % unit;
      HOST_WIDE_INT offset = (bitnum - *new_bitnum) / BITS_PER_UNIT;
      return adjust_bitfield_address (mem, mode, offset);
    }
}

/* The caller wants to perform insertion or extraction PATTERN on a
   bitfield of size BITSIZE at BITNUM bits into memory operand OP0.
   BITREGION_START and BITREGION_END are as for store_bit_field
   and FIELDMODE is the natural mode of the field.

   Search for a mode that is compatible with the memory access
   restrictions and (where applicable) with a register insertion or
   extraction.  Return the new memory on success, storing the adjusted
   bit position in *NEW_BITNUM.  Return null otherwise.  */

static rtx
adjust_bit_field_mem_for_reg (enum extraction_pattern pattern,
			      rtx op0, HOST_WIDE_INT bitsize,
			      HOST_WIDE_INT bitnum,
			      unsigned HOST_WIDE_INT bitregion_start,
			      unsigned HOST_WIDE_INT bitregion_end,
			      enum machine_mode fieldmode,
			      unsigned HOST_WIDE_INT *new_bitnum)
{
  bit_field_mode_iterator iter (bitsize, bitnum, bitregion_start,
				bitregion_end, MEM_ALIGN (op0),
				MEM_VOLATILE_P (op0));
  enum machine_mode best_mode;
  if (iter.next_mode (&best_mode))
    {
      /* We can use a memory in BEST_MODE.  See whether this is true for
	 any wider modes.  All other things being equal, we prefer to
	 use the widest mode possible because it tends to expose more
	 CSE opportunities.  */
      if (!iter.prefer_smaller_modes ())
	{
	  /* Limit the search to the mode required by the corresponding
	     register insertion or extraction instruction, if any.  */
	  enum machine_mode limit_mode = word_mode;
	  extraction_insn insn;
	  if (get_best_reg_extraction_insn (&insn, pattern,
					    GET_MODE_BITSIZE (best_mode),
					    fieldmode))
	    limit_mode = insn.field_mode;

	  enum machine_mode wider_mode;
	  while (iter.next_mode (&wider_mode)
		 && GET_MODE_SIZE (wider_mode) <= GET_MODE_SIZE (limit_mode))
	    best_mode = wider_mode;
	}
      return narrow_bit_field_mem (op0, best_mode, bitsize, bitnum,
				   new_bitnum);
    }
  return NULL_RTX;
}

/* Return true if a bitfield of size BITSIZE at bit number BITNUM within
   a structure of mode STRUCT_MODE represents a lowpart subreg.   The subreg
   offset is then BITNUM / BITS_PER_UNIT.  */

static bool
lowpart_bit_field_p (unsigned HOST_WIDE_INT bitnum,
		     unsigned HOST_WIDE_INT bitsize,
		     enum machine_mode struct_mode)
{
  if (BYTES_BIG_ENDIAN)
    return (bitnum % BITS_PER_UNIT == 0
	    && (bitnum + bitsize == GET_MODE_BITSIZE (struct_mode)
		|| (bitnum + bitsize) % BITS_PER_WORD == 0));
  else
    return bitnum % BITS_PER_WORD == 0;
}

/* Return true if OP is a memory and if a bitfield of size BITSIZE at
   bit number BITNUM can be treated as a simple value of mode MODE.  */

static bool
simple_mem_bitfield_p (rtx op0, unsigned HOST_WIDE_INT bitsize,
		       unsigned HOST_WIDE_INT bitnum, enum machine_mode mode)
{
  return (MEM_P (op0)
	  && bitnum % BITS_PER_UNIT == 0
	  && bitsize == GET_MODE_BITSIZE (mode)
	  && (!SLOW_UNALIGNED_ACCESS (mode, MEM_ALIGN (op0))
	      || (bitnum % GET_MODE_ALIGNMENT (mode) == 0
		  && MEM_ALIGN (op0) >= GET_MODE_ALIGNMENT (mode))));
}

/* Try to use instruction INSV to store VALUE into a field of OP0.
   BITSIZE and BITNUM are as for store_bit_field.  */

static bool
store_bit_field_using_insv (const extraction_insn *insv, rtx op0,
			    unsigned HOST_WIDE_INT bitsize,
			    unsigned HOST_WIDE_INT bitnum, rtx value)
{
  struct expand_operand ops[4];
  rtx value1;
  rtx xop0 = op0;
  rtx last = get_last_insn ();
  bool copy_back = false;

  enum machine_mode op_mode = insv->field_mode;
  unsigned int unit = GET_MODE_BITSIZE (op_mode);
  if (bitsize == 0 || bitsize > unit)
    return false;

  if (MEM_P (xop0))
    /* Get a reference to the first byte of the field.  */
    xop0 = narrow_bit_field_mem (xop0, insv->struct_mode, bitsize, bitnum,
				 &bitnum);
  else
    {
      /* Convert from counting within OP0 to counting in OP_MODE.  */
      if (BYTES_BIG_ENDIAN)
	bitnum += unit - GET_MODE_BITSIZE (GET_MODE (op0));

      /* If xop0 is a register, we need it in OP_MODE
	 to make it acceptable to the format of insv.  */
      if (GET_CODE (xop0) == SUBREG)
	/* We can't just change the mode, because this might clobber op0,
	   and we will need the original value of op0 if insv fails.  */
	xop0 = gen_rtx_SUBREG (op_mode, SUBREG_REG (xop0), SUBREG_BYTE (xop0));
      if (REG_P (xop0) && GET_MODE (xop0) != op_mode)
	xop0 = gen_lowpart_SUBREG (op_mode, xop0);
    }

  /* If the destination is a paradoxical subreg such that we need a
     truncate to the inner mode, perform the insertion on a temporary and
     truncate the result to the original destination.  Note that we can't
     just truncate the paradoxical subreg as (truncate:N (subreg:W (reg:N
     X) 0)) is (reg:N X).  */
  if (GET_CODE (xop0) == SUBREG
      && REG_P (SUBREG_REG (xop0))
      && !TRULY_NOOP_TRUNCATION_MODES_P (GET_MODE (SUBREG_REG (xop0)),
					 op_mode))
    {
      rtx tem = gen_reg_rtx (op_mode);
      emit_move_insn (tem, xop0);
      xop0 = tem;
      copy_back = true;
    }

  /* If BITS_BIG_ENDIAN is zero on a BYTES_BIG_ENDIAN machine, we count
     "backwards" from the size of the unit we are inserting into.
     Otherwise, we count bits from the most significant on a
     BYTES/BITS_BIG_ENDIAN machine.  */

  if (BITS_BIG_ENDIAN != BYTES_BIG_ENDIAN)
    bitnum = unit - bitsize - bitnum;

  /* Convert VALUE to op_mode (which insv insn wants) in VALUE1.  */
  value1 = value;
  if (GET_MODE (value) != op_mode)
    {
      if (GET_MODE_BITSIZE (GET_MODE (value)) >= bitsize)
	{
	  /* Optimization: Don't bother really extending VALUE
	     if it has all the bits we will actually use.  However,
	     if we must narrow it, be sure we do it correctly.  */

	  if (GET_MODE_SIZE (GET_MODE (value)) < GET_MODE_SIZE (op_mode))
	    {
	      rtx tmp;

	      tmp = simplify_subreg (op_mode, value1, GET_MODE (value), 0);
	      if (! tmp)
		tmp = simplify_gen_subreg (op_mode,
					   force_reg (GET_MODE (value),
						      value1),
					   GET_MODE (value), 0);
	      value1 = tmp;
	    }
	  else
	    value1 = gen_lowpart (op_mode, value1);
	}
      else if (CONST_INT_P (value))
	value1 = gen_int_mode (INTVAL (value), op_mode);
      else
	/* Parse phase is supposed to make VALUE's data type
	   match that of the component reference, which is a type
	   at least as wide as the field; so VALUE should have
	   a mode that corresponds to that type.  */
	gcc_assert (CONSTANT_P (value));
    }

  create_fixed_operand (&ops[0], xop0);
  create_integer_operand (&ops[1], bitsize);
  create_integer_operand (&ops[2], bitnum);
  create_input_operand (&ops[3], value1, op_mode);
  if (maybe_expand_insn (insv->icode, 4, ops))
    {
      if (copy_back)
	convert_move (op0, xop0, true);
      return true;
    }
  delete_insns_since (last);
  return false;
}

/* A subroutine of store_bit_field, with the same arguments.  Return true
   if the operation could be implemented.

   If FALLBACK_P is true, fall back to store_fixed_bit_field if we have
   no other way of implementing the operation.  If FALLBACK_P is false,
   return false instead.  */

static bool
store_bit_field_1 (rtx str_rtx, unsigned HOST_WIDE_INT bitsize,
		   unsigned HOST_WIDE_INT bitnum,
		   unsigned HOST_WIDE_INT bitregion_start,
		   unsigned HOST_WIDE_INT bitregion_end,
		   enum machine_mode fieldmode,
		   rtx value, bool fallback_p)
{
  rtx op0 = str_rtx;
  rtx orig_value;

  while (GET_CODE (op0) == SUBREG)
    {
      /* The following line once was done only if WORDS_BIG_ENDIAN,
	 but I think that is a mistake.  WORDS_BIG_ENDIAN is
	 meaningful at a much higher level; when structures are copied
	 between memory and regs, the higher-numbered regs
	 always get higher addresses.  */
      int inner_mode_size = GET_MODE_SIZE (GET_MODE (SUBREG_REG (op0)));
      int outer_mode_size = GET_MODE_SIZE (GET_MODE (op0));
      int byte_offset = 0;

      /* Paradoxical subregs need special handling on big endian machines.  */
      if (SUBREG_BYTE (op0) == 0 && inner_mode_size < outer_mode_size)
	{
	  int difference = inner_mode_size - outer_mode_size;

	  if (WORDS_BIG_ENDIAN)
	    byte_offset += (difference / UNITS_PER_WORD) * UNITS_PER_WORD;
	  if (BYTES_BIG_ENDIAN)
	    byte_offset += difference % UNITS_PER_WORD;
	}
      else
	byte_offset = SUBREG_BYTE (op0);

      bitnum += byte_offset * BITS_PER_UNIT;
      op0 = SUBREG_REG (op0);
    }

  /* No action is needed if the target is a register and if the field
     lies completely outside that register.  This can occur if the source
     code contains an out-of-bounds access to a small array.  */
  if (REG_P (op0) && bitnum >= GET_MODE_BITSIZE (GET_MODE (op0)))
    return true;

  /* Use vec_set patterns for inserting parts of vectors whenever
     available.  */
  if (VECTOR_MODE_P (GET_MODE (op0))
      && !MEM_P (op0)
      && optab_handler (vec_set_optab, GET_MODE (op0)) != CODE_FOR_nothing
      && fieldmode == GET_MODE_INNER (GET_MODE (op0))
      && bitsize == GET_MODE_BITSIZE (GET_MODE_INNER (GET_MODE (op0)))
      && !(bitnum % GET_MODE_BITSIZE (GET_MODE_INNER (GET_MODE (op0)))))
    {
      struct expand_operand ops[3];
      enum machine_mode outermode = GET_MODE (op0);
      enum machine_mode innermode = GET_MODE_INNER (outermode);
      enum insn_code icode = optab_handler (vec_set_optab, outermode);
      int pos = bitnum / GET_MODE_BITSIZE (innermode);

      create_fixed_operand (&ops[0], op0);
      create_input_operand (&ops[1], value, innermode);
      create_integer_operand (&ops[2], pos);
      if (maybe_expand_insn (icode, 3, ops))
	return true;
    }

  /* If the target is a register, overwriting the entire object, or storing
     a full-word or multi-word field can be done with just a SUBREG.  */
  if (!MEM_P (op0)
      && bitsize == GET_MODE_BITSIZE (fieldmode)
      && ((bitsize == GET_MODE_BITSIZE (GET_MODE (op0)) && bitnum == 0)
	  || (bitsize % BITS_PER_WORD == 0 && bitnum % BITS_PER_WORD == 0)))
    {
      /* Use the subreg machinery either to narrow OP0 to the required
	 words or to cope with mode punning between equal-sized modes.  */
      rtx sub = simplify_gen_subreg (fieldmode, op0, GET_MODE (op0),
				     bitnum / BITS_PER_UNIT);
      if (sub)
	{
	  emit_move_insn (sub, value);
	  return true;
	}
    }

  /* If the target is memory, storing any naturally aligned field can be
     done with a simple store.  For targets that support fast unaligned
     memory, any naturally sized, unit aligned field can be done directly.  */
  if (simple_mem_bitfield_p (op0, bitsize, bitnum, fieldmode))
    {
      op0 = adjust_bitfield_address (op0, fieldmode, bitnum / BITS_PER_UNIT);
      emit_move_insn (op0, value);
      return true;
    }

  /* Make sure we are playing with integral modes.  Pun with subregs
     if we aren't.  This must come after the entire register case above,
     since that case is valid for any mode.  The following cases are only
     valid for integral modes.  */
  {
    enum machine_mode imode = int_mode_for_mode (GET_MODE (op0));
    if (imode != GET_MODE (op0))
      {
	if (MEM_P (op0))
	  op0 = adjust_bitfield_address_size (op0, imode, 0, MEM_SIZE (op0));
	else
	  {
	    gcc_assert (imode != BLKmode);
	    op0 = gen_lowpart (imode, op0);
	  }
      }
  }

  /* Storing an lsb-aligned field in a register
     can be done with a movstrict instruction.  */

  if (!MEM_P (op0)
      && lowpart_bit_field_p (bitnum, bitsize, GET_MODE (op0))
      && bitsize == GET_MODE_BITSIZE (fieldmode)
      && optab_handler (movstrict_optab, fieldmode) != CODE_FOR_nothing)
    {
      struct expand_operand ops[2];
      enum insn_code icode = optab_handler (movstrict_optab, fieldmode);
      rtx arg0 = op0;
      unsigned HOST_WIDE_INT subreg_off;

      if (GET_CODE (arg0) == SUBREG)
	{
	  /* Else we've got some float mode source being extracted into
	     a different float mode destination -- this combination of
	     subregs results in Severe Tire Damage.  */
	  gcc_assert (GET_MODE (SUBREG_REG (arg0)) == fieldmode
		      || GET_MODE_CLASS (fieldmode) == MODE_INT
		      || GET_MODE_CLASS (fieldmode) == MODE_PARTIAL_INT);
	  arg0 = SUBREG_REG (arg0);
	}

      subreg_off = bitnum / BITS_PER_UNIT;
      if (validate_subreg (fieldmode, GET_MODE (arg0), arg0, subreg_off))
	{
	  arg0 = gen_rtx_SUBREG (fieldmode, arg0, subreg_off);

	  create_fixed_operand (&ops[0], arg0);
	  /* Shrink the source operand to FIELDMODE.  */
	  create_convert_operand_to (&ops[1], value, fieldmode, false);
	  if (maybe_expand_insn (icode, 2, ops))
	    return true;
	}
    }

  /* Handle fields bigger than a word.  */

  if (bitsize > BITS_PER_WORD)
    {
      /* Here we transfer the words of the field
	 in the order least significant first.
	 This is because the most significant word is the one which may
	 be less than full.
	 However, only do that if the value is not BLKmode.  */

      unsigned int backwards = WORDS_BIG_ENDIAN && fieldmode != BLKmode;
      unsigned int nwords = (bitsize + (BITS_PER_WORD - 1)) / BITS_PER_WORD;
      unsigned int i;
      rtx last;

      /* This is the mode we must force value to, so that there will be enough
	 subwords to extract.  Note that fieldmode will often (always?) be
	 VOIDmode, because that is what store_field uses to indicate that this
	 is a bit field, but passing VOIDmode to operand_subword_force
	 is not allowed.  */
      fieldmode = GET_MODE (value);
      if (fieldmode == VOIDmode)
	fieldmode = smallest_mode_for_size (nwords * BITS_PER_WORD, MODE_INT);

      last = get_last_insn ();
      for (i = 0; i < nwords; i++)
	{
	  /* If I is 0, use the low-order word in both field and target;
	     if I is 1, use the next to lowest word; and so on.  */
	  unsigned int wordnum = (backwards
				  ? GET_MODE_SIZE (fieldmode) / UNITS_PER_WORD
				  - i - 1
				  : i);
	  unsigned int bit_offset = (backwards
				     ? MAX ((int) bitsize - ((int) i + 1)
					    * BITS_PER_WORD,
					    0)
				     : (int) i * BITS_PER_WORD);
	  rtx value_word = operand_subword_force (value, wordnum, fieldmode);
	  unsigned HOST_WIDE_INT new_bitsize =
	    MIN (BITS_PER_WORD, bitsize - i * BITS_PER_WORD);

	  /* If the remaining chunk doesn't have full wordsize we have
	     to make sure that for big endian machines the higher order
	     bits are used.  */
	  if (new_bitsize < BITS_PER_WORD && BYTES_BIG_ENDIAN && !backwards)
	    value_word = simplify_expand_binop (word_mode, lshr_optab,
						value_word,
						GEN_INT (BITS_PER_WORD
							 - new_bitsize),
						NULL_RTX, true,
						OPTAB_LIB_WIDEN);

	  if (!store_bit_field_1 (op0, new_bitsize,
				  bitnum + bit_offset,
				  bitregion_start, bitregion_end,
				  word_mode,
				  value_word, fallback_p))
	    {
	      delete_insns_since (last);
	      return false;
	    }
	}
      return true;
    }

  /* If VALUE has a floating-point or complex mode, access it as an
     integer of the corresponding size.  This can occur on a machine
     with 64 bit registers that uses SFmode for float.  It can also
     occur for unaligned float or complex fields.  */
  orig_value = value;
  if (GET_MODE (value) != VOIDmode
      && GET_MODE_CLASS (GET_MODE (value)) != MODE_INT
      && GET_MODE_CLASS (GET_MODE (value)) != MODE_PARTIAL_INT)
    {
      value = gen_reg_rtx (int_mode_for_mode (GET_MODE (value)));
      emit_move_insn (gen_lowpart (GET_MODE (orig_value), value), orig_value);
    }

  /* If OP0 is a multi-word register, narrow it to the affected word.
     If the region spans two words, defer to store_split_bit_field.  */
  if (!MEM_P (op0) && GET_MODE_SIZE (GET_MODE (op0)) > UNITS_PER_WORD)
    {
      op0 = simplify_gen_subreg (word_mode, op0, GET_MODE (op0),
				 bitnum / BITS_PER_WORD * UNITS_PER_WORD);
      gcc_assert (op0);
      bitnum %= BITS_PER_WORD;
      if (bitnum + bitsize > BITS_PER_WORD)
	{
	  if (!fallback_p)
	    return false;

	  store_split_bit_field (op0, bitsize, bitnum, bitregion_start,
				 bitregion_end, value);
	  return true;
	}
    }

  /* From here on we can assume that the field to be stored in fits
     within a word.  If the destination is a register, it too fits
     in a word.  */

  extraction_insn insv;
  if (!MEM_P (op0)
      && get_best_reg_extraction_insn (&insv, EP_insv,
				       GET_MODE_BITSIZE (GET_MODE (op0)),
				       fieldmode)
      && store_bit_field_using_insv (&insv, op0, bitsize, bitnum, value))
    return true;

  /* If OP0 is a memory, try copying it to a register and seeing if a
     cheap register alternative is available.  */
  if (MEM_P (op0))
    {
      /* Do not use unaligned memory insvs for volatile bitfields when
	 -fstrict-volatile-bitfields is in effect.  */
      if (!(MEM_VOLATILE_P (op0)
	    && flag_strict_volatile_bitfields > 0)
	  && get_best_mem_extraction_insn (&insv, EP_insv, bitsize, bitnum,
					   fieldmode)
	  && store_bit_field_using_insv (&insv, op0, bitsize, bitnum, value))
	return true;

      rtx last = get_last_insn ();

      /* Try loading part of OP0 into a register, inserting the bitfield
	 into that, and then copying the result back to OP0.  */
      unsigned HOST_WIDE_INT bitpos;
      rtx xop0 = adjust_bit_field_mem_for_reg (EP_insv, op0, bitsize, bitnum,
					       bitregion_start, bitregion_end,
					       fieldmode, &bitpos);
      if (xop0)
	{
	  rtx tempreg = copy_to_reg (xop0);
	  if (store_bit_field_1 (tempreg, bitsize, bitpos,
				 bitregion_start, bitregion_end,
				 fieldmode, orig_value, false))
	    {
	      emit_move_insn (xop0, tempreg);
	      return true;
	    }
	  delete_insns_since (last);
	}
    }

  if (!fallback_p)
    return false;

  store_fixed_bit_field (op0, bitsize, bitnum, bitregion_start,
			 bitregion_end, value);
  return true;
}

/* Generate code to store value from rtx VALUE
   into a bit-field within structure STR_RTX
   containing BITSIZE bits starting at bit BITNUM.

   BITREGION_START is bitpos of the first bitfield in this region.
   BITREGION_END is the bitpos of the ending bitfield in this region.
   These two fields are 0, if the C++ memory model does not apply,
   or we are not interested in keeping track of bitfield regions.

   FIELDMODE is the machine-mode of the FIELD_DECL node for this field.  */

void
store_bit_field (rtx str_rtx, unsigned HOST_WIDE_INT bitsize,
		 unsigned HOST_WIDE_INT bitnum,
		 unsigned HOST_WIDE_INT bitregion_start,
		 unsigned HOST_WIDE_INT bitregion_end,
		 enum machine_mode fieldmode,
		 rtx value)
{
  /* Under the C++0x memory model, we must not touch bits outside the
     bit region.  Adjust the address to start at the beginning of the
     bit region.  */
  if (MEM_P (str_rtx) && bitregion_start > 0)
    {
      enum machine_mode bestmode;
      HOST_WIDE_INT offset, size;

      gcc_assert ((bitregion_start % BITS_PER_UNIT) == 0);

      offset = bitregion_start / BITS_PER_UNIT;
      bitnum -= bitregion_start;
      size = (bitnum + bitsize + BITS_PER_UNIT - 1) / BITS_PER_UNIT;
      bitregion_end -= bitregion_start;
      bitregion_start = 0;
      bestmode = get_best_mode (bitsize, bitnum,
				bitregion_start, bitregion_end,
				MEM_ALIGN (str_rtx), VOIDmode,
				MEM_VOLATILE_P (str_rtx));
      str_rtx = adjust_bitfield_address_size (str_rtx, bestmode, offset, size);
    }

  if (!store_bit_field_1 (str_rtx, bitsize, bitnum,
			  bitregion_start, bitregion_end,
			  fieldmode, value, true))
    gcc_unreachable ();
}

/* Use shifts and boolean operations to store VALUE into a bit field of
   width BITSIZE in OP0, starting at bit BITNUM.  */

static void
store_fixed_bit_field (rtx op0, unsigned HOST_WIDE_INT bitsize,
		       unsigned HOST_WIDE_INT bitnum,
		       unsigned HOST_WIDE_INT bitregion_start,
		       unsigned HOST_WIDE_INT bitregion_end,
		       rtx value)
{
  enum machine_mode mode;
  rtx temp;
  int all_zero = 0;
  int all_one = 0;

  /* There is a case not handled here:
     a structure with a known alignment of just a halfword
     and a field split across two aligned halfwords within the structure.
     Or likewise a structure with a known alignment of just a byte
     and a field split across two bytes.
     Such cases are not supposed to be able to occur.  */

  if (MEM_P (op0))
    {
      unsigned HOST_WIDE_INT maxbits = MAX_FIXED_MODE_SIZE;

      if (bitregion_end)
	maxbits = bitregion_end - bitregion_start + 1;

      /* Get the proper mode to use for this field.  We want a mode that
	 includes the entire field.  If such a mode would be larger than
	 a word, we won't be doing the extraction the normal way.
	 We don't want a mode bigger than the destination.  */

      mode = GET_MODE (op0);
      if (GET_MODE_BITSIZE (mode) == 0
	  || GET_MODE_BITSIZE (mode) > GET_MODE_BITSIZE (word_mode))
	mode = word_mode;

      if (MEM_VOLATILE_P (op0)
          && GET_MODE_BITSIZE (GET_MODE (op0)) > 0
	  && GET_MODE_BITSIZE (GET_MODE (op0)) <= maxbits
	  && flag_strict_volatile_bitfields > 0)
	mode = GET_MODE (op0);
      else
	mode = get_best_mode (bitsize, bitnum, bitregion_start, bitregion_end,
			      MEM_ALIGN (op0), mode, MEM_VOLATILE_P (op0));

      if (mode == VOIDmode)
	{
	  /* The only way this should occur is if the field spans word
	     boundaries.  */
	  store_split_bit_field (op0, bitsize, bitnum, bitregion_start,
				 bitregion_end, value);
	  return;
	}

      op0 = narrow_bit_field_mem (op0, mode, bitsize, bitnum, &bitnum);
    }

  mode = GET_MODE (op0);
  gcc_assert (SCALAR_INT_MODE_P (mode));

  /* Note that bitsize + bitnum can be greater than GET_MODE_BITSIZE (mode)
     for invalid input, such as f5 from gcc.dg/pr48335-2.c.  */

  if (BYTES_BIG_ENDIAN)
    /* BITNUM is the distance between our msb
       and that of the containing datum.
       Convert it to the distance from the lsb.  */
    bitnum = GET_MODE_BITSIZE (mode) - bitsize - bitnum;

  /* Now BITNUM is always the distance between our lsb
     and that of OP0.  */

  /* Shift VALUE left by BITNUM bits.  If VALUE is not constant,
     we must first convert its mode to MODE.  */

  if (CONST_INT_P (value))
    {
      HOST_WIDE_INT v = INTVAL (value);

      if (bitsize < HOST_BITS_PER_WIDE_INT)
	v &= ((HOST_WIDE_INT) 1 << bitsize) - 1;

      if (v == 0)
	all_zero = 1;
      else if ((bitsize < HOST_BITS_PER_WIDE_INT
		&& v == ((HOST_WIDE_INT) 1 << bitsize) - 1)
	       || (bitsize == HOST_BITS_PER_WIDE_INT && v == -1))
	all_one = 1;

      value = lshift_value (mode, value, bitnum, bitsize);
    }
  else
    {
      int must_and = (GET_MODE_BITSIZE (GET_MODE (value)) != bitsize
		      && bitnum + bitsize != GET_MODE_BITSIZE (mode));

      if (GET_MODE (value) != mode)
	value = convert_to_mode (mode, value, 1);

      if (must_and)
	value = expand_binop (mode, and_optab, value,
			      mask_rtx (mode, 0, bitsize, 0),
			      NULL_RTX, 1, OPTAB_LIB_WIDEN);
      if (bitnum > 0)
	value = expand_shift (LSHIFT_EXPR, mode, value,
			      bitnum, NULL_RTX, 1);
    }

  /* Now clear the chosen bits in OP0,
     except that if VALUE is -1 we need not bother.  */
  /* We keep the intermediates in registers to allow CSE to combine
     consecutive bitfield assignments.  */

  temp = force_reg (mode, op0);

  if (! all_one)
    {
      temp = expand_binop (mode, and_optab, temp,
			   mask_rtx (mode, bitnum, bitsize, 1),
			   NULL_RTX, 1, OPTAB_LIB_WIDEN);
      temp = force_reg (mode, temp);
    }

  /* Now logical-or VALUE into OP0, unless it is zero.  */

  if (! all_zero)
    {
      temp = expand_binop (mode, ior_optab, temp, value,
			   NULL_RTX, 1, OPTAB_LIB_WIDEN);
      temp = force_reg (mode, temp);
    }

  if (op0 != temp)
    {
      op0 = copy_rtx (op0);
      emit_move_insn (op0, temp);
    }
}

/* Store a bit field that is split across multiple accessible memory objects.

   OP0 is the REG, SUBREG or MEM rtx for the first of the objects.
   BITSIZE is the field width; BITPOS the position of its first bit
   (within the word).
   VALUE is the value to store.

   This does not yet handle fields wider than BITS_PER_WORD.  */

static void
store_split_bit_field (rtx op0, unsigned HOST_WIDE_INT bitsize,
		       unsigned HOST_WIDE_INT bitpos,
		       unsigned HOST_WIDE_INT bitregion_start,
		       unsigned HOST_WIDE_INT bitregion_end,
		       rtx value)
{
  unsigned int unit;
  unsigned int bitsdone = 0;

  /* Make sure UNIT isn't larger than BITS_PER_WORD, we can only handle that
     much at a time.  */
  if (REG_P (op0) || GET_CODE (op0) == SUBREG)
    unit = BITS_PER_WORD;
  else
    unit = MIN (MEM_ALIGN (op0), BITS_PER_WORD);

  /* If VALUE is a constant other than a CONST_INT, get it into a register in
     WORD_MODE.  If we can do this using gen_lowpart_common, do so.  Note
     that VALUE might be a floating-point constant.  */
  if (CONSTANT_P (value) && !CONST_INT_P (value))
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

  while (bitsdone < bitsize)
    {
      unsigned HOST_WIDE_INT thissize;
      rtx part, word;
      unsigned HOST_WIDE_INT thispos;
      unsigned HOST_WIDE_INT offset;

      offset = (bitpos + bitsdone) / unit;
      thispos = (bitpos + bitsdone) % unit;

      /* When region of bytes we can touch is restricted, decrease
	 UNIT close to the end of the region as needed.  If op0 is a REG
	 or SUBREG of REG, don't do this, as there can't be data races
	 on a register and we can expand shorter code in some cases.  */
      if (bitregion_end
	  && unit > BITS_PER_UNIT
	  && bitpos + bitsdone - thispos + unit > bitregion_end + 1
	  && !REG_P (op0)
	  && (GET_CODE (op0) != SUBREG || !REG_P (SUBREG_REG (op0))))
	{
	  unit = unit / 2;
	  continue;
	}

      /* THISSIZE must not overrun a word boundary.  Otherwise,
	 store_fixed_bit_field will call us again, and we will mutually
	 recurse forever.  */
      thissize = MIN (bitsize - bitsdone, BITS_PER_WORD);
      thissize = MIN (thissize, unit - thispos);

      if (BYTES_BIG_ENDIAN)
	{
	  /* Fetch successively less significant portions.  */
	  if (CONST_INT_P (value))
	    part = GEN_INT (((unsigned HOST_WIDE_INT) (INTVAL (value))
			     >> (bitsize - bitsdone - thissize))
			    & (((HOST_WIDE_INT) 1 << thissize) - 1));
	  else
	    {
	      int total_bits = GET_MODE_BITSIZE (GET_MODE (value));
	      /* The args are chosen so that the last part includes the
		 lsb.  Give extract_bit_field the value it needs (with
		 endianness compensation) to fetch the piece we want.  */
	      part = extract_fixed_bit_field (word_mode, value, thissize,
					      total_bits - bitsize + bitsdone,
					      NULL_RTX, 1, false);
	    }
	}
      else
	{
	  /* Fetch successively more significant portions.  */
	  if (CONST_INT_P (value))
	    part = GEN_INT (((unsigned HOST_WIDE_INT) (INTVAL (value))
			     >> bitsdone)
			    & (((HOST_WIDE_INT) 1 << thissize) - 1));
	  else
	    part = extract_fixed_bit_field (word_mode, value, thissize,
					    bitsdone, NULL_RTX, 1, false);
	}

      /* If OP0 is a register, then handle OFFSET here.

	 When handling multiword bitfields, extract_bit_field may pass
	 down a word_mode SUBREG of a larger REG for a bitfield that actually
	 crosses a word boundary.  Thus, for a SUBREG, we must find
	 the current word starting from the base register.  */
      if (GET_CODE (op0) == SUBREG)
	{
	  int word_offset = (SUBREG_BYTE (op0) / UNITS_PER_WORD)
			    + (offset * unit / BITS_PER_WORD);
	  enum machine_mode sub_mode = GET_MODE (SUBREG_REG (op0));
	  if (sub_mode != BLKmode && GET_MODE_SIZE (sub_mode) < UNITS_PER_WORD)
	    word = word_offset ? const0_rtx : op0;
	  else
	    word = operand_subword_force (SUBREG_REG (op0), word_offset,
					  GET_MODE (SUBREG_REG (op0)));
	  offset &= BITS_PER_WORD / unit - 1;
	}
      else if (REG_P (op0))
	{
	  enum machine_mode op0_mode = GET_MODE (op0);
	  if (op0_mode != BLKmode && GET_MODE_SIZE (op0_mode) < UNITS_PER_WORD)
	    word = offset ? const0_rtx : op0;
	  else
	    word = operand_subword_force (op0, offset * unit / BITS_PER_WORD,
					  GET_MODE (op0));
	  offset &= BITS_PER_WORD / unit - 1;
	}
      else
	word = op0;

      /* OFFSET is in UNITs, and UNIT is in bits.  If WORD is const0_rtx,
	 it is just an out-of-bounds access.  Ignore it.  */
      if (word != const0_rtx)
	store_fixed_bit_field (word, thissize, offset * unit + thispos,
			       bitregion_start, bitregion_end, part);
      bitsdone += thissize;
    }
}

/* A subroutine of extract_bit_field_1 that converts return value X
   to either MODE or TMODE.  MODE, TMODE and UNSIGNEDP are arguments
   to extract_bit_field.  */

static rtx
convert_extracted_bit_field (rtx x, enum machine_mode mode,
			     enum machine_mode tmode, bool unsignedp)
{
  if (GET_MODE (x) == tmode || GET_MODE (x) == mode)
    return x;

  /* If the x mode is not a scalar integral, first convert to the
     integer mode of that size and then access it as a floating-point
     value via a SUBREG.  */
  if (!SCALAR_INT_MODE_P (tmode))
    {
      enum machine_mode smode;

      smode = mode_for_size (GET_MODE_BITSIZE (tmode), MODE_INT, 0);
      x = convert_to_mode (smode, x, unsignedp);
      x = force_reg (smode, x);
      return gen_lowpart (tmode, x);
    }

  return convert_to_mode (tmode, x, unsignedp);
}

/* Try to use an ext(z)v pattern to extract a field from OP0.
   Return the extracted value on success, otherwise return null.
   EXT_MODE is the mode of the extraction and the other arguments
   are as for extract_bit_field.  */

static rtx
extract_bit_field_using_extv (const extraction_insn *extv, rtx op0,
			      unsigned HOST_WIDE_INT bitsize,
			      unsigned HOST_WIDE_INT bitnum,
			      int unsignedp, rtx target,
			      enum machine_mode mode, enum machine_mode tmode)
{
  struct expand_operand ops[4];
  rtx spec_target = target;
  rtx spec_target_subreg = 0;
  enum machine_mode ext_mode = extv->field_mode;
  unsigned unit = GET_MODE_BITSIZE (ext_mode);

  if (bitsize == 0 || unit < bitsize)
    return NULL_RTX;

  if (MEM_P (op0))
    /* Get a reference to the first byte of the field.  */
    op0 = narrow_bit_field_mem (op0, extv->struct_mode, bitsize, bitnum,
				&bitnum);
  else
    {
      /* Convert from counting within OP0 to counting in EXT_MODE.  */
      if (BYTES_BIG_ENDIAN)
	bitnum += unit - GET_MODE_BITSIZE (GET_MODE (op0));

      /* If op0 is a register, we need it in EXT_MODE to make it
	 acceptable to the format of ext(z)v.  */
      if (GET_CODE (op0) == SUBREG && GET_MODE (op0) != ext_mode)
	return NULL_RTX;
      if (REG_P (op0) && GET_MODE (op0) != ext_mode)
	op0 = gen_lowpart_SUBREG (ext_mode, op0);
    }

  /* If BITS_BIG_ENDIAN is zero on a BYTES_BIG_ENDIAN machine, we count
     "backwards" from the size of the unit we are extracting from.
     Otherwise, we count bits from the most significant on a
     BYTES/BITS_BIG_ENDIAN machine.  */

  if (BITS_BIG_ENDIAN != BYTES_BIG_ENDIAN)
    bitnum = unit - bitsize - bitnum;

  if (target == 0)
    target = spec_target = gen_reg_rtx (tmode);

  if (GET_MODE (target) != ext_mode)
    {
      /* Don't use LHS paradoxical subreg if explicit truncation is needed
	 between the mode of the extraction (word_mode) and the target
	 mode.  Instead, create a temporary and use convert_move to set
	 the target.  */
      if (REG_P (target)
	  && TRULY_NOOP_TRUNCATION_MODES_P (GET_MODE (target), ext_mode))
	{
	  target = gen_lowpart (ext_mode, target);
	  if (GET_MODE_PRECISION (ext_mode)
	      > GET_MODE_PRECISION (GET_MODE (spec_target)))
	    spec_target_subreg = target;
	}
      else
	target = gen_reg_rtx (ext_mode);
    }

  create_output_operand (&ops[0], target, ext_mode);
  create_fixed_operand (&ops[1], op0);
  create_integer_operand (&ops[2], bitsize);
  create_integer_operand (&ops[3], bitnum);
  if (maybe_expand_insn (extv->icode, 4, ops))
    {
      target = ops[0].value;
      if (target == spec_target)
	return target;
      if (target == spec_target_subreg)
	return spec_target;
      return convert_extracted_bit_field (target, mode, tmode, unsignedp);
    }
  return NULL_RTX;
}

/* A subroutine of extract_bit_field, with the same arguments.
   If FALLBACK_P is true, fall back to extract_fixed_bit_field
   if we can find no other means of implementing the operation.
   if FALLBACK_P is false, return NULL instead.  */

static rtx
extract_bit_field_1 (rtx str_rtx, unsigned HOST_WIDE_INT bitsize,
		     unsigned HOST_WIDE_INT bitnum,
		     int unsignedp, bool packedp, rtx target,
		     enum machine_mode mode, enum machine_mode tmode,
		     bool fallback_p)
{
  rtx op0 = str_rtx;
  enum machine_mode int_mode;
  enum machine_mode mode1;

  if (tmode == VOIDmode)
    tmode = mode;

  while (GET_CODE (op0) == SUBREG)
    {
      bitnum += SUBREG_BYTE (op0) * BITS_PER_UNIT;
      op0 = SUBREG_REG (op0);
    }

  /* If we have an out-of-bounds access to a register, just return an
     uninitialized register of the required mode.  This can occur if the
     source code contains an out-of-bounds access to a small array.  */
  if (REG_P (op0) && bitnum >= GET_MODE_BITSIZE (GET_MODE (op0)))
    return gen_reg_rtx (tmode);

  if (REG_P (op0)
      && mode == GET_MODE (op0)
      && bitnum == 0
      && bitsize == GET_MODE_BITSIZE (GET_MODE (op0)))
    {
      /* We're trying to extract a full register from itself.  */
      return op0;
    }

  /* See if we can get a better vector mode before extracting.  */
  if (VECTOR_MODE_P (GET_MODE (op0))
      && !MEM_P (op0)
      && GET_MODE_INNER (GET_MODE (op0)) != tmode)
    {
      enum machine_mode new_mode;

      if (GET_MODE_CLASS (tmode) == MODE_FLOAT)
	new_mode = MIN_MODE_VECTOR_FLOAT;
      else if (GET_MODE_CLASS (tmode) == MODE_FRACT)
	new_mode = MIN_MODE_VECTOR_FRACT;
      else if (GET_MODE_CLASS (tmode) == MODE_UFRACT)
	new_mode = MIN_MODE_VECTOR_UFRACT;
      else if (GET_MODE_CLASS (tmode) == MODE_ACCUM)
	new_mode = MIN_MODE_VECTOR_ACCUM;
      else if (GET_MODE_CLASS (tmode) == MODE_UACCUM)
	new_mode = MIN_MODE_VECTOR_UACCUM;
      else
	new_mode = MIN_MODE_VECTOR_INT;

      for (; new_mode != VOIDmode ; new_mode = GET_MODE_WIDER_MODE (new_mode))
	if (GET_MODE_SIZE (new_mode) == GET_MODE_SIZE (GET_MODE (op0))
	    && targetm.vector_mode_supported_p (new_mode))
	  break;
      if (new_mode != VOIDmode)
	op0 = gen_lowpart (new_mode, op0);
    }

  /* Use vec_extract patterns for extracting parts of vectors whenever
     available.  */
  if (VECTOR_MODE_P (GET_MODE (op0))
      && !MEM_P (op0)
      && optab_handler (vec_extract_optab, GET_MODE (op0)) != CODE_FOR_nothing
      && ((bitnum + bitsize - 1) / GET_MODE_BITSIZE (GET_MODE_INNER (GET_MODE (op0)))
	  == bitnum / GET_MODE_BITSIZE (GET_MODE_INNER (GET_MODE (op0)))))
    {
      struct expand_operand ops[3];
      enum machine_mode outermode = GET_MODE (op0);
      enum machine_mode innermode = GET_MODE_INNER (outermode);
      enum insn_code icode = optab_handler (vec_extract_optab, outermode);
      unsigned HOST_WIDE_INT pos = bitnum / GET_MODE_BITSIZE (innermode);

      create_output_operand (&ops[0], target, innermode);
      create_input_operand (&ops[1], op0, outermode);
      create_integer_operand (&ops[2], pos);
      if (maybe_expand_insn (icode, 3, ops))
	{
	  target = ops[0].value;
      	  if (GET_MODE (target) != mode)
	    return gen_lowpart (tmode, target);
	  return target;
	}
    }

  /* Make sure we are playing with integral modes.  Pun with subregs
     if we aren't.  */
  {
    enum machine_mode imode = int_mode_for_mode (GET_MODE (op0));
    if (imode != GET_MODE (op0))
      {
	if (MEM_P (op0))
	  op0 = adjust_bitfield_address_size (op0, imode, 0, MEM_SIZE (op0));
	else if (imode != BLKmode)
	  {
	    op0 = gen_lowpart (imode, op0);

	    /* If we got a SUBREG, force it into a register since we
	       aren't going to be able to do another SUBREG on it.  */
	    if (GET_CODE (op0) == SUBREG)
	      op0 = force_reg (imode, op0);
	  }
	else if (REG_P (op0))
	  {
	    rtx reg, subreg;
	    imode = smallest_mode_for_size (GET_MODE_BITSIZE (GET_MODE (op0)),
					    MODE_INT);
	    reg = gen_reg_rtx (imode);
	    subreg = gen_lowpart_SUBREG (GET_MODE (op0), reg);
	    emit_move_insn (subreg, op0);
	    op0 = reg;
	    bitnum += SUBREG_BYTE (subreg) * BITS_PER_UNIT;
	  }
	else
	  {
	    HOST_WIDE_INT size = GET_MODE_SIZE (GET_MODE (op0));
	    rtx mem = assign_stack_temp (GET_MODE (op0), size);
	    emit_move_insn (mem, op0);
	    op0 = adjust_bitfield_address_size (mem, BLKmode, 0, size);
	  }
      }
  }

  /* ??? We currently assume TARGET is at least as big as BITSIZE.
     If that's wrong, the solution is to test for it and set TARGET to 0
     if needed.  */

  /* If the bitfield is volatile, we need to make sure the access
     remains on a type-aligned boundary.  */
  if (GET_CODE (op0) == MEM
      && MEM_VOLATILE_P (op0)
      && GET_MODE_BITSIZE (GET_MODE (op0)) > 0
      && flag_strict_volatile_bitfields > 0)
    goto no_subreg_mode_swap;

  /* Only scalar integer modes can be converted via subregs.  There is an
     additional problem for FP modes here in that they can have a precision
     which is different from the size.  mode_for_size uses precision, but
     we want a mode based on the size, so we must avoid calling it for FP
     modes.  */
  mode1 = mode;
  if (SCALAR_INT_MODE_P (tmode))
    {
      enum machine_mode try_mode = mode_for_size (bitsize,
						  GET_MODE_CLASS (tmode), 0);
      if (try_mode != BLKmode)
	mode1 = try_mode;
    }
  gcc_assert (mode1 != BLKmode);

  /* Extraction of a full MODE1 value can be done with a subreg as long
     as the least significant bit of the value is the least significant
     bit of either OP0 or a word of OP0.  */
  if (!MEM_P (op0)
      && lowpart_bit_field_p (bitnum, bitsize, GET_MODE (op0))
      && bitsize == GET_MODE_BITSIZE (mode1)
      && TRULY_NOOP_TRUNCATION_MODES_P (mode1, GET_MODE (op0)))
    {
      rtx sub = simplify_gen_subreg (mode1, op0, GET_MODE (op0),
				     bitnum / BITS_PER_UNIT);
      if (sub)
	return convert_extracted_bit_field (sub, mode, tmode, unsignedp);
    }

  /* Extraction of a full MODE1 value can be done with a load as long as
     the field is on a byte boundary and is sufficiently aligned.  */
  if (simple_mem_bitfield_p (op0, bitsize, bitnum, mode1))
    {
      op0 = adjust_bitfield_address (op0, mode1, bitnum / BITS_PER_UNIT);
      return convert_extracted_bit_field (op0, mode, tmode, unsignedp);
    }

 no_subreg_mode_swap:

  /* Handle fields bigger than a word.  */

  if (bitsize > BITS_PER_WORD)
    {
      /* Here we transfer the words of the field
	 in the order least significant first.
	 This is because the most significant word is the one which may
	 be less than full.  */

      unsigned int backwards = WORDS_BIG_ENDIAN;
      unsigned int nwords = (bitsize + (BITS_PER_WORD - 1)) / BITS_PER_WORD;
      unsigned int i;
      rtx last;

      if (target == 0 || !REG_P (target) || !valid_multiword_target_p (target))
	target = gen_reg_rtx (mode);

      /* Indicate for flow that the entire target reg is being set.  */
      emit_clobber (target);

      last = get_last_insn ();
      for (i = 0; i < nwords; i++)
	{
	  /* If I is 0, use the low-order word in both field and target;
	     if I is 1, use the next to lowest word; and so on.  */
	  /* Word number in TARGET to use.  */
	  unsigned int wordnum
	    = (backwards
	       ? GET_MODE_SIZE (GET_MODE (target)) / UNITS_PER_WORD - i - 1
	       : i);
	  /* Offset from start of field in OP0.  */
	  unsigned int bit_offset = (backwards
				     ? MAX ((int) bitsize - ((int) i + 1)
					    * BITS_PER_WORD,
					    0)
				     : (int) i * BITS_PER_WORD);
	  rtx target_part = operand_subword (target, wordnum, 1, VOIDmode);
	  rtx result_part
	    = extract_bit_field_1 (op0, MIN (BITS_PER_WORD,
					     bitsize - i * BITS_PER_WORD),
				   bitnum + bit_offset, 1, false, target_part,
				   mode, word_mode, fallback_p);

	  gcc_assert (target_part);
	  if (!result_part)
	    {
	      delete_insns_since (last);
	      return NULL;
	    }

	  if (result_part != target_part)
	    emit_move_insn (target_part, result_part);
	}

      if (unsignedp)
	{
	  /* Unless we've filled TARGET, the upper regs in a multi-reg value
	     need to be zero'd out.  */
	  if (GET_MODE_SIZE (GET_MODE (target)) > nwords * UNITS_PER_WORD)
	    {
	      unsigned int i, total_words;

	      total_words = GET_MODE_SIZE (GET_MODE (target)) / UNITS_PER_WORD;
	      for (i = nwords; i < total_words; i++)
		emit_move_insn
		  (operand_subword (target,
				    backwards ? total_words - i - 1 : i,
				    1, VOIDmode),
		   const0_rtx);
	    }
	  return target;
	}

      /* Signed bit field: sign-extend with two arithmetic shifts.  */
      target = expand_shift (LSHIFT_EXPR, mode, target,
			     GET_MODE_BITSIZE (mode) - bitsize, NULL_RTX, 0);
      return expand_shift (RSHIFT_EXPR, mode, target,
			   GET_MODE_BITSIZE (mode) - bitsize, NULL_RTX, 0);
    }

  /* If OP0 is a multi-word register, narrow it to the affected word.
     If the region spans two words, defer to extract_split_bit_field.  */
  if (!MEM_P (op0) && GET_MODE_SIZE (GET_MODE (op0)) > UNITS_PER_WORD)
    {
      op0 = simplify_gen_subreg (word_mode, op0, GET_MODE (op0),
				 bitnum / BITS_PER_WORD * UNITS_PER_WORD);
      bitnum %= BITS_PER_WORD;
      if (bitnum + bitsize > BITS_PER_WORD)
	{
	  if (!fallback_p)
	    return NULL_RTX;
	  target = extract_split_bit_field (op0, bitsize, bitnum, unsignedp);
	  return convert_extracted_bit_field (target, mode, tmode, unsignedp);
	}
    }

  /* From here on we know the desired field is smaller than a word.
     If OP0 is a register, it too fits within a word.  */
  enum extraction_pattern pattern = unsignedp ? EP_extzv : EP_extv;
  extraction_insn extv;
  if (!MEM_P (op0)
      /* ??? We could limit the structure size to the part of OP0 that
	 contains the field, with appropriate checks for endianness
	 and TRULY_NOOP_TRUNCATION.  */
      && get_best_reg_extraction_insn (&extv, pattern,
				       GET_MODE_BITSIZE (GET_MODE (op0)),
				       tmode))
    {
      rtx result = extract_bit_field_using_extv (&extv, op0, bitsize, bitnum,
						 unsignedp, target, mode,
						 tmode);
      if (result)
	return result;
    }

  /* If OP0 is a memory, try copying it to a register and seeing if a
     cheap register alternative is available.  */
  if (MEM_P (op0))
    {
      /* Do not use extv/extzv for volatile bitfields when
         -fstrict-volatile-bitfields is in effect.  */
      if (!(MEM_VOLATILE_P (op0) && flag_strict_volatile_bitfields > 0)
	  && get_best_mem_extraction_insn (&extv, pattern, bitsize, bitnum,
					   tmode))
	{
	  rtx result = extract_bit_field_using_extv (&extv, op0, bitsize,
						     bitnum, unsignedp,
						     target, mode,
						     tmode);
	  if (result)
	    return result;
	}

      rtx last = get_last_insn ();

      /* Try loading part of OP0 into a register and extracting the
	 bitfield from that.  */
      unsigned HOST_WIDE_INT bitpos;
      rtx xop0 = adjust_bit_field_mem_for_reg (pattern, op0, bitsize, bitnum,
					       0, 0, tmode, &bitpos);
      if (xop0)
	{
	  xop0 = copy_to_reg (xop0);
	  rtx result = extract_bit_field_1 (xop0, bitsize, bitpos,
					    unsignedp, packedp, target,
					    mode, tmode, false);
	  if (result)
	    return result;
	  delete_insns_since (last);
	}
    }

  if (!fallback_p)
    return NULL;

  /* Find a correspondingly-sized integer field, so we can apply
     shifts and masks to it.  */
  int_mode = int_mode_for_mode (tmode);
  if (int_mode == BLKmode)
    int_mode = int_mode_for_mode (mode);
  /* Should probably push op0 out to memory and then do a load.  */
  gcc_assert (int_mode != BLKmode);

  target = extract_fixed_bit_field (int_mode, op0, bitsize, bitnum,
				    target, unsignedp, packedp);
  return convert_extracted_bit_field (target, mode, tmode, unsignedp);
}

/* Generate code to extract a byte-field from STR_RTX
   containing BITSIZE bits, starting at BITNUM,
   and put it in TARGET if possible (if TARGET is nonzero).
   Regardless of TARGET, we return the rtx for where the value is placed.

   STR_RTX is the structure containing the byte (a REG or MEM).
   UNSIGNEDP is nonzero if this is an unsigned bit field.
   PACKEDP is nonzero if the field has the packed attribute.
   MODE is the natural mode of the field value once extracted.
   TMODE is the mode the caller would like the value to have;
   but the value may be returned with type MODE instead.

   If a TARGET is specified and we can store in it at no extra cost,
   we do so, and return TARGET.
   Otherwise, we return a REG of mode TMODE or MODE, with TMODE preferred
   if they are equally easy.  */

rtx
extract_bit_field (rtx str_rtx, unsigned HOST_WIDE_INT bitsize,
		   unsigned HOST_WIDE_INT bitnum, int unsignedp, bool packedp,
		   rtx target, enum machine_mode mode, enum machine_mode tmode)
{
  return extract_bit_field_1 (str_rtx, bitsize, bitnum, unsignedp, packedp,
			      target, mode, tmode, true);
}

/* Use shifts and boolean operations to extract a field of BITSIZE bits
   from bit BITNUM of OP0.

   UNSIGNEDP is nonzero for an unsigned bit field (don't sign-extend value).
   PACKEDP is true if the field has the packed attribute.

   If TARGET is nonzero, attempts to store the value there
   and return TARGET, but this is not guaranteed.
   If TARGET is not used, create a pseudo-reg of mode TMODE for the value.  */

static rtx
extract_fixed_bit_field (enum machine_mode tmode, rtx op0,
			 unsigned HOST_WIDE_INT bitsize,
			 unsigned HOST_WIDE_INT bitnum, rtx target,
			 int unsignedp, bool packedp)
{
  enum machine_mode mode;

  if (MEM_P (op0))
    {
      /* Get the proper mode to use for this field.  We want a mode that
	 includes the entire field.  If such a mode would be larger than
	 a word, we won't be doing the extraction the normal way.  */

      if (MEM_VOLATILE_P (op0)
	  && flag_strict_volatile_bitfields > 0)
	{
	  if (GET_MODE_BITSIZE (GET_MODE (op0)) > 0)
	    mode = GET_MODE (op0);
	  else if (target && GET_MODE_BITSIZE (GET_MODE (target)) > 0)
	    mode = GET_MODE (target);
	  else
	    mode = tmode;
	}
      else
	mode = get_best_mode (bitsize, bitnum, 0, 0,
			      MEM_ALIGN (op0), word_mode, MEM_VOLATILE_P (op0));

      if (mode == VOIDmode)
	/* The only way this should occur is if the field spans word
	   boundaries.  */
	return extract_split_bit_field (op0, bitsize, bitnum, unsignedp);

      unsigned int total_bits = GET_MODE_BITSIZE (mode);
      HOST_WIDE_INT bit_offset = bitnum - bitnum % total_bits;

      /* If we're accessing a volatile MEM, we can't apply BIT_OFFSET
	 if it results in a multi-word access where we otherwise wouldn't
	 have one.  So, check for that case here.  */
      if (MEM_P (op0)
	  && MEM_VOLATILE_P (op0)
	  && flag_strict_volatile_bitfields > 0
	  && bitnum % BITS_PER_UNIT + bitsize <= total_bits
	  && bitnum % GET_MODE_BITSIZE (mode) + bitsize > total_bits)
	{
	  if (STRICT_ALIGNMENT)
	    {
	      static bool informed_about_misalignment = false;

	      if (packedp)
		{
		  if (bitsize == total_bits)
		    warning_at (input_location, OPT_fstrict_volatile_bitfields,
				"multiple accesses to volatile structure"
				" member because of packed attribute");
		  else
		    warning_at (input_location, OPT_fstrict_volatile_bitfields,
				"multiple accesses to volatile structure"
				" bitfield because of packed attribute");

		  return extract_split_bit_field (op0, bitsize, bitnum,
						  unsignedp);
		}

	      if (bitsize == total_bits)
		warning_at (input_location, OPT_fstrict_volatile_bitfields,
			    "mis-aligned access used for structure member");
	      else
		warning_at (input_location, OPT_fstrict_volatile_bitfields,
			    "mis-aligned access used for structure bitfield");

	      if (! informed_about_misalignment)
		{
		  informed_about_misalignment = true;
		  inform (input_location,
			  "when a volatile object spans multiple type-sized"
			  " locations, the compiler must choose between using"
			  " a single mis-aligned access to preserve the"
			  " volatility, or using multiple aligned accesses"
			  " to avoid runtime faults; this code may fail at"
			  " runtime if the hardware does not allow this"
			  " access");
		}
	    }
	  bit_offset = bitnum - bitnum % BITS_PER_UNIT;
	}
      op0 = adjust_bitfield_address (op0, mode, bit_offset / BITS_PER_UNIT);
      bitnum -= bit_offset;
    }

  mode = GET_MODE (op0);
  gcc_assert (SCALAR_INT_MODE_P (mode));

  /* Note that bitsize + bitnum can be greater than GET_MODE_BITSIZE (mode)
     for invalid input, such as extract equivalent of f5 from
     gcc.dg/pr48335-2.c.  */

  if (BYTES_BIG_ENDIAN)
    /* BITNUM is the distance between our msb and that of OP0.
       Convert it to the distance from the lsb.  */
    bitnum = GET_MODE_BITSIZE (mode) - bitsize - bitnum;

  /* Now BITNUM is always the distance between the field's lsb and that of OP0.
     We have reduced the big-endian case to the little-endian case.  */

  if (unsignedp)
    {
      if (bitnum)
	{
	  /* If the field does not already start at the lsb,
	     shift it so it does.  */
	  /* Maybe propagate the target for the shift.  */
	  rtx subtarget = (target != 0 && REG_P (target) ? target : 0);
	  if (tmode != mode)
	    subtarget = 0;
	  op0 = expand_shift (RSHIFT_EXPR, mode, op0, bitnum, subtarget, 1);
	}
      /* Convert the value to the desired mode.  */
      if (mode != tmode)
	op0 = convert_to_mode (tmode, op0, 1);

      /* Unless the msb of the field used to be the msb when we shifted,
	 mask out the upper bits.  */

      if (GET_MODE_BITSIZE (mode) != bitnum + bitsize)
	return expand_binop (GET_MODE (op0), and_optab, op0,
			     mask_rtx (GET_MODE (op0), 0, bitsize, 0),
			     target, 1, OPTAB_LIB_WIDEN);
      return op0;
    }

  /* To extract a signed bit-field, first shift its msb to the msb of the word,
     then arithmetic-shift its lsb to the lsb of the word.  */
  op0 = force_reg (mode, op0);

  /* Find the narrowest integer mode that contains the field.  */

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_INT); mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    if (GET_MODE_BITSIZE (mode) >= bitsize + bitnum)
      {
	op0 = convert_to_mode (mode, op0, 0);
	break;
      }

  if (mode != tmode)
    target = 0;

  if (GET_MODE_BITSIZE (mode) != (bitsize + bitnum))
    {
      int amount = GET_MODE_BITSIZE (mode) - (bitsize + bitnum);
      /* Maybe propagate the target for the shift.  */
      rtx subtarget = (target != 0 && REG_P (target) ? target : 0);
      op0 = expand_shift (LSHIFT_EXPR, mode, op0, amount, subtarget, 1);
    }

  return expand_shift (RSHIFT_EXPR, mode, op0,
		       GET_MODE_BITSIZE (mode) - bitsize, target, 0);
}

/* Return a constant integer (CONST_INT or CONST_DOUBLE) mask value
   of mode MODE with BITSIZE ones followed by BITPOS zeros, or the
   complement of that if COMPLEMENT.  The mask is truncated if
   necessary to the width of mode MODE.  The mask is zero-extended if
   BITSIZE+BITPOS is too small for MODE.  */

static rtx
mask_rtx (enum machine_mode mode, int bitpos, int bitsize, int complement)
{
  double_int mask;

  mask = double_int::mask (bitsize);
  mask = mask.llshift (bitpos, HOST_BITS_PER_DOUBLE_INT);

  if (complement)
    mask = ~mask;

  return immed_double_int_const (mask, mode);
}

/* Return a constant integer (CONST_INT or CONST_DOUBLE) rtx with the value
   VALUE truncated to BITSIZE bits and then shifted left BITPOS bits.  */

static rtx
lshift_value (enum machine_mode mode, rtx value, int bitpos, int bitsize)
{
  double_int val;
  
  val = double_int::from_uhwi (INTVAL (value)).zext (bitsize);
  val = val.llshift (bitpos, HOST_BITS_PER_DOUBLE_INT);

  return immed_double_int_const (val, mode);
}

/* Extract a bit field that is split across two words
   and return an RTX for the result.

   OP0 is the REG, SUBREG or MEM rtx for the first of the two words.
   BITSIZE is the field width; BITPOS, position of its first bit, in the word.
   UNSIGNEDP is 1 if should zero-extend the contents; else sign-extend.  */

static rtx
extract_split_bit_field (rtx op0, unsigned HOST_WIDE_INT bitsize,
			 unsigned HOST_WIDE_INT bitpos, int unsignedp)
{
  unsigned int unit;
  unsigned int bitsdone = 0;
  rtx result = NULL_RTX;
  int first = 1;

  /* Make sure UNIT isn't larger than BITS_PER_WORD, we can only handle that
     much at a time.  */
  if (REG_P (op0) || GET_CODE (op0) == SUBREG)
    unit = BITS_PER_WORD;
  else
    unit = MIN (MEM_ALIGN (op0), BITS_PER_WORD);

  while (bitsdone < bitsize)
    {
      unsigned HOST_WIDE_INT thissize;
      rtx part, word;
      unsigned HOST_WIDE_INT thispos;
      unsigned HOST_WIDE_INT offset;

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
	  int word_offset = (SUBREG_BYTE (op0) / UNITS_PER_WORD) + offset;
	  word = operand_subword_force (SUBREG_REG (op0), word_offset,
					GET_MODE (SUBREG_REG (op0)));
	  offset = 0;
	}
      else if (REG_P (op0))
	{
	  word = operand_subword_force (op0, offset, GET_MODE (op0));
	  offset = 0;
	}
      else
	word = op0;

      /* Extract the parts in bit-counting order,
	 whose meaning is determined by BYTES_PER_UNIT.
	 OFFSET is in UNITs, and UNIT is in bits.  */
      part = extract_fixed_bit_field (word_mode, word, thissize,
				      offset * unit + thispos, 0, 1, false);
      bitsdone += thissize;

      /* Shift this part into place for the result.  */
      if (BYTES_BIG_ENDIAN)
	{
	  if (bitsize != bitsdone)
	    part = expand_shift (LSHIFT_EXPR, word_mode, part,
				 bitsize - bitsdone, 0, 1);
	}
      else
	{
	  if (bitsdone != thissize)
	    part = expand_shift (LSHIFT_EXPR, word_mode, part,
				 bitsdone - thissize, 0, 1);
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
			 BITS_PER_WORD - bitsize, NULL_RTX, 0);
  return expand_shift (RSHIFT_EXPR, word_mode, result,
		       BITS_PER_WORD - bitsize, NULL_RTX, 0);
}

/* Try to read the low bits of SRC as an rvalue of mode MODE, preserving
   the bit pattern.  SRC_MODE is the mode of SRC; if this is smaller than
   MODE, fill the upper bits with zeros.  Fail if the layout of either
   mode is unknown (as for CC modes) or if the extraction would involve
   unprofitable mode punning.  Return the value on success, otherwise
   return null.

   This is different from gen_lowpart* in these respects:

     - the returned value must always be considered an rvalue

     - when MODE is wider than SRC_MODE, the extraction involves
       a zero extension

     - when MODE is smaller than SRC_MODE, the extraction involves
       a truncation (and is thus subject to TRULY_NOOP_TRUNCATION).

   In other words, this routine performs a computation, whereas the
   gen_lowpart* routines are conceptually lvalue or rvalue subreg
   operations.  */

rtx
extract_low_bits (enum machine_mode mode, enum machine_mode src_mode, rtx src)
{
  enum machine_mode int_mode, src_int_mode;

  if (mode == src_mode)
    return src;

  if (CONSTANT_P (src))
    {
      /* simplify_gen_subreg can't be used here, as if simplify_subreg
	 fails, it will happily create (subreg (symbol_ref)) or similar
	 invalid SUBREGs.  */
      unsigned int byte = subreg_lowpart_offset (mode, src_mode);
      rtx ret = simplify_subreg (mode, src, src_mode, byte);
      if (ret)
	return ret;

      if (GET_MODE (src) == VOIDmode
	  || !validate_subreg (mode, src_mode, src, byte))
	return NULL_RTX;

      src = force_reg (GET_MODE (src), src);
      return gen_rtx_SUBREG (mode, src, byte);
    }

  if (GET_MODE_CLASS (mode) == MODE_CC || GET_MODE_CLASS (src_mode) == MODE_CC)
    return NULL_RTX;

  if (GET_MODE_BITSIZE (mode) == GET_MODE_BITSIZE (src_mode)
      && MODES_TIEABLE_P (mode, src_mode))
    {
      rtx x = gen_lowpart_common (mode, src);
      if (x)
        return x;
    }

  src_int_mode = int_mode_for_mode (src_mode);
  int_mode = int_mode_for_mode (mode);
  if (src_int_mode == BLKmode || int_mode == BLKmode)
    return NULL_RTX;

  if (!MODES_TIEABLE_P (src_int_mode, src_mode))
    return NULL_RTX;
  if (!MODES_TIEABLE_P (int_mode, mode))
    return NULL_RTX;

  src = gen_lowpart (src_int_mode, src);
  src = convert_modes (int_mode, src_int_mode, src, true);
  src = gen_lowpart (mode, src);
  return src;
}

/* Add INC into TARGET.  */

void
expand_inc (rtx target, rtx inc)
{
  rtx value = expand_binop (GET_MODE (target), add_optab,
			    target, inc,
			    target, 0, OPTAB_LIB_WIDEN);
  if (value != target)
    emit_move_insn (target, value);
}

/* Subtract DEC from TARGET.  */

void
expand_dec (rtx target, rtx dec)
{
  rtx value = expand_binop (GET_MODE (target), sub_optab,
			    target, dec,
			    target, 0, OPTAB_LIB_WIDEN);
  if (value != target)
    emit_move_insn (target, value);
}

/* Output a shift instruction for expression code CODE,
   with SHIFTED being the rtx for the value to shift,
   and AMOUNT the rtx for the amount to shift by.
   Store the result in the rtx TARGET, if that is convenient.
   If UNSIGNEDP is nonzero, do a logical shift; otherwise, arithmetic.
   Return the rtx for where the value is.  */

static rtx
expand_shift_1 (enum tree_code code, enum machine_mode mode, rtx shifted,
		rtx amount, rtx target, int unsignedp)
{
  rtx op1, temp = 0;
  int left = (code == LSHIFT_EXPR || code == LROTATE_EXPR);
  int rotate = (code == LROTATE_EXPR || code == RROTATE_EXPR);
  optab lshift_optab = ashl_optab;
  optab rshift_arith_optab = ashr_optab;
  optab rshift_uns_optab = lshr_optab;
  optab lrotate_optab = rotl_optab;
  optab rrotate_optab = rotr_optab;
  enum machine_mode op1_mode;
  int attempt;
  bool speed = optimize_insn_for_speed_p ();

  op1 = amount;
  op1_mode = GET_MODE (op1);

  /* Determine whether the shift/rotate amount is a vector, or scalar.  If the
     shift amount is a vector, use the vector/vector shift patterns.  */
  if (VECTOR_MODE_P (mode) && VECTOR_MODE_P (op1_mode))
    {
      lshift_optab = vashl_optab;
      rshift_arith_optab = vashr_optab;
      rshift_uns_optab = vlshr_optab;
      lrotate_optab = vrotl_optab;
      rrotate_optab = vrotr_optab;
    }

  /* Previously detected shift-counts computed by NEGATE_EXPR
     and shifted in the other direction; but that does not work
     on all machines.  */

  if (SHIFT_COUNT_TRUNCATED)
    {
      if (CONST_INT_P (op1)
	  && ((unsigned HOST_WIDE_INT) INTVAL (op1) >=
	      (unsigned HOST_WIDE_INT) GET_MODE_BITSIZE (mode)))
	op1 = GEN_INT ((unsigned HOST_WIDE_INT) INTVAL (op1)
		       % GET_MODE_BITSIZE (mode));
      else if (GET_CODE (op1) == SUBREG
	       && subreg_lowpart_p (op1)
	       && SCALAR_INT_MODE_P (GET_MODE (SUBREG_REG (op1)))
	       && SCALAR_INT_MODE_P (GET_MODE (op1)))
	op1 = SUBREG_REG (op1);
    }

  /* Canonicalize rotates by constant amount.  If op1 is bitsize / 2,
     prefer left rotation, if op1 is from bitsize / 2 + 1 to
     bitsize - 1, use other direction of rotate with 1 .. bitsize / 2 - 1
     amount instead.  */
  if (rotate
      && CONST_INT_P (op1)
      && IN_RANGE (INTVAL (op1), GET_MODE_BITSIZE (mode) / 2 + left,
		   GET_MODE_BITSIZE (mode) - 1))
    {
      op1 = GEN_INT (GET_MODE_BITSIZE (mode) - INTVAL (op1));
      left = !left;
      code = left ? LROTATE_EXPR : RROTATE_EXPR;
    }

  if (op1 == const0_rtx)
    return shifted;

  /* Check whether its cheaper to implement a left shift by a constant
     bit count by a sequence of additions.  */
  if (code == LSHIFT_EXPR
      && CONST_INT_P (op1)
      && INTVAL (op1) > 0
      && INTVAL (op1) < GET_MODE_PRECISION (mode)
      && INTVAL (op1) < MAX_BITS_PER_WORD
      && (shift_cost (speed, mode, INTVAL (op1))
	  > INTVAL (op1) * add_cost (speed, mode))
      && shift_cost (speed, mode, INTVAL (op1)) != MAX_COST)
    {
      int i;
      for (i = 0; i < INTVAL (op1); i++)
	{
	  temp = force_reg (mode, shifted);
	  shifted = expand_binop (mode, add_optab, temp, temp, NULL_RTX,
				  unsignedp, OPTAB_LIB_WIDEN);
	}
      return shifted;
    }

  for (attempt = 0; temp == 0 && attempt < 3; attempt++)
    {
      enum optab_methods methods;

      if (attempt == 0)
	methods = OPTAB_DIRECT;
      else if (attempt == 1)
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
		 by N bits, compute
		 (A << N) | ((unsigned) A >> ((-N) & (C - 1)))
		 where C is the bitsize of A.

		 It is theoretically possible that the target machine might
		 not be able to perform either shift and hence we would
		 be making two libcalls rather than just the one for the
		 shift (similarly if IOR could not be done).  We will allow
		 this extremely unlikely lossage to avoid complicating the
		 code below.  */

	      rtx subtarget = target == shifted ? 0 : target;
	      rtx new_amount, other_amount;
	      rtx temp1;

	      new_amount = op1;
	      if (op1 == const0_rtx)
		return shifted;
	      else if (CONST_INT_P (op1))
		other_amount = GEN_INT (GET_MODE_BITSIZE (mode)
					- INTVAL (op1));
	      else
		{
		  other_amount
		    = simplify_gen_unary (NEG, GET_MODE (op1),
					  op1, GET_MODE (op1));
		  HOST_WIDE_INT mask = GET_MODE_PRECISION (mode) - 1;
		  other_amount
		    = simplify_gen_binary (AND, GET_MODE (op1), other_amount,
					   gen_int_mode (mask, GET_MODE (op1)));
		}

	      shifted = force_reg (mode, shifted);

	      temp = expand_shift_1 (left ? LSHIFT_EXPR : RSHIFT_EXPR,
				     mode, shifted, new_amount, 0, 1);
	      temp1 = expand_shift_1 (left ? RSHIFT_EXPR : LSHIFT_EXPR,
				      mode, shifted, other_amount,
				      subtarget, 1);
	      return expand_binop (mode, ior_optab, temp, temp1, target,
				   unsignedp, methods);
	    }

	  temp = expand_binop (mode,
			       left ? lrotate_optab : rrotate_optab,
			       shifted, op1, target, unsignedp, methods);
	}
      else if (unsignedp)
	temp = expand_binop (mode,
			     left ? lshift_optab : rshift_uns_optab,
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
			       left ? lshift_optab : rshift_arith_optab,
			       shifted, op1, target, unsignedp, methods1);
	}

      /* We used to try extzv here for logical right shifts, but that was
	 only useful for one machine, the VAX, and caused poor code
	 generation there for lshrdi3, so the code was deleted and a
	 define_expand for lshrsi3 was added to vax.md.  */
    }

  gcc_assert (temp);
  return temp;
}

/* Output a shift instruction for expression code CODE,
   with SHIFTED being the rtx for the value to shift,
   and AMOUNT the amount to shift by.
   Store the result in the rtx TARGET, if that is convenient.
   If UNSIGNEDP is nonzero, do a logical shift; otherwise, arithmetic.
   Return the rtx for where the value is.  */

rtx
expand_shift (enum tree_code code, enum machine_mode mode, rtx shifted,
	      int amount, rtx target, int unsignedp)
{
  return expand_shift_1 (code, mode,
			 shifted, GEN_INT (amount), target, unsignedp);
}

/* Output a shift instruction for expression code CODE,
   with SHIFTED being the rtx for the value to shift,
   and AMOUNT the tree for the amount to shift by.
   Store the result in the rtx TARGET, if that is convenient.
   If UNSIGNEDP is nonzero, do a logical shift; otherwise, arithmetic.
   Return the rtx for where the value is.  */

rtx
expand_variable_shift (enum tree_code code, enum machine_mode mode, rtx shifted,
		       tree amount, rtx target, int unsignedp)
{
  return expand_shift_1 (code, mode,
			 shifted, expand_normal (amount), target, unsignedp);
}


/* Indicates the type of fixup needed after a constant multiplication.
   BASIC_VARIANT means no fixup is needed, NEGATE_VARIANT means that
   the result should be negated, and ADD_VARIANT means that the
   multiplicand should be added to the result.  */
enum mult_variant {basic_variant, negate_variant, add_variant};

static void synth_mult (struct algorithm *, unsigned HOST_WIDE_INT,
			const struct mult_cost *, enum machine_mode mode);
static bool choose_mult_variant (enum machine_mode, HOST_WIDE_INT,
				 struct algorithm *, enum mult_variant *, int);
static rtx expand_mult_const (enum machine_mode, rtx, HOST_WIDE_INT, rtx,
			      const struct algorithm *, enum mult_variant);
static unsigned HOST_WIDE_INT invert_mod2n (unsigned HOST_WIDE_INT, int);
static rtx extract_high_half (enum machine_mode, rtx);
static rtx expmed_mult_highpart (enum machine_mode, rtx, rtx, rtx, int, int);
static rtx expmed_mult_highpart_optab (enum machine_mode, rtx, rtx, rtx,
				       int, int);
/* Compute and return the best algorithm for multiplying by T.
   The algorithm must cost less than cost_limit
   If retval.cost >= COST_LIMIT, no algorithm was found and all
   other field of the returned struct are undefined.
   MODE is the machine mode of the multiplication.  */

static void
synth_mult (struct algorithm *alg_out, unsigned HOST_WIDE_INT t,
	    const struct mult_cost *cost_limit, enum machine_mode mode)
{
  int m;
  struct algorithm *alg_in, *best_alg;
  struct mult_cost best_cost;
  struct mult_cost new_limit;
  int op_cost, op_latency;
  unsigned HOST_WIDE_INT orig_t = t;
  unsigned HOST_WIDE_INT q;
  int maxm, hash_index;
  bool cache_hit = false;
  enum alg_code cache_alg = alg_zero;
  bool speed = optimize_insn_for_speed_p ();
  enum machine_mode imode;
  struct alg_hash_entry *entry_ptr;

  /* Indicate that no algorithm is yet found.  If no algorithm
     is found, this value will be returned and indicate failure.  */
  alg_out->cost.cost = cost_limit->cost + 1;
  alg_out->cost.latency = cost_limit->latency + 1;

  if (cost_limit->cost < 0
      || (cost_limit->cost == 0 && cost_limit->latency <= 0))
    return;

  /* Be prepared for vector modes.  */
  imode = GET_MODE_INNER (mode);
  if (imode == VOIDmode)
    imode = mode;

  maxm = MIN (BITS_PER_WORD, GET_MODE_BITSIZE (imode));

  /* Restrict the bits of "t" to the multiplication's mode.  */
  t &= GET_MODE_MASK (imode);

  /* t == 1 can be done in zero cost.  */
  if (t == 1)
    {
      alg_out->ops = 1;
      alg_out->cost.cost = 0;
      alg_out->cost.latency = 0;
      alg_out->op[0] = alg_m;
      return;
    }

  /* t == 0 sometimes has a cost.  If it does and it exceeds our limit,
     fail now.  */
  if (t == 0)
    {
      if (MULT_COST_LESS (cost_limit, zero_cost (speed)))
	return;
      else
	{
	  alg_out->ops = 1;
	  alg_out->cost.cost = zero_cost (speed);
	  alg_out->cost.latency = zero_cost (speed);
	  alg_out->op[0] = alg_zero;
	  return;
	}
    }

  /* We'll be needing a couple extra algorithm structures now.  */

  alg_in = XALLOCA (struct algorithm);
  best_alg = XALLOCA (struct algorithm);
  best_cost = *cost_limit;

  /* Compute the hash index.  */
  hash_index = (t ^ (unsigned int) mode ^ (speed * 256)) % NUM_ALG_HASH_ENTRIES;

  /* See if we already know what to do for T.  */
  entry_ptr = alg_hash_entry_ptr (hash_index);
  if (entry_ptr->t == t
      && entry_ptr->mode == mode
      && entry_ptr->mode == mode
      && entry_ptr->speed == speed
      && entry_ptr->alg != alg_unknown)
    {
      cache_alg = entry_ptr->alg;

      if (cache_alg == alg_impossible)
	{
	  /* The cache tells us that it's impossible to synthesize
	     multiplication by T within entry_ptr->cost.  */
	  if (!CHEAPER_MULT_COST (&entry_ptr->cost, cost_limit))
	    /* COST_LIMIT is at least as restrictive as the one
	       recorded in the hash table, in which case we have no
	       hope of synthesizing a multiplication.  Just
	       return.  */
	    return;

	  /* If we get here, COST_LIMIT is less restrictive than the
	     one recorded in the hash table, so we may be able to
	     synthesize a multiplication.  Proceed as if we didn't
	     have the cache entry.  */
	}
      else
	{
	  if (CHEAPER_MULT_COST (cost_limit, &entry_ptr->cost))
	    /* The cached algorithm shows that this multiplication
	       requires more cost than COST_LIMIT.  Just return.  This
	       way, we don't clobber this cache entry with
	       alg_impossible but retain useful information.  */
	    return;

	  cache_hit = true;

	  switch (cache_alg)
	    {
	    case alg_shift:
	      goto do_alg_shift;

	    case alg_add_t_m2:
	    case alg_sub_t_m2:
	      goto do_alg_addsub_t_m2;

	    case alg_add_factor:
	    case alg_sub_factor:
	      goto do_alg_addsub_factor;

	    case alg_add_t2_m:
	      goto do_alg_add_t2_m;

	    case alg_sub_t2_m:
	      goto do_alg_sub_t2_m;

	    default:
	      gcc_unreachable ();
	    }
	}
    }

  /* If we have a group of zero bits at the low-order part of T, try
     multiplying by the remaining bits and then doing a shift.  */

  if ((t & 1) == 0)
    {
    do_alg_shift:
      m = floor_log2 (t & -t);	/* m = number of low zero bits */
      if (m < maxm)
	{
	  q = t >> m;
	  /* The function expand_shift will choose between a shift and
	     a sequence of additions, so the observed cost is given as
	     MIN (m * add_cost(speed, mode), shift_cost(speed, mode, m)).  */
	  op_cost = m * add_cost (speed, mode);
	  if (shift_cost (speed, mode, m) < op_cost)
	    op_cost = shift_cost (speed, mode, m);
	  new_limit.cost = best_cost.cost - op_cost;
	  new_limit.latency = best_cost.latency - op_cost;
	  synth_mult (alg_in, q, &new_limit, mode);

	  alg_in->cost.cost += op_cost;
	  alg_in->cost.latency += op_cost;
	  if (CHEAPER_MULT_COST (&alg_in->cost, &best_cost))
	    {
	      struct algorithm *x;
	      best_cost = alg_in->cost;
	      x = alg_in, alg_in = best_alg, best_alg = x;
	      best_alg->log[best_alg->ops] = m;
	      best_alg->op[best_alg->ops] = alg_shift;
	    }

	  /* See if treating ORIG_T as a signed number yields a better
	     sequence.  Try this sequence only for a negative ORIG_T
	     as it would be useless for a non-negative ORIG_T.  */
	  if ((HOST_WIDE_INT) orig_t < 0)
	    {
	      /* Shift ORIG_T as follows because a right shift of a
		 negative-valued signed type is implementation
		 defined.  */
	      q = ~(~orig_t >> m);
	      /* The function expand_shift will choose between a shift
		 and a sequence of additions, so the observed cost is
		 given as MIN (m * add_cost(speed, mode),
		 shift_cost(speed, mode, m)).  */
	      op_cost = m * add_cost (speed, mode);
	      if (shift_cost (speed, mode, m) < op_cost)
		op_cost = shift_cost (speed, mode, m);
	      new_limit.cost = best_cost.cost - op_cost;
	      new_limit.latency = best_cost.latency - op_cost;
	      synth_mult (alg_in, q, &new_limit, mode);

	      alg_in->cost.cost += op_cost;
	      alg_in->cost.latency += op_cost;
	      if (CHEAPER_MULT_COST (&alg_in->cost, &best_cost))
		{
		  struct algorithm *x;
		  best_cost = alg_in->cost;
		  x = alg_in, alg_in = best_alg, best_alg = x;
		  best_alg->log[best_alg->ops] = m;
		  best_alg->op[best_alg->ops] = alg_shift;
		}
	    }
	}
      if (cache_hit)
	goto done;
    }

  /* If we have an odd number, add or subtract one.  */
  if ((t & 1) != 0)
    {
      unsigned HOST_WIDE_INT w;

    do_alg_addsub_t_m2:
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

	  op_cost = add_cost (speed, mode);
	  new_limit.cost = best_cost.cost - op_cost;
	  new_limit.latency = best_cost.latency - op_cost;
	  synth_mult (alg_in, t + 1, &new_limit, mode);

	  alg_in->cost.cost += op_cost;
	  alg_in->cost.latency += op_cost;
	  if (CHEAPER_MULT_COST (&alg_in->cost, &best_cost))
	    {
	      struct algorithm *x;
	      best_cost = alg_in->cost;
	      x = alg_in, alg_in = best_alg, best_alg = x;
	      best_alg->log[best_alg->ops] = 0;
	      best_alg->op[best_alg->ops] = alg_sub_t_m2;
	    }
	}
      else
	{
	  /* T ends with ...01 or ...011.  Multiply by (T - 1) and add 1.  */

	  op_cost = add_cost (speed, mode);
	  new_limit.cost = best_cost.cost - op_cost;
	  new_limit.latency = best_cost.latency - op_cost;
	  synth_mult (alg_in, t - 1, &new_limit, mode);

	  alg_in->cost.cost += op_cost;
	  alg_in->cost.latency += op_cost;
	  if (CHEAPER_MULT_COST (&alg_in->cost, &best_cost))
	    {
	      struct algorithm *x;
	      best_cost = alg_in->cost;
	      x = alg_in, alg_in = best_alg, best_alg = x;
	      best_alg->log[best_alg->ops] = 0;
	      best_alg->op[best_alg->ops] = alg_add_t_m2;
	    }
	}

      /* We may be able to calculate a * -7, a * -15, a * -31, etc
	 quickly with a - a * n for some appropriate constant n.  */
      m = exact_log2 (-orig_t + 1);
      if (m >= 0 && m < maxm)
	{
	  op_cost = shiftsub1_cost (speed, mode, m);
	  new_limit.cost = best_cost.cost - op_cost;
	  new_limit.latency = best_cost.latency - op_cost;
	  synth_mult (alg_in, (unsigned HOST_WIDE_INT) (-orig_t + 1) >> m,
		      &new_limit, mode);

	  alg_in->cost.cost += op_cost;
	  alg_in->cost.latency += op_cost;
	  if (CHEAPER_MULT_COST (&alg_in->cost, &best_cost))
	    {
	      struct algorithm *x;
	      best_cost = alg_in->cost;
	      x = alg_in, alg_in = best_alg, best_alg = x;
	      best_alg->log[best_alg->ops] = m;
	      best_alg->op[best_alg->ops] = alg_sub_t_m2;
	    }
	}

      if (cache_hit)
	goto done;
    }

  /* Look for factors of t of the form
     t = q(2**m +- 1), 2 <= m <= floor(log2(t - 1)).
     If we find such a factor, we can multiply by t using an algorithm that
     multiplies by q, shift the result by m and add/subtract it to itself.

     We search for large factors first and loop down, even if large factors
     are less probable than small; if we find a large factor we will find a
     good sequence quickly, and therefore be able to prune (by decreasing
     COST_LIMIT) the search.  */

 do_alg_addsub_factor:
  for (m = floor_log2 (t - 1); m >= 2; m--)
    {
      unsigned HOST_WIDE_INT d;

      d = ((unsigned HOST_WIDE_INT) 1 << m) + 1;
      if (t % d == 0 && t > d && m < maxm
	  && (!cache_hit || cache_alg == alg_add_factor))
	{
	  /* If the target has a cheap shift-and-add instruction use
	     that in preference to a shift insn followed by an add insn.
	     Assume that the shift-and-add is "atomic" with a latency
	     equal to its cost, otherwise assume that on superscalar
	     hardware the shift may be executed concurrently with the
	     earlier steps in the algorithm.  */
	  op_cost = add_cost (speed, mode) + shift_cost (speed, mode, m);
	  if (shiftadd_cost (speed, mode, m) < op_cost)
	    {
	      op_cost = shiftadd_cost (speed, mode, m);
	      op_latency = op_cost;
	    }
	  else
	    op_latency = add_cost (speed, mode);

	  new_limit.cost = best_cost.cost - op_cost;
	  new_limit.latency = best_cost.latency - op_latency;
	  synth_mult (alg_in, t / d, &new_limit, mode);

	  alg_in->cost.cost += op_cost;
	  alg_in->cost.latency += op_latency;
	  if (alg_in->cost.latency < op_cost)
	    alg_in->cost.latency = op_cost;
	  if (CHEAPER_MULT_COST (&alg_in->cost, &best_cost))
	    {
	      struct algorithm *x;
	      best_cost = alg_in->cost;
	      x = alg_in, alg_in = best_alg, best_alg = x;
	      best_alg->log[best_alg->ops] = m;
	      best_alg->op[best_alg->ops] = alg_add_factor;
	    }
	  /* Other factors will have been taken care of in the recursion.  */
	  break;
	}

      d = ((unsigned HOST_WIDE_INT) 1 << m) - 1;
      if (t % d == 0 && t > d && m < maxm
	  && (!cache_hit || cache_alg == alg_sub_factor))
	{
	  /* If the target has a cheap shift-and-subtract insn use
	     that in preference to a shift insn followed by a sub insn.
	     Assume that the shift-and-sub is "atomic" with a latency
	     equal to it's cost, otherwise assume that on superscalar
	     hardware the shift may be executed concurrently with the
	     earlier steps in the algorithm.  */
	  op_cost = add_cost (speed, mode) + shift_cost (speed, mode, m);
	  if (shiftsub0_cost (speed, mode, m) < op_cost)
	    {
	      op_cost = shiftsub0_cost (speed, mode, m);
	      op_latency = op_cost;
	    }
	  else
	    op_latency = add_cost (speed, mode);

	  new_limit.cost = best_cost.cost - op_cost;
	  new_limit.latency = best_cost.latency - op_latency;
	  synth_mult (alg_in, t / d, &new_limit, mode);

	  alg_in->cost.cost += op_cost;
	  alg_in->cost.latency += op_latency;
	  if (alg_in->cost.latency < op_cost)
	    alg_in->cost.latency = op_cost;
	  if (CHEAPER_MULT_COST (&alg_in->cost, &best_cost))
	    {
	      struct algorithm *x;
	      best_cost = alg_in->cost;
	      x = alg_in, alg_in = best_alg, best_alg = x;
	      best_alg->log[best_alg->ops] = m;
	      best_alg->op[best_alg->ops] = alg_sub_factor;
	    }
	  break;
	}
    }
  if (cache_hit)
    goto done;

  /* Try shift-and-add (load effective address) instructions,
     i.e. do a*3, a*5, a*9.  */
  if ((t & 1) != 0)
    {
    do_alg_add_t2_m:
      q = t - 1;
      q = q & -q;
      m = exact_log2 (q);
      if (m >= 0 && m < maxm)
	{
	  op_cost = shiftadd_cost (speed, mode, m);
	  new_limit.cost = best_cost.cost - op_cost;
	  new_limit.latency = best_cost.latency - op_cost;
	  synth_mult (alg_in, (t - 1) >> m, &new_limit, mode);

	  alg_in->cost.cost += op_cost;
	  alg_in->cost.latency += op_cost;
	  if (CHEAPER_MULT_COST (&alg_in->cost, &best_cost))
	    {
	      struct algorithm *x;
	      best_cost = alg_in->cost;
	      x = alg_in, alg_in = best_alg, best_alg = x;
	      best_alg->log[best_alg->ops] = m;
	      best_alg->op[best_alg->ops] = alg_add_t2_m;
	    }
	}
      if (cache_hit)
	goto done;

    do_alg_sub_t2_m:
      q = t + 1;
      q = q & -q;
      m = exact_log2 (q);
      if (m >= 0 && m < maxm)
	{
	  op_cost = shiftsub0_cost (speed, mode, m);
	  new_limit.cost = best_cost.cost - op_cost;
	  new_limit.latency = best_cost.latency - op_cost;
	  synth_mult (alg_in, (t + 1) >> m, &new_limit, mode);

	  alg_in->cost.cost += op_cost;
	  alg_in->cost.latency += op_cost;
	  if (CHEAPER_MULT_COST (&alg_in->cost, &best_cost))
	    {
	      struct algorithm *x;
	      best_cost = alg_in->cost;
	      x = alg_in, alg_in = best_alg, best_alg = x;
	      best_alg->log[best_alg->ops] = m;
	      best_alg->op[best_alg->ops] = alg_sub_t2_m;
	    }
	}
      if (cache_hit)
	goto done;
    }

 done:
  /* If best_cost has not decreased, we have not found any algorithm.  */
  if (!CHEAPER_MULT_COST (&best_cost, cost_limit))
    {
      /* We failed to find an algorithm.  Record alg_impossible for
	 this case (that is, <T, MODE, COST_LIMIT>) so that next time
	 we are asked to find an algorithm for T within the same or
	 lower COST_LIMIT, we can immediately return to the
	 caller.  */
      entry_ptr->t = t;
      entry_ptr->mode = mode;
      entry_ptr->speed = speed;
      entry_ptr->alg = alg_impossible;
      entry_ptr->cost = *cost_limit;
      return;
    }

  /* Cache the result.  */
  if (!cache_hit)
    {
      entry_ptr->t = t;
      entry_ptr->mode = mode;
      entry_ptr->speed = speed;
      entry_ptr->alg = best_alg->op[best_alg->ops];
      entry_ptr->cost.cost = best_cost.cost;
      entry_ptr->cost.latency = best_cost.latency;
    }

  /* If we are getting a too long sequence for `struct algorithm'
     to record, make this search fail.  */
  if (best_alg->ops == MAX_BITS_PER_WORD)
    return;

  /* Copy the algorithm from temporary space to the space at alg_out.
     We avoid using structure assignment because the majority of
     best_alg is normally undefined, and this is a critical function.  */
  alg_out->ops = best_alg->ops + 1;
  alg_out->cost = best_cost;
  memcpy (alg_out->op, best_alg->op,
	  alg_out->ops * sizeof *alg_out->op);
  memcpy (alg_out->log, best_alg->log,
	  alg_out->ops * sizeof *alg_out->log);
}

/* Find the cheapest way of multiplying a value of mode MODE by VAL.
   Try three variations:

       - a shift/add sequence based on VAL itself
       - a shift/add sequence based on -VAL, followed by a negation
       - a shift/add sequence based on VAL - 1, followed by an addition.

   Return true if the cheapest of these cost less than MULT_COST,
   describing the algorithm in *ALG and final fixup in *VARIANT.  */

static bool
choose_mult_variant (enum machine_mode mode, HOST_WIDE_INT val,
		     struct algorithm *alg, enum mult_variant *variant,
		     int mult_cost)
{
  struct algorithm alg2;
  struct mult_cost limit;
  int op_cost;
  bool speed = optimize_insn_for_speed_p ();

  /* Fail quickly for impossible bounds.  */
  if (mult_cost < 0)
    return false;

  /* Ensure that mult_cost provides a reasonable upper bound.
     Any constant multiplication can be performed with less
     than 2 * bits additions.  */
  op_cost = 2 * GET_MODE_UNIT_BITSIZE (mode) * add_cost (speed, mode);
  if (mult_cost > op_cost)
    mult_cost = op_cost;

  *variant = basic_variant;
  limit.cost = mult_cost;
  limit.latency = mult_cost;
  synth_mult (alg, val, &limit, mode);

  /* This works only if the inverted value actually fits in an
     `unsigned int' */
  if (HOST_BITS_PER_INT >= GET_MODE_UNIT_BITSIZE (mode))
    {
      op_cost = neg_cost(speed, mode);
      if (MULT_COST_LESS (&alg->cost, mult_cost))
	{
	  limit.cost = alg->cost.cost - op_cost;
	  limit.latency = alg->cost.latency - op_cost;
	}
      else
	{
	  limit.cost = mult_cost - op_cost;
	  limit.latency = mult_cost - op_cost;
	}

      synth_mult (&alg2, -val, &limit, mode);
      alg2.cost.cost += op_cost;
      alg2.cost.latency += op_cost;
      if (CHEAPER_MULT_COST (&alg2.cost, &alg->cost))
	*alg = alg2, *variant = negate_variant;
    }

  /* This proves very useful for division-by-constant.  */
  op_cost = add_cost (speed, mode);
  if (MULT_COST_LESS (&alg->cost, mult_cost))
    {
      limit.cost = alg->cost.cost - op_cost;
      limit.latency = alg->cost.latency - op_cost;
    }
  else
    {
      limit.cost = mult_cost - op_cost;
      limit.latency = mult_cost - op_cost;
    }

  synth_mult (&alg2, val - 1, &limit, mode);
  alg2.cost.cost += op_cost;
  alg2.cost.latency += op_cost;
  if (CHEAPER_MULT_COST (&alg2.cost, &alg->cost))
    *alg = alg2, *variant = add_variant;

  return MULT_COST_LESS (&alg->cost, mult_cost);
}

/* A subroutine of expand_mult, used for constant multiplications.
   Multiply OP0 by VAL in mode MODE, storing the result in TARGET if
   convenient.  Use the shift/add sequence described by ALG and apply
   the final fixup specified by VARIANT.  */

static rtx
expand_mult_const (enum machine_mode mode, rtx op0, HOST_WIDE_INT val,
		   rtx target, const struct algorithm *alg,
		   enum mult_variant variant)
{
  HOST_WIDE_INT val_so_far;
  rtx insn, accum, tem;
  int opno;
  enum machine_mode nmode;

  /* Avoid referencing memory over and over and invalid sharing
     on SUBREGs.  */
  op0 = force_reg (mode, op0);

  /* ACCUM starts out either as OP0 or as a zero, depending on
     the first operation.  */

  if (alg->op[0] == alg_zero)
    {
      accum = copy_to_mode_reg (mode, CONST0_RTX (mode));
      val_so_far = 0;
    }
  else if (alg->op[0] == alg_m)
    {
      accum = copy_to_mode_reg (mode, op0);
      val_so_far = 1;
    }
  else
    gcc_unreachable ();

  for (opno = 1; opno < alg->ops; opno++)
    {
      int log = alg->log[opno];
      rtx shift_subtarget = optimize ? 0 : accum;
      rtx add_target
	= (opno == alg->ops - 1 && target != 0 && variant != add_variant
	   && !optimize)
	  ? target : 0;
      rtx accum_target = optimize ? 0 : accum;
      rtx accum_inner;

      switch (alg->op[opno])
	{
	case alg_shift:
	  tem = expand_shift (LSHIFT_EXPR, mode, accum, log, NULL_RTX, 0);
	  /* REG_EQUAL note will be attached to the following insn.  */
	  emit_move_insn (accum, tem);
	  val_so_far <<= log;
	  break;

	case alg_add_t_m2:
	  tem = expand_shift (LSHIFT_EXPR, mode, op0, log, NULL_RTX, 0);
	  accum = force_operand (gen_rtx_PLUS (mode, accum, tem),
				 add_target ? add_target : accum_target);
	  val_so_far += (HOST_WIDE_INT) 1 << log;
	  break;

	case alg_sub_t_m2:
	  tem = expand_shift (LSHIFT_EXPR, mode, op0, log, NULL_RTX, 0);
	  accum = force_operand (gen_rtx_MINUS (mode, accum, tem),
				 add_target ? add_target : accum_target);
	  val_so_far -= (HOST_WIDE_INT) 1 << log;
	  break;

	case alg_add_t2_m:
	  accum = expand_shift (LSHIFT_EXPR, mode, accum,
				log, shift_subtarget, 0);
	  accum = force_operand (gen_rtx_PLUS (mode, accum, op0),
				 add_target ? add_target : accum_target);
	  val_so_far = (val_so_far << log) + 1;
	  break;

	case alg_sub_t2_m:
	  accum = expand_shift (LSHIFT_EXPR, mode, accum,
				log, shift_subtarget, 0);
	  accum = force_operand (gen_rtx_MINUS (mode, accum, op0),
				 add_target ? add_target : accum_target);
	  val_so_far = (val_so_far << log) - 1;
	  break;

	case alg_add_factor:
	  tem = expand_shift (LSHIFT_EXPR, mode, accum, log, NULL_RTX, 0);
	  accum = force_operand (gen_rtx_PLUS (mode, accum, tem),
				 add_target ? add_target : accum_target);
	  val_so_far += val_so_far << log;
	  break;

	case alg_sub_factor:
	  tem = expand_shift (LSHIFT_EXPR, mode, accum, log, NULL_RTX, 0);
	  accum = force_operand (gen_rtx_MINUS (mode, tem, accum),
				 (add_target
				  ? add_target : (optimize ? 0 : tem)));
	  val_so_far = (val_so_far << log) - val_so_far;
	  break;

	default:
	  gcc_unreachable ();
	}

      if (SCALAR_INT_MODE_P (mode))
	{
	  /* Write a REG_EQUAL note on the last insn so that we can cse
	     multiplication sequences.  Note that if ACCUM is a SUBREG,
	     we've set the inner register and must properly indicate that.  */
          tem = op0, nmode = mode;
          accum_inner = accum;
          if (GET_CODE (accum) == SUBREG)
	    {
	      accum_inner = SUBREG_REG (accum);
	      nmode = GET_MODE (accum_inner);
	      tem = gen_lowpart (nmode, op0);
	    }

          insn = get_last_insn ();
          set_dst_reg_note (insn, REG_EQUAL,
			    gen_rtx_MULT (nmode, tem,
					  gen_int_mode (val_so_far, nmode)),
			    accum_inner);
	}
    }

  if (variant == negate_variant)
    {
      val_so_far = -val_so_far;
      accum = expand_unop (mode, neg_optab, accum, target, 0);
    }
  else if (variant == add_variant)
    {
      val_so_far = val_so_far + 1;
      accum = force_operand (gen_rtx_PLUS (mode, accum, op0), target);
    }

  /* Compare only the bits of val and val_so_far that are significant
     in the result mode, to avoid sign-/zero-extension confusion.  */
  nmode = GET_MODE_INNER (mode);
  if (nmode == VOIDmode)
    nmode = mode;
  val &= GET_MODE_MASK (nmode);
  val_so_far &= GET_MODE_MASK (nmode);
  gcc_assert (val == val_so_far);

  return accum;
}

/* Perform a multiplication and return an rtx for the result.
   MODE is mode of value; OP0 and OP1 are what to multiply (rtx's);
   TARGET is a suggestion for where to store the result (an rtx).

   We check specially for a constant integer as OP1.
   If you want this check for OP0 as well, then before calling
   you should swap the two operands if OP0 would be constant.  */

rtx
expand_mult (enum machine_mode mode, rtx op0, rtx op1, rtx target,
	     int unsignedp)
{
  enum mult_variant variant;
  struct algorithm algorithm;
  rtx scalar_op1;
  int max_cost;
  bool speed = optimize_insn_for_speed_p ();
  bool do_trapv = flag_trapv && SCALAR_INT_MODE_P (mode) && !unsignedp;

  if (CONSTANT_P (op0))
    {
      rtx temp = op0;
      op0 = op1;
      op1 = temp;
    }

  /* For vectors, there are several simplifications that can be made if
     all elements of the vector constant are identical.  */
  scalar_op1 = op1;
  if (GET_CODE (op1) == CONST_VECTOR)
    {
      int i, n = CONST_VECTOR_NUNITS (op1);
      scalar_op1 = CONST_VECTOR_ELT (op1, 0);
      for (i = 1; i < n; ++i)
	if (!rtx_equal_p (scalar_op1, CONST_VECTOR_ELT (op1, i)))
	  goto skip_scalar;
    }

  if (INTEGRAL_MODE_P (mode))
    {
      rtx fake_reg;
      HOST_WIDE_INT coeff;
      bool is_neg;
      int mode_bitsize;

      if (op1 == CONST0_RTX (mode))
	return op1;
      if (op1 == CONST1_RTX (mode))
	return op0;
      if (op1 == CONSTM1_RTX (mode))
	return expand_unop (mode, do_trapv ? negv_optab : neg_optab,
			    op0, target, 0);

      if (do_trapv)
	goto skip_synth;

      /* These are the operations that are potentially turned into
	 a sequence of shifts and additions.  */
      mode_bitsize = GET_MODE_UNIT_BITSIZE (mode);

      /* synth_mult does an `unsigned int' multiply.  As long as the mode is
	 less than or equal in size to `unsigned int' this doesn't matter.
	 If the mode is larger than `unsigned int', then synth_mult works
	 only if the constant value exactly fits in an `unsigned int' without
	 any truncation.  This means that multiplying by negative values does
	 not work; results are off by 2^32 on a 32 bit machine.  */

      if (CONST_INT_P (scalar_op1))
	{
	  coeff = INTVAL (scalar_op1);
	  is_neg = coeff < 0;
	}
      else if (CONST_DOUBLE_AS_INT_P (scalar_op1))
	{
	  /* If we are multiplying in DImode, it may still be a win
	     to try to work with shifts and adds.  */
	  if (CONST_DOUBLE_HIGH (scalar_op1) == 0
	      && (CONST_DOUBLE_LOW (scalar_op1) > 0
		  || (CONST_DOUBLE_LOW (scalar_op1) < 0
		      && EXACT_POWER_OF_2_OR_ZERO_P
			   (CONST_DOUBLE_LOW (scalar_op1)))))
	    {
	      coeff = CONST_DOUBLE_LOW (scalar_op1);
	      is_neg = false;
	    }
	  else if (CONST_DOUBLE_LOW (scalar_op1) == 0)
	    {
	      coeff = CONST_DOUBLE_HIGH (scalar_op1);
	      if (EXACT_POWER_OF_2_OR_ZERO_P (coeff))
		{
		  int shift = floor_log2 (coeff) + HOST_BITS_PER_WIDE_INT;
		  if (shift < HOST_BITS_PER_DOUBLE_INT - 1
		      || mode_bitsize <= HOST_BITS_PER_DOUBLE_INT)
		    return expand_shift (LSHIFT_EXPR, mode, op0,
					 shift, target, unsignedp);
		}
	      goto skip_synth;
	    }
	  else
	    goto skip_synth;
	}
      else
	goto skip_synth;

      /* We used to test optimize here, on the grounds that it's better to
	 produce a smaller program when -O is not used.  But this causes
	 such a terrible slowdown sometimes that it seems better to always
	 use synth_mult.  */

      /* Special case powers of two.  */
      if (EXACT_POWER_OF_2_OR_ZERO_P (coeff)
	  && !(is_neg && mode_bitsize > HOST_BITS_PER_WIDE_INT))
	return expand_shift (LSHIFT_EXPR, mode, op0,
			     floor_log2 (coeff), target, unsignedp);

      fake_reg = gen_raw_REG (mode, LAST_VIRTUAL_REGISTER + 1);

      /* Attempt to handle multiplication of DImode values by negative
	 coefficients, by performing the multiplication by a positive
	 multiplier and then inverting the result.  */
      if (is_neg && mode_bitsize > HOST_BITS_PER_WIDE_INT)
	{
	  /* Its safe to use -coeff even for INT_MIN, as the
	     result is interpreted as an unsigned coefficient.
	     Exclude cost of op0 from max_cost to match the cost
	     calculation of the synth_mult.  */
	  coeff = -(unsigned HOST_WIDE_INT) coeff;
	  max_cost = (set_src_cost (gen_rtx_MULT (mode, fake_reg, op1), speed)
		      - neg_cost(speed, mode));
	  if (max_cost <= 0)
	    goto skip_synth;

	  /* Special case powers of two.  */
	  if (EXACT_POWER_OF_2_OR_ZERO_P (coeff))
	    {
	      rtx temp = expand_shift (LSHIFT_EXPR, mode, op0,
				       floor_log2 (coeff), target, unsignedp);
	      return expand_unop (mode, neg_optab, temp, target, 0);
	    }

	  if (choose_mult_variant (mode, coeff, &algorithm, &variant,
				   max_cost))
	    {
	      rtx temp = expand_mult_const (mode, op0, coeff, NULL_RTX,
					    &algorithm, variant);
	      return expand_unop (mode, neg_optab, temp, target, 0);
	    }
	  goto skip_synth;
	}

      /* Exclude cost of op0 from max_cost to match the cost
	 calculation of the synth_mult.  */
      max_cost = set_src_cost (gen_rtx_MULT (mode, fake_reg, op1), speed);
      if (choose_mult_variant (mode, coeff, &algorithm, &variant, max_cost))
	return expand_mult_const (mode, op0, coeff, target,
				  &algorithm, variant);
    }
 skip_synth:

  /* Expand x*2.0 as x+x.  */
  if (CONST_DOUBLE_AS_FLOAT_P (scalar_op1))
    {
      REAL_VALUE_TYPE d;
      REAL_VALUE_FROM_CONST_DOUBLE (d, scalar_op1);

      if (REAL_VALUES_EQUAL (d, dconst2))
	{
	  op0 = force_reg (GET_MODE (op0), op0);
	  return expand_binop (mode, add_optab, op0, op0,
			       target, unsignedp, OPTAB_LIB_WIDEN);
	}
    }
 skip_scalar:

  /* This used to use umul_optab if unsigned, but for non-widening multiply
     there is no difference between signed and unsigned.  */
  op0 = expand_binop (mode, do_trapv ? smulv_optab : smul_optab,
		      op0, op1, target, unsignedp, OPTAB_LIB_WIDEN);
  gcc_assert (op0);
  return op0;
}

/* Return a cost estimate for multiplying a register by the given
   COEFFicient in the given MODE and SPEED.  */

int
mult_by_coeff_cost (HOST_WIDE_INT coeff, enum machine_mode mode, bool speed)
{
  int max_cost;
  struct algorithm algorithm;
  enum mult_variant variant;

  rtx fake_reg = gen_raw_REG (mode, LAST_VIRTUAL_REGISTER + 1);
  max_cost = set_src_cost (gen_rtx_MULT (mode, fake_reg, fake_reg), speed);
  if (choose_mult_variant (mode, coeff, &algorithm, &variant, max_cost))
    return algorithm.cost.cost;
  else
    return max_cost;
}

/* Perform a widening multiplication and return an rtx for the result.
   MODE is mode of value; OP0 and OP1 are what to multiply (rtx's);
   TARGET is a suggestion for where to store the result (an rtx).
   THIS_OPTAB is the optab we should use, it must be either umul_widen_optab
   or smul_widen_optab.

   We check specially for a constant integer as OP1, comparing the
   cost of a widening multiply against the cost of a sequence of shifts
   and adds.  */

rtx
expand_widening_mult (enum machine_mode mode, rtx op0, rtx op1, rtx target,
		      int unsignedp, optab this_optab)
{
  bool speed = optimize_insn_for_speed_p ();
  rtx cop1;

  if (CONST_INT_P (op1)
      && GET_MODE (op0) != VOIDmode
      && (cop1 = convert_modes (mode, GET_MODE (op0), op1,
				this_optab == umul_widen_optab))
      && CONST_INT_P (cop1)
      && (INTVAL (cop1) >= 0
	  || HWI_COMPUTABLE_MODE_P (mode)))
    {
      HOST_WIDE_INT coeff = INTVAL (cop1);
      int max_cost;
      enum mult_variant variant;
      struct algorithm algorithm;

      /* Special case powers of two.  */
      if (EXACT_POWER_OF_2_OR_ZERO_P (coeff))
	{
	  op0 = convert_to_mode (mode, op0, this_optab == umul_widen_optab);
	  return expand_shift (LSHIFT_EXPR, mode, op0,
			       floor_log2 (coeff), target, unsignedp);
	}

      /* Exclude cost of op0 from max_cost to match the cost
	 calculation of the synth_mult.  */
      max_cost = mul_widen_cost (speed, mode);
      if (choose_mult_variant (mode, coeff, &algorithm, &variant,
			       max_cost))
	{
	  op0 = convert_to_mode (mode, op0, this_optab == umul_widen_optab);
	  return expand_mult_const (mode, op0, coeff, target,
				    &algorithm, variant);
	}
    }
  return expand_binop (mode, this_optab, op0, op1, target,
		       unsignedp, OPTAB_LIB_WIDEN);
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

unsigned HOST_WIDE_INT
choose_multiplier (unsigned HOST_WIDE_INT d, int n, int precision,
		   unsigned HOST_WIDE_INT *multiplier_ptr,
		   int *post_shift_ptr, int *lgup_ptr)
{
  double_int mhigh, mlow;
  int lgup, post_shift;
  int pow, pow2;

  /* lgup = ceil(log2(divisor)); */
  lgup = ceil_log2 (d);

  gcc_assert (lgup <= n);

  pow = n + lgup;
  pow2 = n + lgup - precision;

  /* We could handle this with some effort, but this case is much
     better handled directly with a scc insn, so rely on caller using
     that.  */
  gcc_assert (pow != HOST_BITS_PER_DOUBLE_INT);

  /* mlow = 2^(N + lgup)/d */
  double_int val = double_int_zero.set_bit (pow);
  mlow = val.div (double_int::from_uhwi (d), true, TRUNC_DIV_EXPR); 

  /* mhigh = (2^(N + lgup) + 2^(N + lgup - precision))/d */
  val |= double_int_zero.set_bit (pow2);
  mhigh = val.div (double_int::from_uhwi (d), true, TRUNC_DIV_EXPR);

  gcc_assert (!mhigh.high || val.high - d < d);
  gcc_assert (mhigh.high <= 1 && mlow.high <= 1);
  /* Assert that mlow < mhigh.  */
  gcc_assert (mlow.ult (mhigh));

  /* If precision == N, then mlow, mhigh exceed 2^N
     (but they do not exceed 2^(N+1)).  */

  /* Reduce to lowest terms.  */
  for (post_shift = lgup; post_shift > 0; post_shift--)
    {
      int shft = HOST_BITS_PER_WIDE_INT - 1;
      unsigned HOST_WIDE_INT ml_lo = (mlow.high << shft) | (mlow.low >> 1);
      unsigned HOST_WIDE_INT mh_lo = (mhigh.high << shft) | (mhigh.low >> 1);
      if (ml_lo >= mh_lo)
	break;

      mlow = double_int::from_uhwi (ml_lo);
      mhigh = double_int::from_uhwi (mh_lo);
    }

  *post_shift_ptr = post_shift;
  *lgup_ptr = lgup;
  if (n < HOST_BITS_PER_WIDE_INT)
    {
      unsigned HOST_WIDE_INT mask = ((unsigned HOST_WIDE_INT) 1 << n) - 1;
      *multiplier_ptr = mhigh.low & mask;
      return mhigh.low >= mask;
    }
  else
    {
      *multiplier_ptr = mhigh.low;
      return mhigh.high;
    }
}

/* Compute the inverse of X mod 2**n, i.e., find Y such that X * Y is
   congruent to 1 (mod 2**N).  */

static unsigned HOST_WIDE_INT
invert_mod2n (unsigned HOST_WIDE_INT x, int n)
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
expand_mult_highpart_adjust (enum machine_mode mode, rtx adj_operand, rtx op0,
			     rtx op1, rtx target, int unsignedp)
{
  rtx tem;
  enum rtx_code adj_code = unsignedp ? PLUS : MINUS;

  tem = expand_shift (RSHIFT_EXPR, mode, op0,
		      GET_MODE_BITSIZE (mode) - 1, NULL_RTX, 0);
  tem = expand_and (mode, tem, op1, NULL_RTX);
  adj_operand
    = force_operand (gen_rtx_fmt_ee (adj_code, mode, adj_operand, tem),
		     adj_operand);

  tem = expand_shift (RSHIFT_EXPR, mode, op1,
		      GET_MODE_BITSIZE (mode) - 1, NULL_RTX, 0);
  tem = expand_and (mode, tem, op0, NULL_RTX);
  target = force_operand (gen_rtx_fmt_ee (adj_code, mode, adj_operand, tem),
			  target);

  return target;
}

/* Subroutine of expmed_mult_highpart.  Return the MODE high part of OP.  */

static rtx
extract_high_half (enum machine_mode mode, rtx op)
{
  enum machine_mode wider_mode;

  if (mode == word_mode)
    return gen_highpart (mode, op);

  gcc_assert (!SCALAR_FLOAT_MODE_P (mode));

  wider_mode = GET_MODE_WIDER_MODE (mode);
  op = expand_shift (RSHIFT_EXPR, wider_mode, op,
		     GET_MODE_BITSIZE (mode), 0, 1);
  return convert_modes (mode, wider_mode, op, 0);
}

/* Like expmed_mult_highpart, but only consider using a multiplication
   optab.  OP1 is an rtx for the constant operand.  */

static rtx
expmed_mult_highpart_optab (enum machine_mode mode, rtx op0, rtx op1,
			    rtx target, int unsignedp, int max_cost)
{
  rtx narrow_op1 = gen_int_mode (INTVAL (op1), mode);
  enum machine_mode wider_mode;
  optab moptab;
  rtx tem;
  int size;
  bool speed = optimize_insn_for_speed_p ();

  gcc_assert (!SCALAR_FLOAT_MODE_P (mode));

  wider_mode = GET_MODE_WIDER_MODE (mode);
  size = GET_MODE_BITSIZE (mode);

  /* Firstly, try using a multiplication insn that only generates the needed
     high part of the product, and in the sign flavor of unsignedp.  */
  if (mul_highpart_cost (speed, mode) < max_cost)
    {
      moptab = unsignedp ? umul_highpart_optab : smul_highpart_optab;
      tem = expand_binop (mode, moptab, op0, narrow_op1, target,
			  unsignedp, OPTAB_DIRECT);
      if (tem)
	return tem;
    }

  /* Secondly, same as above, but use sign flavor opposite of unsignedp.
     Need to adjust the result after the multiplication.  */
  if (size - 1 < BITS_PER_WORD
      && (mul_highpart_cost (speed, mode)
	  + 2 * shift_cost (speed, mode, size-1)
	  + 4 * add_cost (speed, mode) < max_cost))
    {
      moptab = unsignedp ? smul_highpart_optab : umul_highpart_optab;
      tem = expand_binop (mode, moptab, op0, narrow_op1, target,
			  unsignedp, OPTAB_DIRECT);
      if (tem)
	/* We used the wrong signedness.  Adjust the result.  */
	return expand_mult_highpart_adjust (mode, tem, op0, narrow_op1,
					    tem, unsignedp);
    }

  /* Try widening multiplication.  */
  moptab = unsignedp ? umul_widen_optab : smul_widen_optab;
  if (widening_optab_handler (moptab, wider_mode, mode) != CODE_FOR_nothing
      && mul_widen_cost (speed, wider_mode) < max_cost)
    {
      tem = expand_binop (wider_mode, moptab, op0, narrow_op1, 0,
			  unsignedp, OPTAB_WIDEN);
      if (tem)
	return extract_high_half (mode, tem);
    }

  /* Try widening the mode and perform a non-widening multiplication.  */
  if (optab_handler (smul_optab, wider_mode) != CODE_FOR_nothing
      && size - 1 < BITS_PER_WORD
      && (mul_cost (speed, wider_mode) + shift_cost (speed, mode, size-1)
	  < max_cost))
    {
      rtx insns, wop0, wop1;

      /* We need to widen the operands, for example to ensure the
	 constant multiplier is correctly sign or zero extended.
	 Use a sequence to clean-up any instructions emitted by
	 the conversions if things don't work out.  */
      start_sequence ();
      wop0 = convert_modes (wider_mode, mode, op0, unsignedp);
      wop1 = convert_modes (wider_mode, mode, op1, unsignedp);
      tem = expand_binop (wider_mode, smul_optab, wop0, wop1, 0,
			  unsignedp, OPTAB_WIDEN);
      insns = get_insns ();
      end_sequence ();

      if (tem)
	{
	  emit_insn (insns);
	  return extract_high_half (mode, tem);
	}
    }

  /* Try widening multiplication of opposite signedness, and adjust.  */
  moptab = unsignedp ? smul_widen_optab : umul_widen_optab;
  if (widening_optab_handler (moptab, wider_mode, mode) != CODE_FOR_nothing
      && size - 1 < BITS_PER_WORD
      && (mul_widen_cost (speed, wider_mode)
	  + 2 * shift_cost (speed, mode, size-1)
	  + 4 * add_cost (speed, mode) < max_cost))
    {
      tem = expand_binop (wider_mode, moptab, op0, narrow_op1,
			  NULL_RTX, ! unsignedp, OPTAB_WIDEN);
      if (tem != 0)
	{
	  tem = extract_high_half (mode, tem);
	  /* We used the wrong signedness.  Adjust the result.  */
	  return expand_mult_highpart_adjust (mode, tem, op0, narrow_op1,
					      target, unsignedp);
	}
    }

  return 0;
}

/* Emit code to multiply OP0 and OP1 (where OP1 is an integer constant),
   putting the high half of the result in TARGET if that is convenient,
   and return where the result is.  If the operation can not be performed,
   0 is returned.

   MODE is the mode of operation and result.

   UNSIGNEDP nonzero means unsigned multiply.

   MAX_COST is the total allowed cost for the expanded RTL.  */

static rtx
expmed_mult_highpart (enum machine_mode mode, rtx op0, rtx op1,
		      rtx target, int unsignedp, int max_cost)
{
  enum machine_mode wider_mode = GET_MODE_WIDER_MODE (mode);
  unsigned HOST_WIDE_INT cnst1;
  int extra_cost;
  bool sign_adjust = false;
  enum mult_variant variant;
  struct algorithm alg;
  rtx tem;
  bool speed = optimize_insn_for_speed_p ();

  gcc_assert (!SCALAR_FLOAT_MODE_P (mode));
  /* We can't support modes wider than HOST_BITS_PER_INT.  */
  gcc_assert (HWI_COMPUTABLE_MODE_P (mode));

  cnst1 = INTVAL (op1) & GET_MODE_MASK (mode);

  /* We can't optimize modes wider than BITS_PER_WORD.
     ??? We might be able to perform double-word arithmetic if
     mode == word_mode, however all the cost calculations in
     synth_mult etc. assume single-word operations.  */
  if (GET_MODE_BITSIZE (wider_mode) > BITS_PER_WORD)
    return expmed_mult_highpart_optab (mode, op0, op1, target,
				       unsignedp, max_cost);

  extra_cost = shift_cost (speed, mode, GET_MODE_BITSIZE (mode) - 1);

  /* Check whether we try to multiply by a negative constant.  */
  if (!unsignedp && ((cnst1 >> (GET_MODE_BITSIZE (mode) - 1)) & 1))
    {
      sign_adjust = true;
      extra_cost += add_cost (speed, mode);
    }

  /* See whether shift/add multiplication is cheap enough.  */
  if (choose_mult_variant (wider_mode, cnst1, &alg, &variant,
			   max_cost - extra_cost))
    {
      /* See whether the specialized multiplication optabs are
	 cheaper than the shift/add version.  */
      tem = expmed_mult_highpart_optab (mode, op0, op1, target, unsignedp,
					alg.cost.cost + extra_cost);
      if (tem)
	return tem;

      tem = convert_to_mode (wider_mode, op0, unsignedp);
      tem = expand_mult_const (wider_mode, tem, cnst1, 0, &alg, variant);
      tem = extract_high_half (mode, tem);

      /* Adjust result for signedness.  */
      if (sign_adjust)
	tem = force_operand (gen_rtx_MINUS (mode, tem, op0), tem);

      return tem;
    }
  return expmed_mult_highpart_optab (mode, op0, op1, target,
				     unsignedp, max_cost);
}


/* Expand signed modulus of OP0 by a power of two D in mode MODE.  */

static rtx
expand_smod_pow2 (enum machine_mode mode, rtx op0, HOST_WIDE_INT d)
{
  unsigned HOST_WIDE_INT masklow, maskhigh;
  rtx result, temp, shift, label;
  int logd;

  logd = floor_log2 (d);
  result = gen_reg_rtx (mode);

  /* Avoid conditional branches when they're expensive.  */
  if (BRANCH_COST (optimize_insn_for_speed_p (), false) >= 2
      && optimize_insn_for_speed_p ())
    {
      rtx signmask = emit_store_flag (result, LT, op0, const0_rtx,
				      mode, 0, -1);
      if (signmask)
	{
	  signmask = force_reg (mode, signmask);
	  masklow = ((HOST_WIDE_INT) 1 << logd) - 1;
	  shift = GEN_INT (GET_MODE_BITSIZE (mode) - logd);

	  /* Use the rtx_cost of a LSHIFTRT instruction to determine
	     which instruction sequence to use.  If logical right shifts
	     are expensive the use 2 XORs, 2 SUBs and an AND, otherwise
	     use a LSHIFTRT, 1 ADD, 1 SUB and an AND.  */

	  temp = gen_rtx_LSHIFTRT (mode, result, shift);
	  if (optab_handler (lshr_optab, mode) == CODE_FOR_nothing
	      || (set_src_cost (temp, optimize_insn_for_speed_p ())
		  > COSTS_N_INSNS (2)))
	    {
	      temp = expand_binop (mode, xor_optab, op0, signmask,
				   NULL_RTX, 1, OPTAB_LIB_WIDEN);
	      temp = expand_binop (mode, sub_optab, temp, signmask,
				   NULL_RTX, 1, OPTAB_LIB_WIDEN);
	      temp = expand_binop (mode, and_optab, temp,
				   gen_int_mode (masklow, mode),
				   NULL_RTX, 1, OPTAB_LIB_WIDEN);
	      temp = expand_binop (mode, xor_optab, temp, signmask,
				   NULL_RTX, 1, OPTAB_LIB_WIDEN);
	      temp = expand_binop (mode, sub_optab, temp, signmask,
				   NULL_RTX, 1, OPTAB_LIB_WIDEN);
	    }
	  else
	    {
	      signmask = expand_binop (mode, lshr_optab, signmask, shift,
				       NULL_RTX, 1, OPTAB_LIB_WIDEN);
	      signmask = force_reg (mode, signmask);

	      temp = expand_binop (mode, add_optab, op0, signmask,
				   NULL_RTX, 1, OPTAB_LIB_WIDEN);
	      temp = expand_binop (mode, and_optab, temp,
				   gen_int_mode (masklow, mode),
				   NULL_RTX, 1, OPTAB_LIB_WIDEN);
	      temp = expand_binop (mode, sub_optab, temp, signmask,
				   NULL_RTX, 1, OPTAB_LIB_WIDEN);
	    }
	  return temp;
	}
    }

  /* Mask contains the mode's signbit and the significant bits of the
     modulus.  By including the signbit in the operation, many targets
     can avoid an explicit compare operation in the following comparison
     against zero.  */

  masklow = ((HOST_WIDE_INT) 1 << logd) - 1;
  if (GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT)
    {
      masklow |= HOST_WIDE_INT_M1U << (GET_MODE_BITSIZE (mode) - 1);
      maskhigh = -1;
    }
  else
    maskhigh = HOST_WIDE_INT_M1U
		 << (GET_MODE_BITSIZE (mode) - HOST_BITS_PER_WIDE_INT - 1);

  temp = expand_binop (mode, and_optab, op0,
		       immed_double_const (masklow, maskhigh, mode),
		       result, 1, OPTAB_LIB_WIDEN);
  if (temp != result)
    emit_move_insn (result, temp);

  label = gen_label_rtx ();
  do_cmp_and_jump (result, const0_rtx, GE, mode, label);

  temp = expand_binop (mode, sub_optab, result, const1_rtx, result,
		       0, OPTAB_LIB_WIDEN);
  masklow = HOST_WIDE_INT_M1U << logd;
  maskhigh = -1;
  temp = expand_binop (mode, ior_optab, temp,
		       immed_double_const (masklow, maskhigh, mode),
		       result, 1, OPTAB_LIB_WIDEN);
  temp = expand_binop (mode, add_optab, temp, const1_rtx, result,
		       0, OPTAB_LIB_WIDEN);
  if (temp != result)
    emit_move_insn (result, temp);
  emit_label (label);
  return result;
}

/* Expand signed division of OP0 by a power of two D in mode MODE.
   This routine is only called for positive values of D.  */

static rtx
expand_sdiv_pow2 (enum machine_mode mode, rtx op0, HOST_WIDE_INT d)
{
  rtx temp, label;
  int logd;

  logd = floor_log2 (d);

  if (d == 2
      && BRANCH_COST (optimize_insn_for_speed_p (),
		      false) >= 1)
    {
      temp = gen_reg_rtx (mode);
      temp = emit_store_flag (temp, LT, op0, const0_rtx, mode, 0, 1);
      temp = expand_binop (mode, add_optab, temp, op0, NULL_RTX,
			   0, OPTAB_LIB_WIDEN);
      return expand_shift (RSHIFT_EXPR, mode, temp, logd, NULL_RTX, 0);
    }

#ifdef HAVE_conditional_move
  if (BRANCH_COST (optimize_insn_for_speed_p (), false)
      >= 2)
    {
      rtx temp2;

      /* ??? emit_conditional_move forces a stack adjustment via
	 compare_from_rtx so, if the sequence is discarded, it will
	 be lost.  Do it now instead.  */
      do_pending_stack_adjust ();

      start_sequence ();
      temp2 = copy_to_mode_reg (mode, op0);
      temp = expand_binop (mode, add_optab, temp2, gen_int_mode (d - 1, mode),
			   NULL_RTX, 0, OPTAB_LIB_WIDEN);
      temp = force_reg (mode, temp);

      /* Construct "temp2 = (temp2 < 0) ? temp : temp2".  */
      temp2 = emit_conditional_move (temp2, LT, temp2, const0_rtx,
				     mode, temp, temp2, mode, 0);
      if (temp2)
	{
	  rtx seq = get_insns ();
	  end_sequence ();
	  emit_insn (seq);
	  return expand_shift (RSHIFT_EXPR, mode, temp2, logd, NULL_RTX, 0);
	}
      end_sequence ();
    }
#endif

  if (BRANCH_COST (optimize_insn_for_speed_p (),
		   false) >= 2)
    {
      int ushift = GET_MODE_BITSIZE (mode) - logd;

      temp = gen_reg_rtx (mode);
      temp = emit_store_flag (temp, LT, op0, const0_rtx, mode, 0, -1);
      if (shift_cost (optimize_insn_for_speed_p (), mode, ushift)
	  > COSTS_N_INSNS (1))
	temp = expand_binop (mode, and_optab, temp, gen_int_mode (d - 1, mode),
			     NULL_RTX, 0, OPTAB_LIB_WIDEN);
      else
	temp = expand_shift (RSHIFT_EXPR, mode, temp,
			     ushift, NULL_RTX, 1);
      temp = expand_binop (mode, add_optab, temp, op0, NULL_RTX,
			   0, OPTAB_LIB_WIDEN);
      return expand_shift (RSHIFT_EXPR, mode, temp, logd, NULL_RTX, 0);
    }

  label = gen_label_rtx ();
  temp = copy_to_mode_reg (mode, op0);
  do_cmp_and_jump (temp, const0_rtx, GE, mode, label);
  expand_inc (temp, gen_int_mode (d - 1, mode));
  emit_label (label);
  return expand_shift (RSHIFT_EXPR, mode, temp, logd, NULL_RTX, 0);
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
   so that if a number is shifted by an integer multiple of the given
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

rtx
expand_divmod (int rem_flag, enum tree_code code, enum machine_mode mode,
	       rtx op0, rtx op1, rtx target, int unsignedp)
{
  enum machine_mode compute_mode;
  rtx tquotient;
  rtx quotient = 0, remainder = 0;
  rtx last;
  int size;
  rtx insn;
  optab optab1, optab2;
  int op1_is_constant, op1_is_pow2 = 0;
  int max_cost, extra_cost;
  static HOST_WIDE_INT last_div_const = 0;
  bool speed = optimize_insn_for_speed_p ();

  op1_is_constant = CONST_INT_P (op1);
  if (op1_is_constant)
    {
      unsigned HOST_WIDE_INT ext_op1 = UINTVAL (op1);
      if (unsignedp)
	ext_op1 &= GET_MODE_MASK (mode);
      op1_is_pow2 = ((EXACT_POWER_OF_2_OR_ZERO_P (ext_op1)
		     || (! unsignedp && EXACT_POWER_OF_2_OR_ZERO_P (-ext_op1))));
    }

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
     implemented in expmed_mult_highpart.

     If what we actually want is the remainder, we generate that by another
     by-constant multiplication and a subtraction.  */

  /* We shouldn't be called with OP1 == const1_rtx, but some of the
     code below will malfunction if we are, so check here and handle
     the special case if so.  */
  if (op1 == const1_rtx)
    return rem_flag ? const0_rtx : op0;

    /* When dividing by -1, we could get an overflow.
     negv_optab can handle overflows.  */
  if (! unsignedp && op1 == constm1_rtx)
    {
      if (rem_flag)
	return const0_rtx;
      return expand_unop (mode, flag_trapv && GET_MODE_CLASS(mode) == MODE_INT
			  ? negv_optab : neg_optab, op0, target, 0);
    }

  if (target
      /* Don't use the function value register as a target
	 since we have to read it as well as write it,
	 and function-inlining gets confused by this.  */
      && ((REG_P (target) && REG_FUNCTION_VALUE_P (target))
	  /* Don't clobber an operand while doing a multi-step calculation.  */
	  || ((rem_flag || op1_is_constant)
	      && (reg_mentioned_p (target, op0)
		  || (MEM_P (op0) && MEM_P (target))))
	  || reg_mentioned_p (target, op1)
	  || (MEM_P (op1) && MEM_P (target))))
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
     optimization.  Since expmed_mult_highpart tries so many variants, it is
     not straightforward to generalize this.  Maybe we should make an array
     of possible modes in init_expmed?  Save this for GCC 2.7.  */

  optab1 = ((op1_is_pow2 && op1 != const0_rtx)
	    ? (unsignedp ? lshr_optab : ashr_optab)
	    : (unsignedp ? udiv_optab : sdiv_optab));
  optab2 = ((op1_is_pow2 && op1 != const0_rtx)
	    ? optab1
	    : (unsignedp ? udivmod_optab : sdivmod_optab));

  for (compute_mode = mode; compute_mode != VOIDmode;
       compute_mode = GET_MODE_WIDER_MODE (compute_mode))
    if (optab_handler (optab1, compute_mode) != CODE_FOR_nothing
	|| optab_handler (optab2, compute_mode) != CODE_FOR_nothing)
      break;

  if (compute_mode == VOIDmode)
    for (compute_mode = mode; compute_mode != VOIDmode;
	 compute_mode = GET_MODE_WIDER_MODE (compute_mode))
      if (optab_libfunc (optab1, compute_mode)
	  || optab_libfunc (optab2, compute_mode))
	break;

  /* If we still couldn't find a mode, use MODE, but expand_binop will
     probably die.  */
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
  max_cost = (unsignedp 
	      ? udiv_cost (speed, compute_mode)
	      : sdiv_cost (speed, compute_mode));
  if (rem_flag && ! (last_div_const != 0 && op1_is_constant
		     && INTVAL (op1) == last_div_const))
    max_cost -= (mul_cost (speed, compute_mode)
		 + add_cost (speed, compute_mode));

  last_div_const = ! rem_flag && op1_is_constant ? INTVAL (op1) : 0;

  /* Now convert to the best mode to use.  */
  if (compute_mode != mode)
    {
      op0 = convert_modes (compute_mode, mode, op0, unsignedp);
      op1 = convert_modes (compute_mode, mode, op1, unsignedp);

      /* convert_modes may have placed op1 into a register, so we
	 must recompute the following.  */
      op1_is_constant = CONST_INT_P (op1);
      op1_is_pow2 = (op1_is_constant
		     && ((EXACT_POWER_OF_2_OR_ZERO_P (INTVAL (op1))
			  || (! unsignedp
			      && EXACT_POWER_OF_2_OR_ZERO_P (-UINTVAL (op1))))));
    }

  /* If one of the operands is a volatile MEM, copy it into a register.  */

  if (MEM_P (op0) && MEM_VOLATILE_P (op0))
    op0 = force_reg (compute_mode, op0);
  if (MEM_P (op1) && MEM_VOLATILE_P (op1))
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
		unsigned HOST_WIDE_INT d = (INTVAL (op1)
					    & GET_MODE_MASK (compute_mode));

		if (EXACT_POWER_OF_2_OR_ZERO_P (d))
		  {
		    pre_shift = floor_log2 (d);
		    if (rem_flag)
		      {
			unsigned HOST_WIDE_INT mask
			  = ((unsigned HOST_WIDE_INT) 1 << pre_shift) - 1;
			remainder
			  = expand_binop (compute_mode, and_optab, op0,
					  gen_int_mode (mask, compute_mode),
					  remainder, 1,
					  OPTAB_LIB_WIDEN);
			if (remainder)
			  return gen_lowpart (mode, remainder);
		      }
		    quotient = expand_shift (RSHIFT_EXPR, compute_mode, op0,
					     pre_shift, tquotient, 1);
		  }
		else if (size <= HOST_BITS_PER_WIDE_INT)
		  {
		    if (d >= ((unsigned HOST_WIDE_INT) 1 << (size - 1)))
		      {
			/* Most significant bit of divisor is set; emit an scc
			   insn.  */
			quotient = emit_store_flag_force (tquotient, GEU, op0, op1,
							  compute_mode, 1, 1);
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
			    gcc_assert (!mh);
			  }
			else
			  pre_shift = 0;

			if (mh != 0)
			  {
			    rtx t1, t2, t3, t4;

			    if (post_shift - 1 >= BITS_PER_WORD)
			      goto fail1;

			    extra_cost
			      = (shift_cost (speed, compute_mode, post_shift - 1)
				 + shift_cost (speed, compute_mode, 1)
				 + 2 * add_cost (speed, compute_mode));
			    t1 = expmed_mult_highpart
			      (compute_mode, op0,
			       gen_int_mode (ml, compute_mode),
			       NULL_RTX, 1, max_cost - extra_cost);
			    if (t1 == 0)
			      goto fail1;
			    t2 = force_operand (gen_rtx_MINUS (compute_mode,
							       op0, t1),
						NULL_RTX);
			    t3 = expand_shift (RSHIFT_EXPR, compute_mode,
					       t2, 1, NULL_RTX, 1);
			    t4 = force_operand (gen_rtx_PLUS (compute_mode,
							      t1, t3),
						NULL_RTX);
			    quotient = expand_shift
			      (RSHIFT_EXPR, compute_mode, t4,
			       post_shift - 1, tquotient, 1);
			  }
			else
			  {
			    rtx t1, t2;

			    if (pre_shift >= BITS_PER_WORD
				|| post_shift >= BITS_PER_WORD)
			      goto fail1;

			    t1 = expand_shift
			      (RSHIFT_EXPR, compute_mode, op0,
			       pre_shift, NULL_RTX, 1);
			    extra_cost
			      = (shift_cost (speed, compute_mode, pre_shift)
				 + shift_cost (speed, compute_mode, post_shift));
			    t2 = expmed_mult_highpart
			      (compute_mode, t1,
			       gen_int_mode (ml, compute_mode),
			       NULL_RTX, 1, max_cost - extra_cost);
			    if (t2 == 0)
			      goto fail1;
			    quotient = expand_shift
			      (RSHIFT_EXPR, compute_mode, t2,
			       post_shift, tquotient, 1);
			  }
		      }
		  }
		else		/* Too wide mode to use tricky code */
		  break;

		insn = get_last_insn ();
		if (insn != last)
		  set_dst_reg_note (insn, REG_EQUAL,
				    gen_rtx_UDIV (compute_mode, op0, op1),
				    quotient);
	      }
	    else		/* TRUNC_DIV, signed */
	      {
		unsigned HOST_WIDE_INT ml;
		int lgup, post_shift;
		rtx mlr;
		HOST_WIDE_INT d = INTVAL (op1);
		unsigned HOST_WIDE_INT abs_d;

		/* Since d might be INT_MIN, we have to cast to
		   unsigned HOST_WIDE_INT before negating to avoid
		   undefined signed overflow.  */
		abs_d = (d >= 0
			 ? (unsigned HOST_WIDE_INT) d
			 : - (unsigned HOST_WIDE_INT) d);

		/* n rem d = n rem -d */
		if (rem_flag && d < 0)
		  {
		    d = abs_d;
		    op1 = gen_int_mode (abs_d, compute_mode);
		  }

		if (d == 1)
		  quotient = op0;
		else if (d == -1)
		  quotient = expand_unop (compute_mode, neg_optab, op0,
					  tquotient, 0);
		else if (HOST_BITS_PER_WIDE_INT >= size
			 && abs_d == (unsigned HOST_WIDE_INT) 1 << (size - 1))
		  {
		    /* This case is not handled correctly below.  */
		    quotient = emit_store_flag (tquotient, EQ, op0, op1,
						compute_mode, 1, 1);
		    if (quotient == 0)
		      goto fail1;
		  }
		else if (EXACT_POWER_OF_2_OR_ZERO_P (d)
			 && (rem_flag
			     ? smod_pow2_cheap (speed, compute_mode)
			     : sdiv_pow2_cheap (speed, compute_mode))
			 /* We assume that cheap metric is true if the
			    optab has an expander for this mode.  */
			 && ((optab_handler ((rem_flag ? smod_optab
					      : sdiv_optab),
					     compute_mode)
			      != CODE_FOR_nothing)
			     || (optab_handler (sdivmod_optab,
						compute_mode)
				 != CODE_FOR_nothing)))
		  ;
		else if (EXACT_POWER_OF_2_OR_ZERO_P (abs_d))
		  {
		    if (rem_flag)
		      {
			remainder = expand_smod_pow2 (compute_mode, op0, d);
			if (remainder)
			  return gen_lowpart (mode, remainder);
		      }

		    if (sdiv_pow2_cheap (speed, compute_mode)
			&& ((optab_handler (sdiv_optab, compute_mode)
			     != CODE_FOR_nothing)
			    || (optab_handler (sdivmod_optab, compute_mode)
				!= CODE_FOR_nothing)))
		      quotient = expand_divmod (0, TRUNC_DIV_EXPR,
						compute_mode, op0,
						gen_int_mode (abs_d,
							      compute_mode),
						NULL_RTX, 0);
		    else
		      quotient = expand_sdiv_pow2 (compute_mode, op0, abs_d);

		    /* We have computed OP0 / abs(OP1).  If OP1 is negative,
		       negate the quotient.  */
		    if (d < 0)
		      {
			insn = get_last_insn ();
			if (insn != last
			    && abs_d < ((unsigned HOST_WIDE_INT) 1
					<< (HOST_BITS_PER_WIDE_INT - 1)))
			  set_dst_reg_note (insn, REG_EQUAL,
					    gen_rtx_DIV (compute_mode, op0,
							 gen_int_mode
							   (abs_d,
							    compute_mode)),
					    quotient);

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

			if (post_shift >= BITS_PER_WORD
			    || size - 1 >= BITS_PER_WORD)
			  goto fail1;

			extra_cost = (shift_cost (speed, compute_mode, post_shift)
				      + shift_cost (speed, compute_mode, size - 1)
				      + add_cost (speed, compute_mode));
			t1 = expmed_mult_highpart
			  (compute_mode, op0, gen_int_mode (ml, compute_mode),
			   NULL_RTX, 0, max_cost - extra_cost);
			if (t1 == 0)
			  goto fail1;
			t2 = expand_shift
			  (RSHIFT_EXPR, compute_mode, t1,
			   post_shift, NULL_RTX, 0);
			t3 = expand_shift
			  (RSHIFT_EXPR, compute_mode, op0,
			   size - 1, NULL_RTX, 0);
			if (d < 0)
			  quotient
			    = force_operand (gen_rtx_MINUS (compute_mode,
							    t3, t2),
					     tquotient);
			else
			  quotient
			    = force_operand (gen_rtx_MINUS (compute_mode,
							    t2, t3),
					     tquotient);
		      }
		    else
		      {
			rtx t1, t2, t3, t4;

			if (post_shift >= BITS_PER_WORD
			    || size - 1 >= BITS_PER_WORD)
			  goto fail1;

			ml |= (~(unsigned HOST_WIDE_INT) 0) << (size - 1);
			mlr = gen_int_mode (ml, compute_mode);
			extra_cost = (shift_cost (speed, compute_mode, post_shift)
				      + shift_cost (speed, compute_mode, size - 1)
				      + 2 * add_cost (speed, compute_mode));
			t1 = expmed_mult_highpart (compute_mode, op0, mlr,
						   NULL_RTX, 0,
						   max_cost - extra_cost);
			if (t1 == 0)
			  goto fail1;
			t2 = force_operand (gen_rtx_PLUS (compute_mode,
							  t1, op0),
					    NULL_RTX);
			t3 = expand_shift
			  (RSHIFT_EXPR, compute_mode, t2,
			   post_shift, NULL_RTX, 0);
			t4 = expand_shift
			  (RSHIFT_EXPR, compute_mode, op0,
			   size - 1, NULL_RTX, 0);
			if (d < 0)
			  quotient
			    = force_operand (gen_rtx_MINUS (compute_mode,
							    t4, t3),
					     tquotient);
			else
			  quotient
			    = force_operand (gen_rtx_MINUS (compute_mode,
							    t3, t4),
					     tquotient);
		      }
		  }
		else		/* Too wide mode to use tricky code */
		  break;

		insn = get_last_insn ();
		if (insn != last)
		  set_dst_reg_note (insn, REG_EQUAL,
				    gen_rtx_DIV (compute_mode, op0, op1),
				    quotient);
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
			unsigned HOST_WIDE_INT mask
			  = ((unsigned HOST_WIDE_INT) 1 << pre_shift) - 1;
			remainder = expand_binop
			  (compute_mode, and_optab, op0,
			   gen_int_mode (mask, compute_mode),
			   remainder, 0, OPTAB_LIB_WIDEN);
			if (remainder)
			  return gen_lowpart (mode, remainder);
		      }
		    quotient = expand_shift
		      (RSHIFT_EXPR, compute_mode, op0,
		       pre_shift, tquotient, 0);
		  }
		else
		  {
		    rtx t1, t2, t3, t4;

		    mh = choose_multiplier (d, size, size - 1,
					    &ml, &post_shift, &lgup);
		    gcc_assert (!mh);

		    if (post_shift < BITS_PER_WORD
			&& size - 1 < BITS_PER_WORD)
		      {
			t1 = expand_shift
			  (RSHIFT_EXPR, compute_mode, op0,
			   size - 1, NULL_RTX, 0);
			t2 = expand_binop (compute_mode, xor_optab, op0, t1,
					   NULL_RTX, 0, OPTAB_WIDEN);
			extra_cost = (shift_cost (speed, compute_mode, post_shift)
				      + shift_cost (speed, compute_mode, size - 1)
				      + 2 * add_cost (speed, compute_mode));
			t3 = expmed_mult_highpart
			  (compute_mode, t2, gen_int_mode (ml, compute_mode),
			   NULL_RTX, 1, max_cost - extra_cost);
			if (t3 != 0)
			  {
			    t4 = expand_shift
			      (RSHIFT_EXPR, compute_mode, t3,
			       post_shift, NULL_RTX, 1);
			    quotient = expand_binop (compute_mode, xor_optab,
						     t4, t1, tquotient, 0,
						     OPTAB_WIDEN);
			  }
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
		nsign = expand_shift
		  (RSHIFT_EXPR, compute_mode, t2,
		   size - 1, NULL_RTX, 0);
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
	      = REG_P (target) ? target : gen_reg_rtx (compute_mode);
	    quotient = gen_reg_rtx (compute_mode);
	  }
	else
	  {
	    quotient
	      = REG_P (target) ? target : gen_reg_rtx (compute_mode);
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
				   floor_log2 (d), tquotient, 1);
		t2 = expand_binop (compute_mode, and_optab, op0,
				   gen_int_mode (d - 1, compute_mode),
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
		remainder = (REG_P (target)
			     ? target : gen_reg_rtx (compute_mode));
		quotient = gen_reg_rtx (compute_mode);
	      }
	    else
	      {
		quotient = (REG_P (target)
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
				   floor_log2 (d), tquotient, 0);
		t2 = expand_binop (compute_mode, and_optab, op0,
				   gen_int_mode (d - 1, compute_mode),
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
		remainder= (REG_P (target)
			    ? target : gen_reg_rtx (compute_mode));
		quotient = gen_reg_rtx (compute_mode);
	      }
	    else
	      {
		quotient = (REG_P (target)
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
	    int pre_shift;
	    rtx t1;

	    pre_shift = floor_log2 (d & -d);
	    ml = invert_mod2n (d >> pre_shift, size);
	    t1 = expand_shift (RSHIFT_EXPR, compute_mode, op0,
			       pre_shift, NULL_RTX, unsignedp);
	    quotient = expand_mult (compute_mode, t1,
				    gen_int_mode (ml, compute_mode),
				    NULL_RTX, 1);

	    insn = get_last_insn ();
	    set_dst_reg_note (insn, REG_EQUAL,
			      gen_rtx_fmt_ee (unsignedp ? UDIV : DIV,
					      compute_mode, op0, op1),
			      quotient);
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
	    tem = plus_constant (compute_mode, op1, -1);
	    tem = expand_shift (RSHIFT_EXPR, compute_mode, tem, 1, NULL_RTX, 1);
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
	    abs_rem = expand_abs (compute_mode, remainder, NULL_RTX, 1, 0);
	    abs_op1 = expand_abs (compute_mode, op1, NULL_RTX, 1, 0);
	    tem = expand_shift (LSHIFT_EXPR, compute_mode, abs_rem,
				1, NULL_RTX, 1);
	    do_cmp_and_jump (tem, abs_op1, LTU, compute_mode, label);
	    tem = expand_binop (compute_mode, xor_optab, op0, op1,
				NULL_RTX, 0, OPTAB_WIDEN);
	    mask = expand_shift (RSHIFT_EXPR, compute_mode, tem,
				 size - 1, NULL_RTX, 0);
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
	gcc_unreachable ();
      }

  if (quotient == 0)
    {
      if (target && GET_MODE (target) != compute_mode)
	target = 0;

      if (rem_flag)
	{
	  /* Try to produce the remainder without producing the quotient.
	     If we seem to have a divmod pattern that does not require widening,
	     don't try widening here.  We should really have a WIDEN argument
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
				 ((optab_handler (optab2, compute_mode)
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
			     ((optab_handler (optab2, compute_mode)
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
	{
	  /* No divide instruction either.  Use library for remainder.  */
	  remainder = sign_expand_binop (compute_mode, umod_optab, smod_optab,
					 op0, op1, target,
					 unsignedp, OPTAB_LIB_WIDEN);
	  /* No remainder function.  Try a quotient-and-remainder
	     function, keeping the remainder.  */
	  if (!remainder)
	    {
	      remainder = gen_reg_rtx (compute_mode);
	      if (!expand_twoval_binop_libfunc
		  (unsignedp ? udivmod_optab : sdivmod_optab,
		   op0, op1,
		   NULL_RTX, remainder,
		   unsignedp ? UMOD : MOD))
		remainder = NULL_RTX;
	    }
	}
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
   Usually this is an VAR_DECL, if there is no obvious better choice.
   X may be an expression, however we only support those expressions
   generated by loop.c.  */

tree
make_tree (tree type, rtx x)
{
  tree t;

  switch (GET_CODE (x))
    {
    case CONST_INT:
      {
	HOST_WIDE_INT hi = 0;

	if (INTVAL (x) < 0
	    && !(TYPE_UNSIGNED (type)
		 && (GET_MODE_BITSIZE (TYPE_MODE (type))
		     < HOST_BITS_PER_WIDE_INT)))
	  hi = -1;

	t = build_int_cst_wide (type, INTVAL (x), hi);

	return t;
      }

    case CONST_DOUBLE:
      if (GET_MODE (x) == VOIDmode)
	t = build_int_cst_wide (type,
				CONST_DOUBLE_LOW (x), CONST_DOUBLE_HIGH (x));
      else
	{
	  REAL_VALUE_TYPE d;

	  REAL_VALUE_FROM_CONST_DOUBLE (d, x);
	  t = build_real (type, d);
	}

      return t;

    case CONST_VECTOR:
      {
	int units = CONST_VECTOR_NUNITS (x);
	tree itype = TREE_TYPE (type);
	tree *elts;
	int i;

	/* Build a tree with vector elements.  */
	elts = XALLOCAVEC (tree, units);
	for (i = units - 1; i >= 0; --i)
	  {
	    rtx elt = CONST_VECTOR_ELT (x, i);
	    elts[i] = make_tree (itype, elt);
	  }

	return build_vector (type, elts);
      }

    case PLUS:
      return fold_build2 (PLUS_EXPR, type, make_tree (type, XEXP (x, 0)),
			  make_tree (type, XEXP (x, 1)));

    case MINUS:
      return fold_build2 (MINUS_EXPR, type, make_tree (type, XEXP (x, 0)),
			  make_tree (type, XEXP (x, 1)));

    case NEG:
      return fold_build1 (NEGATE_EXPR, type, make_tree (type, XEXP (x, 0)));

    case MULT:
      return fold_build2 (MULT_EXPR, type, make_tree (type, XEXP (x, 0)),
			  make_tree (type, XEXP (x, 1)));

    case ASHIFT:
      return fold_build2 (LSHIFT_EXPR, type, make_tree (type, XEXP (x, 0)),
			  make_tree (type, XEXP (x, 1)));

    case LSHIFTRT:
      t = unsigned_type_for (type);
      return fold_convert (type, build2 (RSHIFT_EXPR, t,
			    		 make_tree (t, XEXP (x, 0)),
				    	 make_tree (type, XEXP (x, 1))));

    case ASHIFTRT:
      t = signed_type_for (type);
      return fold_convert (type, build2 (RSHIFT_EXPR, t,
					 make_tree (t, XEXP (x, 0)),
				    	 make_tree (type, XEXP (x, 1))));

    case DIV:
      if (TREE_CODE (type) != REAL_TYPE)
	t = signed_type_for (type);
      else
	t = type;

      return fold_convert (type, build2 (TRUNC_DIV_EXPR, t,
				    	 make_tree (t, XEXP (x, 0)),
				    	 make_tree (t, XEXP (x, 1))));
    case UDIV:
      t = unsigned_type_for (type);
      return fold_convert (type, build2 (TRUNC_DIV_EXPR, t,
				    	 make_tree (t, XEXP (x, 0)),
				    	 make_tree (t, XEXP (x, 1))));

    case SIGN_EXTEND:
    case ZERO_EXTEND:
      t = lang_hooks.types.type_for_mode (GET_MODE (XEXP (x, 0)),
					  GET_CODE (x) == ZERO_EXTEND);
      return fold_convert (type, make_tree (t, XEXP (x, 0)));

    case CONST:
      return make_tree (type, XEXP (x, 0));

    case SYMBOL_REF:
      t = SYMBOL_REF_DECL (x);
      if (t)
	return fold_convert (type, build_fold_addr_expr (t));
      /* else fall through.  */

    default:
      t = build_decl (RTL_LOCATION (x), VAR_DECL, NULL_TREE, type);

      /* If TYPE is a POINTER_TYPE, we might need to convert X from
	 address mode to pointer mode.  */
      if (POINTER_TYPE_P (type))
	x = convert_memory_address_addr_space
	      (TYPE_MODE (type), x, TYPE_ADDR_SPACE (TREE_TYPE (type)));

      /* Note that we do *not* use SET_DECL_RTL here, because we do not
	 want set_decl_rtl to go adjusting REG_ATTRS for this temporary.  */
      t->decl_with_rtl.rtl = x;

      return t;
    }
}

/* Compute the logical-and of OP0 and OP1, storing it in TARGET
   and returning TARGET.

   If TARGET is 0, a pseudo-register or constant is returned.  */

rtx
expand_and (enum machine_mode mode, rtx op0, rtx op1, rtx target)
{
  rtx tem = 0;

  if (GET_MODE (op0) == VOIDmode && GET_MODE (op1) == VOIDmode)
    tem = simplify_binary_operation (AND, mode, op0, op1);
  if (tem == 0)
    tem = expand_binop (mode, and_optab, op0, op1, target, 0, OPTAB_LIB_WIDEN);

  if (target == 0)
    target = tem;
  else if (tem != target)
    emit_move_insn (target, tem);
  return target;
}

/* Helper function for emit_store_flag.  */
static rtx
emit_cstore (rtx target, enum insn_code icode, enum rtx_code code,
	     enum machine_mode mode, enum machine_mode compare_mode,
	     int unsignedp, rtx x, rtx y, int normalizep,
	     enum machine_mode target_mode)
{
  struct expand_operand ops[4];
  rtx op0, last, comparison, subtarget;
  enum machine_mode result_mode = targetm.cstore_mode (icode);

  last = get_last_insn ();
  x = prepare_operand (icode, x, 2, mode, compare_mode, unsignedp);
  y = prepare_operand (icode, y, 3, mode, compare_mode, unsignedp);
  if (!x || !y)
    {
      delete_insns_since (last);
      return NULL_RTX;
    }

  if (target_mode == VOIDmode)
    target_mode = result_mode;
  if (!target)
    target = gen_reg_rtx (target_mode);

  comparison = gen_rtx_fmt_ee (code, result_mode, x, y);

  create_output_operand (&ops[0], optimize ? NULL_RTX : target, result_mode);
  create_fixed_operand (&ops[1], comparison);
  create_fixed_operand (&ops[2], x);
  create_fixed_operand (&ops[3], y);
  if (!maybe_expand_insn (icode, 4, ops))
    {
      delete_insns_since (last);
      return NULL_RTX;
    }
  subtarget = ops[0].value;

  /* If we are converting to a wider mode, first convert to
     TARGET_MODE, then normalize.  This produces better combining
     opportunities on machines that have a SIGN_EXTRACT when we are
     testing a single bit.  This mostly benefits the 68k.

     If STORE_FLAG_VALUE does not have the sign bit set when
     interpreted in MODE, we can do this conversion as unsigned, which
     is usually more efficient.  */
  if (GET_MODE_SIZE (target_mode) > GET_MODE_SIZE (result_mode))
    {
      convert_move (target, subtarget,
		    val_signbit_known_clear_p (result_mode,
					       STORE_FLAG_VALUE));
      op0 = target;
      result_mode = target_mode;
    }
  else
    op0 = subtarget;

  /* If we want to keep subexpressions around, don't reuse our last
     target.  */
  if (optimize)
    subtarget = 0;

  /* Now normalize to the proper value in MODE.  Sometimes we don't
     have to do anything.  */
  if (normalizep == 0 || normalizep == STORE_FLAG_VALUE)
    ;
  /* STORE_FLAG_VALUE might be the most negative number, so write
     the comparison this way to avoid a compiler-time warning.  */
  else if (- normalizep == STORE_FLAG_VALUE)
    op0 = expand_unop (result_mode, neg_optab, op0, subtarget, 0);

  /* We don't want to use STORE_FLAG_VALUE < 0 below since this makes
     it hard to use a value of just the sign bit due to ANSI integer
     constant typing rules.  */
  else if (val_signbit_known_set_p (result_mode, STORE_FLAG_VALUE))
    op0 = expand_shift (RSHIFT_EXPR, result_mode, op0,
			GET_MODE_BITSIZE (result_mode) - 1, subtarget,
			normalizep == 1);
  else
    {
      gcc_assert (STORE_FLAG_VALUE & 1);

      op0 = expand_and (result_mode, op0, const1_rtx, subtarget);
      if (normalizep == -1)
	op0 = expand_unop (result_mode, neg_optab, op0, op0, 0);
    }

  /* If we were converting to a smaller mode, do the conversion now.  */
  if (target_mode != result_mode)
    {
      convert_move (target, op0, 0);
      return target;
    }
  else
    return op0;
}


/* A subroutine of emit_store_flag only including "tricks" that do not
   need a recursive call.  These are kept separate to avoid infinite
   loops.  */

static rtx
emit_store_flag_1 (rtx target, enum rtx_code code, rtx op0, rtx op1,
		   enum machine_mode mode, int unsignedp, int normalizep,
		   enum machine_mode target_mode)
{
  rtx subtarget;
  enum insn_code icode;
  enum machine_mode compare_mode;
  enum mode_class mclass;
  enum rtx_code scode;
  rtx tem;

  if (unsignedp)
    code = unsigned_condition (code);
  scode = swap_condition (code);

  /* If one operand is constant, make it the second one.  Only do this
     if the other operand is not constant as well.  */

  if (swap_commutative_operands_p (op0, op1))
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

  /* If we are comparing a double-word integer with zero or -1, we can
     convert the comparison into one involving a single word.  */
  if (GET_MODE_BITSIZE (mode) == BITS_PER_WORD * 2
      && GET_MODE_CLASS (mode) == MODE_INT
      && (!MEM_P (op0) || ! MEM_VOLATILE_P (op0)))
    {
      if ((code == EQ || code == NE)
	  && (op1 == const0_rtx || op1 == constm1_rtx))
	{
	  rtx op00, op01;

	  /* Do a logical OR or AND of the two words and compare the
	     result.  */
	  op00 = simplify_gen_subreg (word_mode, op0, mode, 0);
	  op01 = simplify_gen_subreg (word_mode, op0, mode, UNITS_PER_WORD);
	  tem = expand_binop (word_mode,
			      op1 == const0_rtx ? ior_optab : and_optab,
			      op00, op01, NULL_RTX, unsignedp,
			      OPTAB_DIRECT);

	  if (tem != 0)
	    tem = emit_store_flag (NULL_RTX, code, tem, op1, word_mode,
				   unsignedp, normalizep);
	}
      else if ((code == LT || code == GE) && op1 == const0_rtx)
	{
	  rtx op0h;

	  /* If testing the sign bit, can just test on high word.  */
	  op0h = simplify_gen_subreg (word_mode, op0, mode,
				      subreg_highpart_offset (word_mode,
							      mode));
	  tem = emit_store_flag (NULL_RTX, code, op0h, op1, word_mode,
				 unsignedp, normalizep);
	}
      else
	tem = NULL_RTX;

      if (tem)
	{
	  if (target_mode == VOIDmode || GET_MODE (tem) == target_mode)
	    return tem;
	  if (!target)
	    target = gen_reg_rtx (target_mode);

	  convert_move (target, tem,
			!val_signbit_known_set_p (word_mode,
						  (normalizep ? normalizep
						   : STORE_FLAG_VALUE)));
	  return target;
	}
    }

  /* If this is A < 0 or A >= 0, we can do this by taking the ones
     complement of A (for GE) and shifting the sign bit to the low bit.  */
  if (op1 == const0_rtx && (code == LT || code == GE)
      && GET_MODE_CLASS (mode) == MODE_INT
      && (normalizep || STORE_FLAG_VALUE == 1
	  || val_signbit_p (mode, STORE_FLAG_VALUE)))
    {
      subtarget = target;

      if (!target)
	target_mode = mode;

      /* If the result is to be wider than OP0, it is best to convert it
	 first.  If it is to be narrower, it is *incorrect* to convert it
	 first.  */
      else if (GET_MODE_SIZE (target_mode) > GET_MODE_SIZE (mode))
	{
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
			    GET_MODE_BITSIZE (mode) - 1,
			    subtarget, normalizep != -1);

      if (mode != target_mode)
	op0 = convert_modes (target_mode, mode, op0, 0);

      return op0;
    }

  mclass = GET_MODE_CLASS (mode);
  for (compare_mode = mode; compare_mode != VOIDmode;
       compare_mode = GET_MODE_WIDER_MODE (compare_mode))
    {
     enum machine_mode optab_mode = mclass == MODE_CC ? CCmode : compare_mode;
     icode = optab_handler (cstore_optab, optab_mode);
     if (icode != CODE_FOR_nothing)
	{
	  do_pending_stack_adjust ();
	  tem = emit_cstore (target, icode, code, mode, compare_mode,
			     unsignedp, op0, op1, normalizep, target_mode);
	  if (tem)
	    return tem;

	  if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	    {
	      tem = emit_cstore (target, icode, scode, mode, compare_mode,
				 unsignedp, op1, op0, normalizep, target_mode);
	      if (tem)
	        return tem;
	    }
	  break;
	}
    }

  return 0;
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
emit_store_flag (rtx target, enum rtx_code code, rtx op0, rtx op1,
		 enum machine_mode mode, int unsignedp, int normalizep)
{
  enum machine_mode target_mode = target ? GET_MODE (target) : VOIDmode;
  enum rtx_code rcode;
  rtx subtarget;
  rtx tem, last, trueval;

  tem = emit_store_flag_1 (target, code, op0, op1, mode, unsignedp, normalizep,
			   target_mode);
  if (tem)
    return tem;

  /* If we reached here, we can't do this with a scc insn, however there
     are some comparisons that can be done in other ways.  Don't do any
     of these cases if branches are very cheap.  */
  if (BRANCH_COST (optimize_insn_for_speed_p (), false) == 0)
    return 0;

  /* See what we need to return.  We can only return a 1, -1, or the
     sign bit.  */

  if (normalizep == 0)
    {
      if (STORE_FLAG_VALUE == 1 || STORE_FLAG_VALUE == -1)
	normalizep = STORE_FLAG_VALUE;

      else if (val_signbit_p (mode, STORE_FLAG_VALUE))
	;
      else
	return 0;
    }

  last = get_last_insn ();

  /* If optimizing, use different pseudo registers for each insn, instead
     of reusing the same pseudo.  This leads to better CSE, but slows
     down the compiler, since there are more pseudos */
  subtarget = (!optimize
	       && (target_mode == mode)) ? target : NULL_RTX;
  trueval = GEN_INT (normalizep ? normalizep : STORE_FLAG_VALUE);

  /* For floating-point comparisons, try the reverse comparison or try
     changing the "orderedness" of the comparison.  */
  if (GET_MODE_CLASS (mode) == MODE_FLOAT)
    {
      enum rtx_code first_code;
      bool and_them;

      rcode = reverse_condition_maybe_unordered (code);
      if (can_compare_p (rcode, mode, ccp_store_flag)
          && (code == ORDERED || code == UNORDERED
	      || (! HONOR_NANS (mode) && (code == LTGT || code == UNEQ))
	      || (! HONOR_SNANS (mode) && (code == EQ || code == NE))))
	{
          int want_add = ((STORE_FLAG_VALUE == 1 && normalizep == -1)
		          || (STORE_FLAG_VALUE == -1 && normalizep == 1));

	  /* For the reverse comparison, use either an addition or a XOR.  */
          if (want_add
	      && rtx_cost (GEN_INT (normalizep), PLUS, 1,
			   optimize_insn_for_speed_p ()) == 0)
	    {
	      tem = emit_store_flag_1 (subtarget, rcode, op0, op1, mode, 0,
				       STORE_FLAG_VALUE, target_mode);
	      if (tem)
                return expand_binop (target_mode, add_optab, tem,
				     gen_int_mode (normalizep, target_mode),
				     target, 0, OPTAB_WIDEN);
	    }
          else if (!want_add
	           && rtx_cost (trueval, XOR, 1,
			        optimize_insn_for_speed_p ()) == 0)
	    {
	      tem = emit_store_flag_1 (subtarget, rcode, op0, op1, mode, 0,
				       normalizep, target_mode);
	      if (tem)
                return expand_binop (target_mode, xor_optab, tem, trueval,
				     target, INTVAL (trueval) >= 0, OPTAB_WIDEN);
	    }
	}

      delete_insns_since (last);

      /* Cannot split ORDERED and UNORDERED, only try the above trick.   */
      if (code == ORDERED || code == UNORDERED)
	return 0;

      and_them = split_comparison (code, mode, &first_code, &code);

      /* If there are no NaNs, the first comparison should always fall through.
         Effectively change the comparison to the other one.  */
      if (!HONOR_NANS (mode))
	{
          gcc_assert (first_code == (and_them ? ORDERED : UNORDERED));
	  return emit_store_flag_1 (target, code, op0, op1, mode, 0, normalizep,
				    target_mode);
	}

#ifdef HAVE_conditional_move
      /* Try using a setcc instruction for ORDERED/UNORDERED, followed by a
	 conditional move.  */
      tem = emit_store_flag_1 (subtarget, first_code, op0, op1, mode, 0,
			       normalizep, target_mode);
      if (tem == 0)
	return 0;

      if (and_them)
        tem = emit_conditional_move (target, code, op0, op1, mode,
				     tem, const0_rtx, GET_MODE (tem), 0);
      else
        tem = emit_conditional_move (target, code, op0, op1, mode,
				     trueval, tem, GET_MODE (tem), 0);

      if (tem == 0)
        delete_insns_since (last);
      return tem;
#else
      return 0;
#endif
    }

  /* The remaining tricks only apply to integer comparisons.  */

  if (GET_MODE_CLASS (mode) != MODE_INT)
    return 0;

  /* If this is an equality comparison of integers, we can try to exclusive-or
     (or subtract) the two operands and use a recursive call to try the
     comparison with zero.  Don't do any of these cases if branches are
     very cheap.  */

  if ((code == EQ || code == NE) && op1 != const0_rtx)
    {
      tem = expand_binop (mode, xor_optab, op0, op1, subtarget, 1,
			  OPTAB_WIDEN);

      if (tem == 0)
	tem = expand_binop (mode, sub_optab, op0, op1, subtarget, 1,
			    OPTAB_WIDEN);
      if (tem != 0)
	tem = emit_store_flag (target, code, tem, const0_rtx,
			       mode, unsignedp, normalizep);
      if (tem != 0)
	return tem;

      delete_insns_since (last);
    }

  /* For integer comparisons, try the reverse comparison.  However, for
     small X and if we'd have anyway to extend, implementing "X != 0"
     as "-(int)X >> 31" is still cheaper than inverting "(int)X == 0".  */
  rcode = reverse_condition (code);
  if (can_compare_p (rcode, mode, ccp_store_flag)
      && ! (optab_handler (cstore_optab, mode) == CODE_FOR_nothing
	    && code == NE
	    && GET_MODE_SIZE (mode) < UNITS_PER_WORD
	    && op1 == const0_rtx))
    {
      int want_add = ((STORE_FLAG_VALUE == 1 && normalizep == -1)
		      || (STORE_FLAG_VALUE == -1 && normalizep == 1));

      /* Again, for the reverse comparison, use either an addition or a XOR.  */
      if (want_add
	  && rtx_cost (GEN_INT (normalizep), PLUS, 1,
		       optimize_insn_for_speed_p ()) == 0)
	{
	  tem = emit_store_flag_1 (subtarget, rcode, op0, op1, mode, 0,
				   STORE_FLAG_VALUE, target_mode);
	  if (tem != 0)
            tem = expand_binop (target_mode, add_optab, tem,
				gen_int_mode (normalizep, target_mode),
				target, 0, OPTAB_WIDEN);
	}
      else if (!want_add
	       && rtx_cost (trueval, XOR, 1,
			    optimize_insn_for_speed_p ()) == 0)
	{
	  tem = emit_store_flag_1 (subtarget, rcode, op0, op1, mode, 0,
				   normalizep, target_mode);
	  if (tem != 0)
            tem = expand_binop (target_mode, xor_optab, tem, trueval, target,
				INTVAL (trueval) >= 0, OPTAB_WIDEN);
	}

      if (tem != 0)
	return tem;
      delete_insns_since (last);
    }

  /* Some other cases we can do are EQ, NE, LE, and GT comparisons with
     the constant zero.  Reject all other comparisons at this point.  Only
     do LE and GT if branches are expensive since they are expensive on
     2-operand machines.  */

  if (op1 != const0_rtx
      || (code != EQ && code != NE
	  && (BRANCH_COST (optimize_insn_for_speed_p (),
			   false) <= 1 || (code != LE && code != GT))))
    return 0;

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
			  GET_MODE_BITSIZE (mode) - 1,
			  subtarget, 0);
      tem = expand_binop (mode, sub_optab, tem, op0, subtarget, 0,
			  OPTAB_WIDEN);
    }

  if (code == EQ || code == NE)
    {
      /* For EQ or NE, one way to do the comparison is to apply an operation
	 that converts the operand into a positive number if it is nonzero
	 or zero if it was originally zero.  Then, for EQ, we subtract 1 and
	 for NE we negate.  This puts the result in the sign bit.  Then we
	 normalize with a shift, if needed.

	 Two operations that can do the above actions are ABS and FFS, so try
	 them.  If that doesn't work, and MODE is smaller than a full word,
	 we can use zero-extension to the wider mode (an unsigned conversion)
	 as the operation.  */

      /* Note that ABS doesn't yield a positive number for INT_MIN, but
	 that is compensated by the subsequent overflow when subtracting
	 one / negating.  */

      if (optab_handler (abs_optab, mode) != CODE_FOR_nothing)
	tem = expand_unop (mode, abs_optab, op0, subtarget, 1);
      else if (optab_handler (ffs_optab, mode) != CODE_FOR_nothing)
	tem = expand_unop (mode, ffs_optab, op0, subtarget, 1);
      else if (GET_MODE_SIZE (mode) < UNITS_PER_WORD)
	{
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

      if (tem == 0
	  && (code == NE
	      || BRANCH_COST (optimize_insn_for_speed_p (),
		      	      false) > 1))
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
			GET_MODE_BITSIZE (mode) - 1,
			subtarget, normalizep == 1);

  if (tem)
    {
      if (!target)
        ;
      else if (GET_MODE (tem) != target_mode)
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
emit_store_flag_force (rtx target, enum rtx_code code, rtx op0, rtx op1,
		       enum machine_mode mode, int unsignedp, int normalizep)
{
  rtx tem, label;
  rtx trueval, falseval;

  /* First see if emit_store_flag can do the job.  */
  tem = emit_store_flag (target, code, op0, op1, mode, unsignedp, normalizep);
  if (tem != 0)
    return tem;

  if (!target)
    target = gen_reg_rtx (word_mode);

  /* If this failed, we have to do this with set/compare/jump/set code.
     For foo != 0, if foo is in OP0, just replace it with 1 if nonzero.  */
  trueval = normalizep ? GEN_INT (normalizep) : const1_rtx;
  if (code == NE
      && GET_MODE_CLASS (mode) == MODE_INT
      && REG_P (target)
      && op0 == target
      && op1 == const0_rtx)
    {
      label = gen_label_rtx ();
      do_compare_rtx_and_jump (target, const0_rtx, EQ, unsignedp,
			       mode, NULL_RTX, NULL_RTX, label, -1);
      emit_move_insn (target, trueval);
      emit_label (label);
      return target;
    }

  if (!REG_P (target)
      || reg_mentioned_p (target, op0) || reg_mentioned_p (target, op1))
    target = gen_reg_rtx (GET_MODE (target));

  /* Jump in the right direction if the target cannot implement CODE
     but can jump on its reverse condition.  */
  falseval = const0_rtx;
  if (! can_compare_p (code, mode, ccp_jump)
      && (! FLOAT_MODE_P (mode)
          || code == ORDERED || code == UNORDERED
          || (! HONOR_NANS (mode) && (code == LTGT || code == UNEQ))
          || (! HONOR_SNANS (mode) && (code == EQ || code == NE))))
    {
      enum rtx_code rcode;
      if (FLOAT_MODE_P (mode))
        rcode = reverse_condition_maybe_unordered (code);
      else
        rcode = reverse_condition (code);

      /* Canonicalize to UNORDERED for the libcall.  */
      if (can_compare_p (rcode, mode, ccp_jump)
          || (code == ORDERED && ! can_compare_p (ORDERED, mode, ccp_jump)))
	{
	  falseval = trueval;
	  trueval = const0_rtx;
	  code = rcode;
	}
    }

  emit_move_insn (target, trueval);
  label = gen_label_rtx ();
  do_compare_rtx_and_jump (op0, op1, code, unsignedp, mode, NULL_RTX,
			   NULL_RTX, label, -1);

  emit_move_insn (target, falseval);
  emit_label (label);

  return target;
}

/* Perform possibly multi-word comparison and conditional jump to LABEL
   if ARG1 OP ARG2 true where ARG1 and ARG2 are of mode MODE.  This is
   now a thin wrapper around do_compare_rtx_and_jump.  */

static void
do_cmp_and_jump (rtx arg1, rtx arg2, enum rtx_code op, enum machine_mode mode,
		 rtx label)
{
  int unsignedp = (op == LTU || op == LEU || op == GTU || op == GEU);
  do_compare_rtx_and_jump (arg1, arg2, op, unsignedp, mode,
			   NULL_RTX, NULL_RTX, label, -1);
}
