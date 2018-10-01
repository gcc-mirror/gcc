/* Medium-level subroutines: convert bit-field store and extract
   and shifts, multiplies and divides to rtl instructions.
   Copyright (C) 1987-2018 Free Software Foundation, Inc.

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
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "predict.h"
#include "memmodel.h"
#include "tm_p.h"
#include "expmed.h"
#include "optabs.h"
#include "regs.h"
#include "emit-rtl.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "stor-layout.h"
#include "dojump.h"
#include "explow.h"
#include "expr.h"
#include "langhooks.h"
#include "tree-vector-builder.h"

struct target_expmed default_target_expmed;
#if SWITCHABLE_TARGET
struct target_expmed *this_target_expmed = &default_target_expmed;
#endif

static bool store_integral_bit_field (rtx, opt_scalar_int_mode,
				      unsigned HOST_WIDE_INT,
				      unsigned HOST_WIDE_INT,
				      poly_uint64, poly_uint64,
				      machine_mode, rtx, bool, bool);
static void store_fixed_bit_field (rtx, opt_scalar_int_mode,
				   unsigned HOST_WIDE_INT,
				   unsigned HOST_WIDE_INT,
				   poly_uint64, poly_uint64,
				   rtx, scalar_int_mode, bool);
static void store_fixed_bit_field_1 (rtx, scalar_int_mode,
				     unsigned HOST_WIDE_INT,
				     unsigned HOST_WIDE_INT,
				     rtx, scalar_int_mode, bool);
static void store_split_bit_field (rtx, opt_scalar_int_mode,
				   unsigned HOST_WIDE_INT,
				   unsigned HOST_WIDE_INT,
				   poly_uint64, poly_uint64,
				   rtx, scalar_int_mode, bool);
static rtx extract_integral_bit_field (rtx, opt_scalar_int_mode,
				       unsigned HOST_WIDE_INT,
				       unsigned HOST_WIDE_INT, int, rtx,
				       machine_mode, machine_mode, bool, bool);
static rtx extract_fixed_bit_field (machine_mode, rtx, opt_scalar_int_mode,
				    unsigned HOST_WIDE_INT,
				    unsigned HOST_WIDE_INT, rtx, int, bool);
static rtx extract_fixed_bit_field_1 (machine_mode, rtx, scalar_int_mode,
				      unsigned HOST_WIDE_INT,
				      unsigned HOST_WIDE_INT, rtx, int, bool);
static rtx lshift_value (machine_mode, unsigned HOST_WIDE_INT, int);
static rtx extract_split_bit_field (rtx, opt_scalar_int_mode,
				    unsigned HOST_WIDE_INT,
				    unsigned HOST_WIDE_INT, int, bool);
static void do_cmp_and_jump (rtx, rtx, enum rtx_code, machine_mode, rtx_code_label *);
static rtx expand_smod_pow2 (scalar_int_mode, rtx, HOST_WIDE_INT);
static rtx expand_sdiv_pow2 (scalar_int_mode, rtx, HOST_WIDE_INT);

/* Return a constant integer mask value of mode MODE with BITSIZE ones
   followed by BITPOS zeros, or the complement of that if COMPLEMENT.
   The mask is truncated if necessary to the width of mode MODE.  The
   mask is zero-extended if BITSIZE+BITPOS is too small for MODE.  */

static inline rtx
mask_rtx (scalar_int_mode mode, int bitpos, int bitsize, bool complement)
{
  return immed_wide_int_const
    (wi::shifted_mask (bitpos, bitsize, complement,
		       GET_MODE_PRECISION (mode)), mode);
}

/* Test whether a value is zero of a power of two.  */
#define EXACT_POWER_OF_2_OR_ZERO_P(x) \
  (((x) & ((x) - HOST_WIDE_INT_1U)) == 0)

struct init_expmed_rtl
{
  rtx reg;
  rtx plus;
  rtx neg;
  rtx mult;
  rtx sdiv;
  rtx udiv;
  rtx sdiv_32;
  rtx smod_32;
  rtx wide_mult;
  rtx wide_lshr;
  rtx wide_trunc;
  rtx shift;
  rtx shift_mult;
  rtx shift_add;
  rtx shift_sub0;
  rtx shift_sub1;
  rtx zext;
  rtx trunc;

  rtx pow2[MAX_BITS_PER_WORD];
  rtx cint[MAX_BITS_PER_WORD];
};

static void
init_expmed_one_conv (struct init_expmed_rtl *all, scalar_int_mode to_mode,
		      scalar_int_mode from_mode, bool speed)
{
  int to_size, from_size;
  rtx which;

  to_size = GET_MODE_PRECISION (to_mode);
  from_size = GET_MODE_PRECISION (from_mode);

  /* Most partial integers have a precision less than the "full"
     integer it requires for storage.  In case one doesn't, for
     comparison purposes here, reduce the bit size by one in that
     case.  */
  if (GET_MODE_CLASS (to_mode) == MODE_PARTIAL_INT
      && pow2p_hwi (to_size))
    to_size --;
  if (GET_MODE_CLASS (from_mode) == MODE_PARTIAL_INT
      && pow2p_hwi (from_size))
    from_size --;
  
  /* Assume cost of zero-extend and sign-extend is the same.  */
  which = (to_size < from_size ? all->trunc : all->zext);

  PUT_MODE (all->reg, from_mode);
  set_convert_cost (to_mode, from_mode, speed,
		    set_src_cost (which, to_mode, speed));
}

static void
init_expmed_one_mode (struct init_expmed_rtl *all,
		      machine_mode mode, int speed)
{
  int m, n, mode_bitsize;
  machine_mode mode_from;

  mode_bitsize = GET_MODE_UNIT_BITSIZE (mode);

  PUT_MODE (all->reg, mode);
  PUT_MODE (all->plus, mode);
  PUT_MODE (all->neg, mode);
  PUT_MODE (all->mult, mode);
  PUT_MODE (all->sdiv, mode);
  PUT_MODE (all->udiv, mode);
  PUT_MODE (all->sdiv_32, mode);
  PUT_MODE (all->smod_32, mode);
  PUT_MODE (all->wide_trunc, mode);
  PUT_MODE (all->shift, mode);
  PUT_MODE (all->shift_mult, mode);
  PUT_MODE (all->shift_add, mode);
  PUT_MODE (all->shift_sub0, mode);
  PUT_MODE (all->shift_sub1, mode);
  PUT_MODE (all->zext, mode);
  PUT_MODE (all->trunc, mode);

  set_add_cost (speed, mode, set_src_cost (all->plus, mode, speed));
  set_neg_cost (speed, mode, set_src_cost (all->neg, mode, speed));
  set_mul_cost (speed, mode, set_src_cost (all->mult, mode, speed));
  set_sdiv_cost (speed, mode, set_src_cost (all->sdiv, mode, speed));
  set_udiv_cost (speed, mode, set_src_cost (all->udiv, mode, speed));

  set_sdiv_pow2_cheap (speed, mode, (set_src_cost (all->sdiv_32, mode, speed)
				     <= 2 * add_cost (speed, mode)));
  set_smod_pow2_cheap (speed, mode, (set_src_cost (all->smod_32, mode, speed)
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
      XEXP (all->shift, 1) = all->cint[m];
      XEXP (all->shift_mult, 1) = all->pow2[m];

      set_shift_cost (speed, mode, m, set_src_cost (all->shift, mode, speed));
      set_shiftadd_cost (speed, mode, m, set_src_cost (all->shift_add, mode,
						       speed));
      set_shiftsub0_cost (speed, mode, m, set_src_cost (all->shift_sub0, mode,
							speed));
      set_shiftsub1_cost (speed, mode, m, set_src_cost (all->shift_sub1, mode,
							speed));
    }

  scalar_int_mode int_mode_to;
  if (is_a <scalar_int_mode> (mode, &int_mode_to))
    {
      for (mode_from = MIN_MODE_INT; mode_from <= MAX_MODE_INT;
	   mode_from = (machine_mode)(mode_from + 1))
	init_expmed_one_conv (all, int_mode_to,
			      as_a <scalar_int_mode> (mode_from), speed);

      scalar_int_mode wider_mode;
      if (GET_MODE_CLASS (int_mode_to) == MODE_INT
	  && GET_MODE_WIDER_MODE (int_mode_to).exists (&wider_mode))
	{
	  PUT_MODE (all->zext, wider_mode);
	  PUT_MODE (all->wide_mult, wider_mode);
	  PUT_MODE (all->wide_lshr, wider_mode);
	  XEXP (all->wide_lshr, 1)
	    = gen_int_shift_amount (wider_mode, mode_bitsize);

	  set_mul_widen_cost (speed, wider_mode,
			      set_src_cost (all->wide_mult, wider_mode, speed));
	  set_mul_highpart_cost (speed, int_mode_to,
				 set_src_cost (all->wide_trunc,
					       int_mode_to, speed));
	}
    }
}

void
init_expmed (void)
{
  struct init_expmed_rtl all;
  machine_mode mode = QImode;
  int m, speed;

  memset (&all, 0, sizeof all);
  for (m = 1; m < MAX_BITS_PER_WORD; m++)
    {
      all.pow2[m] = GEN_INT (HOST_WIDE_INT_1 << m);
      all.cint[m] = GEN_INT (m);
    }

  /* Avoid using hard regs in ways which may be unsupported.  */
  all.reg = gen_raw_REG (mode, LAST_VIRTUAL_REGISTER + 1);
  all.plus = gen_rtx_PLUS (mode, all.reg, all.reg);
  all.neg = gen_rtx_NEG (mode, all.reg);
  all.mult = gen_rtx_MULT (mode, all.reg, all.reg);
  all.sdiv = gen_rtx_DIV (mode, all.reg, all.reg);
  all.udiv = gen_rtx_UDIV (mode, all.reg, all.reg);
  all.sdiv_32 = gen_rtx_DIV (mode, all.reg, all.pow2[5]);
  all.smod_32 = gen_rtx_MOD (mode, all.reg, all.pow2[5]);
  all.zext = gen_rtx_ZERO_EXTEND (mode, all.reg);
  all.wide_mult = gen_rtx_MULT (mode, all.zext, all.zext);
  all.wide_lshr = gen_rtx_LSHIFTRT (mode, all.wide_mult, all.reg);
  all.wide_trunc = gen_rtx_TRUNCATE (mode, all.wide_lshr);
  all.shift = gen_rtx_ASHIFT (mode, all.reg, all.reg);
  all.shift_mult = gen_rtx_MULT (mode, all.reg, all.reg);
  all.shift_add = gen_rtx_PLUS (mode, all.shift_mult, all.reg);
  all.shift_sub0 = gen_rtx_MINUS (mode, all.shift_mult, all.reg);
  all.shift_sub1 = gen_rtx_MINUS (mode, all.reg, all.shift_mult);
  all.trunc = gen_rtx_TRUNCATE (mode, all.reg);

  for (speed = 0; speed < 2; speed++)
    {
      crtl->maybe_hot_insn_p = speed;
      set_zero_cost (speed, set_src_cost (const0_rtx, mode, speed));

      for (mode = MIN_MODE_INT; mode <= MAX_MODE_INT;
	   mode = (machine_mode)(mode + 1))
	init_expmed_one_mode (&all, mode, speed);

      if (MIN_MODE_PARTIAL_INT != VOIDmode)
	for (mode = MIN_MODE_PARTIAL_INT; mode <= MAX_MODE_PARTIAL_INT;
	     mode = (machine_mode)(mode + 1))
	  init_expmed_one_mode (&all, mode, speed);

      if (MIN_MODE_VECTOR_INT != VOIDmode)
	for (mode = MIN_MODE_VECTOR_INT; mode <= MAX_MODE_VECTOR_INT;
	     mode = (machine_mode)(mode + 1))
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

  ggc_free (all.trunc);
  ggc_free (all.shift_sub1);
  ggc_free (all.shift_sub0);
  ggc_free (all.shift_add);
  ggc_free (all.shift_mult);
  ggc_free (all.shift);
  ggc_free (all.wide_trunc);
  ggc_free (all.wide_lshr);
  ggc_free (all.wide_mult);
  ggc_free (all.zext);
  ggc_free (all.smod_32);
  ggc_free (all.sdiv_32);
  ggc_free (all.udiv);
  ggc_free (all.sdiv);
  ggc_free (all.mult);
  ggc_free (all.neg);
  ggc_free (all.plus);
  ggc_free (all.reg);
}

/* Return an rtx representing minus the value of X.
   MODE is the intended mode of the result,
   useful if X is a CONST_INT.  */

rtx
negate_rtx (machine_mode mode, rtx x)
{
  rtx result = simplify_unary_operation (NEG, mode, x, mode);

  if (result == 0)
    result = expand_unop (mode, neg_optab, x, NULL_RTX, 0);

  return result;
}

/* Whether reverse storage order is supported on the target.  */
static int reverse_storage_order_supported = -1;

/* Check whether reverse storage order is supported on the target.  */

static void
check_reverse_storage_order_support (void)
{
  if (BYTES_BIG_ENDIAN != WORDS_BIG_ENDIAN)
    {
      reverse_storage_order_supported = 0;
      sorry ("reverse scalar storage order");
    }
  else
    reverse_storage_order_supported = 1;
}

/* Whether reverse FP storage order is supported on the target.  */
static int reverse_float_storage_order_supported = -1;

/* Check whether reverse FP storage order is supported on the target.  */

static void
check_reverse_float_storage_order_support (void)
{
  if (FLOAT_WORDS_BIG_ENDIAN != WORDS_BIG_ENDIAN)
    {
      reverse_float_storage_order_supported = 0;
      sorry ("reverse floating-point scalar storage order");
    }
  else
    reverse_float_storage_order_supported = 1;
}

/* Return an rtx representing value of X with reverse storage order.
   MODE is the intended mode of the result,
   useful if X is a CONST_INT.  */

rtx
flip_storage_order (machine_mode mode, rtx x)
{
  scalar_int_mode int_mode;
  rtx result;

  if (mode == QImode)
    return x;

  if (COMPLEX_MODE_P (mode))
    {
      rtx real = read_complex_part (x, false);
      rtx imag = read_complex_part (x, true);

      real = flip_storage_order (GET_MODE_INNER (mode), real);
      imag = flip_storage_order (GET_MODE_INNER (mode), imag);

      return gen_rtx_CONCAT (mode, real, imag);
    }

  if (__builtin_expect (reverse_storage_order_supported < 0, 0))
    check_reverse_storage_order_support ();

  if (!is_a <scalar_int_mode> (mode, &int_mode))
    {
      if (FLOAT_MODE_P (mode)
	  && __builtin_expect (reverse_float_storage_order_supported < 0, 0))
	check_reverse_float_storage_order_support ();

      if (!int_mode_for_size (GET_MODE_PRECISION (mode), 0).exists (&int_mode))
	{
	  sorry ("reverse storage order for %smode", GET_MODE_NAME (mode));
	  return x;
	}
      x = gen_lowpart (int_mode, x);
    }

  result = simplify_unary_operation (BSWAP, int_mode, x, int_mode);
  if (result == 0)
    result = expand_unop (int_mode, bswap_optab, x, NULL_RTX, 1);

  if (int_mode != mode)
    result = gen_lowpart (mode, result);

  return result;
}

/* If MODE is set, adjust bitfield memory MEM so that it points to the
   first unit of mode MODE that contains a bitfield of size BITSIZE at
   bit position BITNUM.  If MODE is not set, return a BLKmode reference
   to every byte in the bitfield.  Set *NEW_BITNUM to the bit position
   of the field within the new memory.  */

static rtx
narrow_bit_field_mem (rtx mem, opt_scalar_int_mode mode,
		      unsigned HOST_WIDE_INT bitsize,
		      unsigned HOST_WIDE_INT bitnum,
		      unsigned HOST_WIDE_INT *new_bitnum)
{
  scalar_int_mode imode;
  if (mode.exists (&imode))
    {
      unsigned int unit = GET_MODE_BITSIZE (imode);
      *new_bitnum = bitnum % unit;
      HOST_WIDE_INT offset = (bitnum - *new_bitnum) / BITS_PER_UNIT;
      return adjust_bitfield_address (mem, imode, offset);
    }
  else
    {
      *new_bitnum = bitnum % BITS_PER_UNIT;
      HOST_WIDE_INT offset = bitnum / BITS_PER_UNIT;
      HOST_WIDE_INT size = ((*new_bitnum + bitsize + BITS_PER_UNIT - 1)
			    / BITS_PER_UNIT);
      return adjust_bitfield_address_size (mem, BLKmode, offset, size);
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
			      poly_uint64 bitregion_start,
			      poly_uint64 bitregion_end,
			      machine_mode fieldmode,
			      unsigned HOST_WIDE_INT *new_bitnum)
{
  bit_field_mode_iterator iter (bitsize, bitnum, bitregion_start,
				bitregion_end, MEM_ALIGN (op0),
				MEM_VOLATILE_P (op0));
  scalar_int_mode best_mode;
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
	  scalar_int_mode limit_mode = word_mode;
	  extraction_insn insn;
	  if (get_best_reg_extraction_insn (&insn, pattern,
					    GET_MODE_BITSIZE (best_mode),
					    fieldmode))
	    limit_mode = insn.field_mode;

	  scalar_int_mode wider_mode;
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
lowpart_bit_field_p (poly_uint64 bitnum, poly_uint64 bitsize,
		     machine_mode struct_mode)
{
  poly_uint64 regsize = REGMODE_NATURAL_SIZE (struct_mode);
  if (BYTES_BIG_ENDIAN)
    return (multiple_p (bitnum, BITS_PER_UNIT)
	    && (known_eq (bitnum + bitsize, GET_MODE_BITSIZE (struct_mode))
		|| multiple_p (bitnum + bitsize,
			       regsize * BITS_PER_UNIT)));
  else
    return multiple_p (bitnum, regsize * BITS_PER_UNIT);
}

/* Return true if -fstrict-volatile-bitfields applies to an access of OP0
   containing BITSIZE bits starting at BITNUM, with field mode FIELDMODE.
   Return false if the access would touch memory outside the range
   BITREGION_START to BITREGION_END for conformance to the C++ memory
   model.  */

static bool
strict_volatile_bitfield_p (rtx op0, unsigned HOST_WIDE_INT bitsize,
			    unsigned HOST_WIDE_INT bitnum,
			    scalar_int_mode fieldmode,
			    poly_uint64 bitregion_start,
			    poly_uint64 bitregion_end)
{
  unsigned HOST_WIDE_INT modesize = GET_MODE_BITSIZE (fieldmode);

  /* -fstrict-volatile-bitfields must be enabled and we must have a
     volatile MEM.  */
  if (!MEM_P (op0)
      || !MEM_VOLATILE_P (op0)
      || flag_strict_volatile_bitfields <= 0)
    return false;

  /* The bit size must not be larger than the field mode, and
     the field mode must not be larger than a word.  */
  if (bitsize > modesize || modesize > BITS_PER_WORD)
    return false;

  /* Check for cases of unaligned fields that must be split.  */
  if (bitnum % modesize + bitsize > modesize)
    return false;

  /* The memory must be sufficiently aligned for a MODESIZE access.
     This condition guarantees, that the memory access will not
     touch anything after the end of the structure.  */
  if (MEM_ALIGN (op0) < modesize)
    return false;

  /* Check for cases where the C++ memory model applies.  */
  if (maybe_ne (bitregion_end, 0U)
      && (maybe_lt (bitnum - bitnum % modesize, bitregion_start)
	  || maybe_gt (bitnum - bitnum % modesize + modesize - 1,
		       bitregion_end)))
    return false;

  return true;
}

/* Return true if OP is a memory and if a bitfield of size BITSIZE at
   bit number BITNUM can be treated as a simple value of mode MODE.
   Store the byte offset in *BYTENUM if so.  */

static bool
simple_mem_bitfield_p (rtx op0, poly_uint64 bitsize, poly_uint64 bitnum,
		       machine_mode mode, poly_uint64 *bytenum)
{
  return (MEM_P (op0)
	  && multiple_p (bitnum, BITS_PER_UNIT, bytenum)
	  && known_eq (bitsize, GET_MODE_BITSIZE (mode))
	  && (!targetm.slow_unaligned_access (mode, MEM_ALIGN (op0))
	      || (multiple_p (bitnum, GET_MODE_ALIGNMENT (mode))
		  && MEM_ALIGN (op0) >= GET_MODE_ALIGNMENT (mode))));
}

/* Try to use instruction INSV to store VALUE into a field of OP0.
   If OP0_MODE is defined, it is the mode of OP0, otherwise OP0 is a
   BLKmode MEM.  VALUE_MODE is the mode of VALUE.  BITSIZE and BITNUM
   are as for store_bit_field.  */

static bool
store_bit_field_using_insv (const extraction_insn *insv, rtx op0,
			    opt_scalar_int_mode op0_mode,
			    unsigned HOST_WIDE_INT bitsize,
			    unsigned HOST_WIDE_INT bitnum,
			    rtx value, scalar_int_mode value_mode)
{
  struct expand_operand ops[4];
  rtx value1;
  rtx xop0 = op0;
  rtx_insn *last = get_last_insn ();
  bool copy_back = false;

  scalar_int_mode op_mode = insv->field_mode;
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
	bitnum += unit - GET_MODE_BITSIZE (op0_mode.require ());

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

  /* There are similar overflow check at the start of store_bit_field_1,
     but that only check the situation where the field lies completely
     outside the register, while there do have situation where the field
     lies partialy in the register, we need to adjust bitsize for this
     partial overflow situation.  Without this fix, pr48335-2.c on big-endian
     will broken on those arch support bit insert instruction, like arm, aarch64
     etc.  */
  if (bitsize + bitnum > unit && bitnum < unit)
    {
      warning (OPT_Wextra, "write of %wu-bit data outside the bound of "
	       "destination object, data truncated into %wu-bit",
	       bitsize, unit - bitnum);
      bitsize = unit - bitnum;
    }

  /* If BITS_BIG_ENDIAN is zero on a BYTES_BIG_ENDIAN machine, we count
     "backwards" from the size of the unit we are inserting into.
     Otherwise, we count bits from the most significant on a
     BYTES/BITS_BIG_ENDIAN machine.  */

  if (BITS_BIG_ENDIAN != BYTES_BIG_ENDIAN)
    bitnum = unit - bitsize - bitnum;

  /* Convert VALUE to op_mode (which insv insn wants) in VALUE1.  */
  value1 = value;
  if (value_mode != op_mode)
    {
      if (GET_MODE_BITSIZE (value_mode) >= bitsize)
	{
	  rtx tmp;
	  /* Optimization: Don't bother really extending VALUE
	     if it has all the bits we will actually use.  However,
	     if we must narrow it, be sure we do it correctly.  */

	  if (GET_MODE_SIZE (value_mode) < GET_MODE_SIZE (op_mode))
	    {
	      tmp = simplify_subreg (op_mode, value1, value_mode, 0);
	      if (! tmp)
		tmp = simplify_gen_subreg (op_mode,
					   force_reg (value_mode, value1),
					   value_mode, 0);
	    }
	  else
	    {
	      tmp = gen_lowpart_if_possible (op_mode, value1);
	      if (! tmp)
		tmp = gen_lowpart (op_mode, force_reg (value_mode, value1));
	    }
	  value1 = tmp;
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
store_bit_field_1 (rtx str_rtx, poly_uint64 bitsize, poly_uint64 bitnum,
		   poly_uint64 bitregion_start, poly_uint64 bitregion_end,
		   machine_mode fieldmode,
		   rtx value, bool reverse, bool fallback_p)
{
  rtx op0 = str_rtx;

  while (GET_CODE (op0) == SUBREG)
    {
      bitnum += subreg_memory_offset (op0) * BITS_PER_UNIT;
      op0 = SUBREG_REG (op0);
    }

  /* No action is needed if the target is a register and if the field
     lies completely outside that register.  This can occur if the source
     code contains an out-of-bounds access to a small array.  */
  if (REG_P (op0) && known_ge (bitnum, GET_MODE_BITSIZE (GET_MODE (op0))))
    return true;

  /* Use vec_set patterns for inserting parts of vectors whenever
     available.  */
  machine_mode outermode = GET_MODE (op0);
  scalar_mode innermode = GET_MODE_INNER (outermode);
  poly_uint64 pos;
  if (VECTOR_MODE_P (outermode)
      && !MEM_P (op0)
      && optab_handler (vec_set_optab, outermode) != CODE_FOR_nothing
      && fieldmode == innermode
      && known_eq (bitsize, GET_MODE_BITSIZE (innermode))
      && multiple_p (bitnum, GET_MODE_BITSIZE (innermode), &pos))
    {
      struct expand_operand ops[3];
      enum insn_code icode = optab_handler (vec_set_optab, outermode);

      create_fixed_operand (&ops[0], op0);
      create_input_operand (&ops[1], value, innermode);
      create_integer_operand (&ops[2], pos);
      if (maybe_expand_insn (icode, 3, ops))
	return true;
    }

  /* If the target is a register, overwriting the entire object, or storing
     a full-word or multi-word field can be done with just a SUBREG.  */
  if (!MEM_P (op0)
      && known_eq (bitsize, GET_MODE_BITSIZE (fieldmode)))
    {
      /* Use the subreg machinery either to narrow OP0 to the required
	 words or to cope with mode punning between equal-sized modes.
	 In the latter case, use subreg on the rhs side, not lhs.  */
      rtx sub;
      HOST_WIDE_INT regnum;
      poly_uint64 regsize = REGMODE_NATURAL_SIZE (GET_MODE (op0));
      if (known_eq (bitnum, 0U)
	  && known_eq (bitsize, GET_MODE_BITSIZE (GET_MODE (op0))))
	{
	  sub = simplify_gen_subreg (GET_MODE (op0), value, fieldmode, 0);
	  if (sub)
	    {
	      if (reverse)
		sub = flip_storage_order (GET_MODE (op0), sub);
	      emit_move_insn (op0, sub);
	      return true;
	    }
	}
      else if (constant_multiple_p (bitnum, regsize * BITS_PER_UNIT, &regnum)
	       && multiple_p (bitsize, regsize * BITS_PER_UNIT))
	{
	  sub = simplify_gen_subreg (fieldmode, op0, GET_MODE (op0),
				     regnum * regsize);
	  if (sub)
	    {
	      if (reverse)
		value = flip_storage_order (fieldmode, value);
	      emit_move_insn (sub, value);
	      return true;
	    }
	}
    }

  /* If the target is memory, storing any naturally aligned field can be
     done with a simple store.  For targets that support fast unaligned
     memory, any naturally sized, unit aligned field can be done directly.  */
  poly_uint64 bytenum;
  if (simple_mem_bitfield_p (op0, bitsize, bitnum, fieldmode, &bytenum))
    {
      op0 = adjust_bitfield_address (op0, fieldmode, bytenum);
      if (reverse)
	value = flip_storage_order (fieldmode, value);
      emit_move_insn (op0, value);
      return true;
    }

  /* It's possible we'll need to handle other cases here for
     polynomial bitnum and bitsize.  */

  /* From here on we need to be looking at a fixed-size insertion.  */
  unsigned HOST_WIDE_INT ibitsize = bitsize.to_constant ();
  unsigned HOST_WIDE_INT ibitnum = bitnum.to_constant ();

  /* Make sure we are playing with integral modes.  Pun with subregs
     if we aren't.  This must come after the entire register case above,
     since that case is valid for any mode.  The following cases are only
     valid for integral modes.  */
  opt_scalar_int_mode op0_mode = int_mode_for_mode (GET_MODE (op0));
  scalar_int_mode imode;
  if (!op0_mode.exists (&imode) || imode != GET_MODE (op0))
    {
      if (MEM_P (op0))
	op0 = adjust_bitfield_address_size (op0, op0_mode.else_blk (),
					    0, MEM_SIZE (op0));
      else
	op0 = gen_lowpart (op0_mode.require (), op0);
    }

  return store_integral_bit_field (op0, op0_mode, ibitsize, ibitnum,
				   bitregion_start, bitregion_end,
				   fieldmode, value, reverse, fallback_p);
}

/* Subroutine of store_bit_field_1, with the same arguments, except
   that BITSIZE and BITNUM are constant.  Handle cases specific to
   integral modes.  If OP0_MODE is defined, it is the mode of OP0,
   otherwise OP0 is a BLKmode MEM.  */

static bool
store_integral_bit_field (rtx op0, opt_scalar_int_mode op0_mode,
			  unsigned HOST_WIDE_INT bitsize,
			  unsigned HOST_WIDE_INT bitnum,
			  poly_uint64 bitregion_start,
			  poly_uint64 bitregion_end,
			  machine_mode fieldmode,
			  rtx value, bool reverse, bool fallback_p)
{
  /* Storing an lsb-aligned field in a register
     can be done with a movstrict instruction.  */

  if (!MEM_P (op0)
      && !reverse
      && lowpart_bit_field_p (bitnum, bitsize, op0_mode.require ())
      && known_eq (bitsize, GET_MODE_BITSIZE (fieldmode))
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

      const bool backwards = WORDS_BIG_ENDIAN && fieldmode != BLKmode;
      unsigned int nwords = (bitsize + (BITS_PER_WORD - 1)) / BITS_PER_WORD;
      unsigned int i;
      rtx_insn *last;

      /* This is the mode we must force value to, so that there will be enough
	 subwords to extract.  Note that fieldmode will often (always?) be
	 VOIDmode, because that is what store_field uses to indicate that this
	 is a bit field, but passing VOIDmode to operand_subword_force
	 is not allowed.

	 The mode must be fixed-size, since insertions into variable-sized
	 objects are meant to be handled before calling this function.  */
      fixed_size_mode value_mode = as_a <fixed_size_mode> (GET_MODE (value));
      if (value_mode == VOIDmode)
	value_mode = smallest_int_mode_for_size (nwords * BITS_PER_WORD);

      last = get_last_insn ();
      for (i = 0; i < nwords; i++)
	{
	  /* If I is 0, use the low-order word in both field and target;
	     if I is 1, use the next to lowest word; and so on.  */
	  unsigned int wordnum = (backwards
				  ? GET_MODE_SIZE (value_mode) / UNITS_PER_WORD
				  - i - 1
				  : i);
	  unsigned int bit_offset = (backwards ^ reverse
				     ? MAX ((int) bitsize - ((int) i + 1)
					    * BITS_PER_WORD,
					    0)
				     : (int) i * BITS_PER_WORD);
	  rtx value_word = operand_subword_force (value, wordnum, value_mode);
	  unsigned HOST_WIDE_INT new_bitsize =
	    MIN (BITS_PER_WORD, bitsize - i * BITS_PER_WORD);

	  /* If the remaining chunk doesn't have full wordsize we have
	     to make sure that for big-endian machines the higher order
	     bits are used.  */
	  if (new_bitsize < BITS_PER_WORD && BYTES_BIG_ENDIAN && !backwards)
	    {
	      int shift = BITS_PER_WORD - new_bitsize;
	      rtx shift_rtx = gen_int_shift_amount (word_mode, shift);
	      value_word = simplify_expand_binop (word_mode, lshr_optab,
						  value_word, shift_rtx,
						  NULL_RTX, true,
						  OPTAB_LIB_WIDEN);
	    }

	  if (!store_bit_field_1 (op0, new_bitsize,
				  bitnum + bit_offset,
				  bitregion_start, bitregion_end,
				  word_mode,
				  value_word, reverse, fallback_p))
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
  rtx orig_value = value;
  scalar_int_mode value_mode;
  if (GET_MODE (value) == VOIDmode)
    /* By this point we've dealt with values that are bigger than a word,
       so word_mode is a conservatively correct choice.  */
    value_mode = word_mode;
  else if (!is_a <scalar_int_mode> (GET_MODE (value), &value_mode))
    {
      value_mode = int_mode_for_mode (GET_MODE (value)).require ();
      value = gen_reg_rtx (value_mode);
      emit_move_insn (gen_lowpart (GET_MODE (orig_value), value), orig_value);
    }

  /* If OP0 is a multi-word register, narrow it to the affected word.
     If the region spans two words, defer to store_split_bit_field.
     Don't do this if op0 is a single hard register wider than word
     such as a float or vector register.  */
  if (!MEM_P (op0)
      && GET_MODE_SIZE (op0_mode.require ()) > UNITS_PER_WORD
      && (!REG_P (op0)
	  || !HARD_REGISTER_P (op0)
	  || hard_regno_nregs (REGNO (op0), op0_mode.require ()) != 1))
    {
      if (bitnum % BITS_PER_WORD + bitsize > BITS_PER_WORD)
	{
	  if (!fallback_p)
	    return false;

	  store_split_bit_field (op0, op0_mode, bitsize, bitnum,
				 bitregion_start, bitregion_end,
				 value, value_mode, reverse);
	  return true;
	}
      op0 = simplify_gen_subreg (word_mode, op0, op0_mode.require (),
				 bitnum / BITS_PER_WORD * UNITS_PER_WORD);
      gcc_assert (op0);
      op0_mode = word_mode;
      bitnum %= BITS_PER_WORD;
    }

  /* From here on we can assume that the field to be stored in fits
     within a word.  If the destination is a register, it too fits
     in a word.  */

  extraction_insn insv;
  if (!MEM_P (op0)
      && !reverse
      && get_best_reg_extraction_insn (&insv, EP_insv,
				       GET_MODE_BITSIZE (op0_mode.require ()),
				       fieldmode)
      && store_bit_field_using_insv (&insv, op0, op0_mode,
				     bitsize, bitnum, value, value_mode))
    return true;

  /* If OP0 is a memory, try copying it to a register and seeing if a
     cheap register alternative is available.  */
  if (MEM_P (op0) && !reverse)
    {
      if (get_best_mem_extraction_insn (&insv, EP_insv, bitsize, bitnum,
					fieldmode)
	  && store_bit_field_using_insv (&insv, op0, op0_mode,
					 bitsize, bitnum, value, value_mode))
	return true;

      rtx_insn *last = get_last_insn ();

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
				 fieldmode, orig_value, reverse, false))
	    {
	      emit_move_insn (xop0, tempreg);
	      return true;
	    }
	  delete_insns_since (last);
	}
    }

  if (!fallback_p)
    return false;

  store_fixed_bit_field (op0, op0_mode, bitsize, bitnum, bitregion_start,
			 bitregion_end, value, value_mode, reverse);
  return true;
}

/* Generate code to store value from rtx VALUE
   into a bit-field within structure STR_RTX
   containing BITSIZE bits starting at bit BITNUM.

   BITREGION_START is bitpos of the first bitfield in this region.
   BITREGION_END is the bitpos of the ending bitfield in this region.
   These two fields are 0, if the C++ memory model does not apply,
   or we are not interested in keeping track of bitfield regions.

   FIELDMODE is the machine-mode of the FIELD_DECL node for this field.

   If REVERSE is true, the store is to be done in reverse order.  */

void
store_bit_field (rtx str_rtx, poly_uint64 bitsize, poly_uint64 bitnum,
		 poly_uint64 bitregion_start, poly_uint64 bitregion_end,
		 machine_mode fieldmode,
		 rtx value, bool reverse)
{
  /* Handle -fstrict-volatile-bitfields in the cases where it applies.  */
  unsigned HOST_WIDE_INT ibitsize = 0, ibitnum = 0;
  scalar_int_mode int_mode;
  if (bitsize.is_constant (&ibitsize)
      && bitnum.is_constant (&ibitnum)
      && is_a <scalar_int_mode> (fieldmode, &int_mode)
      && strict_volatile_bitfield_p (str_rtx, ibitsize, ibitnum, int_mode,
				     bitregion_start, bitregion_end))
    {
      /* Storing of a full word can be done with a simple store.
	 We know here that the field can be accessed with one single
	 instruction.  For targets that support unaligned memory,
	 an unaligned access may be necessary.  */
      if (ibitsize == GET_MODE_BITSIZE (int_mode))
	{
	  str_rtx = adjust_bitfield_address (str_rtx, int_mode,
					     ibitnum / BITS_PER_UNIT);
	  if (reverse)
	    value = flip_storage_order (int_mode, value);
	  gcc_assert (ibitnum % BITS_PER_UNIT == 0);
	  emit_move_insn (str_rtx, value);
	}
      else
	{
	  rtx temp;

	  str_rtx = narrow_bit_field_mem (str_rtx, int_mode, ibitsize,
					  ibitnum, &ibitnum);
	  gcc_assert (ibitnum + ibitsize <= GET_MODE_BITSIZE (int_mode));
	  temp = copy_to_reg (str_rtx);
	  if (!store_bit_field_1 (temp, ibitsize, ibitnum, 0, 0,
				  int_mode, value, reverse, true))
	    gcc_unreachable ();

	  emit_move_insn (str_rtx, temp);
	}

      return;
    }

  /* Under the C++0x memory model, we must not touch bits outside the
     bit region.  Adjust the address to start at the beginning of the
     bit region.  */
  if (MEM_P (str_rtx) && maybe_ne (bitregion_start, 0U))
    {
      scalar_int_mode best_mode;
      machine_mode addr_mode = VOIDmode;

      poly_uint64 offset = exact_div (bitregion_start, BITS_PER_UNIT);
      bitnum -= bitregion_start;
      poly_int64 size = bits_to_bytes_round_up (bitnum + bitsize);
      bitregion_end -= bitregion_start;
      bitregion_start = 0;
      if (bitsize.is_constant (&ibitsize)
	  && bitnum.is_constant (&ibitnum)
	  && get_best_mode (ibitsize, ibitnum,
			    bitregion_start, bitregion_end,
			    MEM_ALIGN (str_rtx), INT_MAX,
			    MEM_VOLATILE_P (str_rtx), &best_mode))
	addr_mode = best_mode;
      str_rtx = adjust_bitfield_address_size (str_rtx, addr_mode,
					      offset, size);
    }

  if (!store_bit_field_1 (str_rtx, bitsize, bitnum,
			  bitregion_start, bitregion_end,
			  fieldmode, value, reverse, true))
    gcc_unreachable ();
}

/* Use shifts and boolean operations to store VALUE into a bit field of
   width BITSIZE in OP0, starting at bit BITNUM.  If OP0_MODE is defined,
   it is the mode of OP0, otherwise OP0 is a BLKmode MEM.  VALUE_MODE is
   the mode of VALUE.

   If REVERSE is true, the store is to be done in reverse order.  */

static void
store_fixed_bit_field (rtx op0, opt_scalar_int_mode op0_mode,
		       unsigned HOST_WIDE_INT bitsize,
		       unsigned HOST_WIDE_INT bitnum,
		       poly_uint64 bitregion_start, poly_uint64 bitregion_end,
		       rtx value, scalar_int_mode value_mode, bool reverse)
{
  /* There is a case not handled here:
     a structure with a known alignment of just a halfword
     and a field split across two aligned halfwords within the structure.
     Or likewise a structure with a known alignment of just a byte
     and a field split across two bytes.
     Such cases are not supposed to be able to occur.  */

  scalar_int_mode best_mode;
  if (MEM_P (op0))
    {
      unsigned int max_bitsize = BITS_PER_WORD;
      scalar_int_mode imode;
      if (op0_mode.exists (&imode) && GET_MODE_BITSIZE (imode) < max_bitsize)
	max_bitsize = GET_MODE_BITSIZE (imode);

      if (!get_best_mode (bitsize, bitnum, bitregion_start, bitregion_end,
			  MEM_ALIGN (op0), max_bitsize, MEM_VOLATILE_P (op0),
			  &best_mode))
	{
	  /* The only way this should occur is if the field spans word
	     boundaries.  */
	  store_split_bit_field (op0, op0_mode, bitsize, bitnum,
				 bitregion_start, bitregion_end,
				 value, value_mode, reverse);
	  return;
	}

      op0 = narrow_bit_field_mem (op0, best_mode, bitsize, bitnum, &bitnum);
    }
  else
    best_mode = op0_mode.require ();

  store_fixed_bit_field_1 (op0, best_mode, bitsize, bitnum,
			   value, value_mode, reverse);
}

/* Helper function for store_fixed_bit_field, stores
   the bit field always using MODE, which is the mode of OP0.  The other
   arguments are as for store_fixed_bit_field.  */

static void
store_fixed_bit_field_1 (rtx op0, scalar_int_mode mode,
			 unsigned HOST_WIDE_INT bitsize,
			 unsigned HOST_WIDE_INT bitnum,
			 rtx value, scalar_int_mode value_mode, bool reverse)
{
  rtx temp;
  int all_zero = 0;
  int all_one = 0;

  /* Note that bitsize + bitnum can be greater than GET_MODE_BITSIZE (mode)
     for invalid input, such as f5 from gcc.dg/pr48335-2.c.  */

  if (reverse ? !BYTES_BIG_ENDIAN : BYTES_BIG_ENDIAN)
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
      unsigned HOST_WIDE_INT v = UINTVAL (value);

      if (bitsize < HOST_BITS_PER_WIDE_INT)
	v &= (HOST_WIDE_INT_1U << bitsize) - 1;

      if (v == 0)
	all_zero = 1;
      else if ((bitsize < HOST_BITS_PER_WIDE_INT
		&& v == (HOST_WIDE_INT_1U << bitsize) - 1)
	       || (bitsize == HOST_BITS_PER_WIDE_INT
		   && v == HOST_WIDE_INT_M1U))
	all_one = 1;

      value = lshift_value (mode, v, bitnum);
    }
  else
    {
      int must_and = (GET_MODE_BITSIZE (value_mode) != bitsize
		      && bitnum + bitsize != GET_MODE_BITSIZE (mode));

      if (value_mode != mode)
	value = convert_to_mode (mode, value, 1);

      if (must_and)
	value = expand_binop (mode, and_optab, value,
			      mask_rtx (mode, 0, bitsize, 0),
			      NULL_RTX, 1, OPTAB_LIB_WIDEN);
      if (bitnum > 0)
	value = expand_shift (LSHIFT_EXPR, mode, value,
			      bitnum, NULL_RTX, 1);
    }

  if (reverse)
    value = flip_storage_order (mode, value);

  /* Now clear the chosen bits in OP0,
     except that if VALUE is -1 we need not bother.  */
  /* We keep the intermediates in registers to allow CSE to combine
     consecutive bitfield assignments.  */

  temp = force_reg (mode, op0);

  if (! all_one)
    {
      rtx mask = mask_rtx (mode, bitnum, bitsize, 1);
      if (reverse)
	mask = flip_storage_order (mode, mask);
      temp = expand_binop (mode, and_optab, temp, mask,
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
   VALUE is the value to store, which has mode VALUE_MODE.
   If OP0_MODE is defined, it is the mode of OP0, otherwise OP0 is
   a BLKmode MEM.

   If REVERSE is true, the store is to be done in reverse order.

   This does not yet handle fields wider than BITS_PER_WORD.  */

static void
store_split_bit_field (rtx op0, opt_scalar_int_mode op0_mode,
		       unsigned HOST_WIDE_INT bitsize,
		       unsigned HOST_WIDE_INT bitpos,
		       poly_uint64 bitregion_start, poly_uint64 bitregion_end,
		       rtx value, scalar_int_mode value_mode, bool reverse)
{
  unsigned int unit, total_bits, bitsdone = 0;

  /* Make sure UNIT isn't larger than BITS_PER_WORD, we can only handle that
     much at a time.  */
  if (REG_P (op0) || GET_CODE (op0) == SUBREG)
    unit = BITS_PER_WORD;
  else
    unit = MIN (MEM_ALIGN (op0), BITS_PER_WORD);

  /* If OP0 is a memory with a mode, then UNIT must not be larger than
     OP0's mode as well.  Otherwise, store_fixed_bit_field will call us
     again, and we will mutually recurse forever.  */
  if (MEM_P (op0) && op0_mode.exists ())
    unit = MIN (unit, GET_MODE_BITSIZE (op0_mode.require ()));

  /* If VALUE is a constant other than a CONST_INT, get it into a register in
     WORD_MODE.  If we can do this using gen_lowpart_common, do so.  Note
     that VALUE might be a floating-point constant.  */
  if (CONSTANT_P (value) && !CONST_INT_P (value))
    {
      rtx word = gen_lowpart_common (word_mode, value);

      if (word && (value != word))
	value = word;
      else
	value = gen_lowpart_common (word_mode, force_reg (value_mode, value));
      value_mode = word_mode;
    }

  total_bits = GET_MODE_BITSIZE (value_mode);

  while (bitsdone < bitsize)
    {
      unsigned HOST_WIDE_INT thissize;
      unsigned HOST_WIDE_INT thispos;
      unsigned HOST_WIDE_INT offset;
      rtx part;

      offset = (bitpos + bitsdone) / unit;
      thispos = (bitpos + bitsdone) % unit;

      /* When region of bytes we can touch is restricted, decrease
	 UNIT close to the end of the region as needed.  If op0 is a REG
	 or SUBREG of REG, don't do this, as there can't be data races
	 on a register and we can expand shorter code in some cases.  */
      if (maybe_ne (bitregion_end, 0U)
	  && unit > BITS_PER_UNIT
	  && maybe_gt (bitpos + bitsdone - thispos + unit, bitregion_end + 1)
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

      if (reverse ? !BYTES_BIG_ENDIAN : BYTES_BIG_ENDIAN)
	{
	  /* Fetch successively less significant portions.  */
	  if (CONST_INT_P (value))
	    part = GEN_INT (((unsigned HOST_WIDE_INT) (INTVAL (value))
			     >> (bitsize - bitsdone - thissize))
			    & ((HOST_WIDE_INT_1 << thissize) - 1));
          /* Likewise, but the source is little-endian.  */
          else if (reverse)
	    part = extract_fixed_bit_field (word_mode, value, value_mode,
					    thissize,
					    bitsize - bitsdone - thissize,
					    NULL_RTX, 1, false);
	  else
	    /* The args are chosen so that the last part includes the
	       lsb.  Give extract_bit_field the value it needs (with
	       endianness compensation) to fetch the piece we want.  */
	    part = extract_fixed_bit_field (word_mode, value, value_mode,
					    thissize,
					    total_bits - bitsize + bitsdone,
					    NULL_RTX, 1, false);
	}
      else
	{
	  /* Fetch successively more significant portions.  */
	  if (CONST_INT_P (value))
	    part = GEN_INT (((unsigned HOST_WIDE_INT) (INTVAL (value))
			     >> bitsdone)
			    & ((HOST_WIDE_INT_1 << thissize) - 1));
	  /* Likewise, but the source is big-endian.  */
          else if (reverse)
	    part = extract_fixed_bit_field (word_mode, value, value_mode,
					    thissize,
					    total_bits - bitsdone - thissize,
					    NULL_RTX, 1, false);
	  else
	    part = extract_fixed_bit_field (word_mode, value, value_mode,
					    thissize, bitsdone, NULL_RTX,
					    1, false);
	}

      /* If OP0 is a register, then handle OFFSET here.  */
      rtx op0_piece = op0;
      opt_scalar_int_mode op0_piece_mode = op0_mode;
      if (SUBREG_P (op0) || REG_P (op0))
	{
	  scalar_int_mode imode;
	  if (op0_mode.exists (&imode)
	      && GET_MODE_SIZE (imode) < UNITS_PER_WORD)
	    {
	      if (offset)
		op0_piece = const0_rtx;
	    }
	  else
	    {
	      op0_piece = operand_subword_force (op0,
						 offset * unit / BITS_PER_WORD,
						 GET_MODE (op0));
	      op0_piece_mode = word_mode;
	    }
	  offset &= BITS_PER_WORD / unit - 1;
	}

      /* OFFSET is in UNITs, and UNIT is in bits.  If WORD is const0_rtx,
	 it is just an out-of-bounds access.  Ignore it.  */
      if (op0_piece != const0_rtx)
	store_fixed_bit_field (op0_piece, op0_piece_mode, thissize,
			       offset * unit + thispos, bitregion_start,
			       bitregion_end, part, word_mode, reverse);
      bitsdone += thissize;
    }
}

/* A subroutine of extract_bit_field_1 that converts return value X
   to either MODE or TMODE.  MODE, TMODE and UNSIGNEDP are arguments
   to extract_bit_field.  */

static rtx
convert_extracted_bit_field (rtx x, machine_mode mode,
			     machine_mode tmode, bool unsignedp)
{
  if (GET_MODE (x) == tmode || GET_MODE (x) == mode)
    return x;

  /* If the x mode is not a scalar integral, first convert to the
     integer mode of that size and then access it as a floating-point
     value via a SUBREG.  */
  if (!SCALAR_INT_MODE_P (tmode))
    {
      scalar_int_mode int_mode = int_mode_for_mode (tmode).require ();
      x = convert_to_mode (int_mode, x, unsignedp);
      x = force_reg (int_mode, x);
      return gen_lowpart (tmode, x);
    }

  return convert_to_mode (tmode, x, unsignedp);
}

/* Try to use an ext(z)v pattern to extract a field from OP0.
   Return the extracted value on success, otherwise return null.
   EXTV describes the extraction instruction to use.  If OP0_MODE
   is defined, it is the mode of OP0, otherwise OP0 is a BLKmode MEM.
   The other arguments are as for extract_bit_field.  */

static rtx
extract_bit_field_using_extv (const extraction_insn *extv, rtx op0,
			      opt_scalar_int_mode op0_mode,
			      unsigned HOST_WIDE_INT bitsize,
			      unsigned HOST_WIDE_INT bitnum,
			      int unsignedp, rtx target,
			      machine_mode mode, machine_mode tmode)
{
  struct expand_operand ops[4];
  rtx spec_target = target;
  rtx spec_target_subreg = 0;
  scalar_int_mode ext_mode = extv->field_mode;
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
	bitnum += unit - GET_MODE_BITSIZE (op0_mode.require ());

      /* If op0 is a register, we need it in EXT_MODE to make it
	 acceptable to the format of ext(z)v.  */
      if (GET_CODE (op0) == SUBREG && op0_mode.require () != ext_mode)
	return NULL_RTX;
      if (REG_P (op0) && op0_mode.require () != ext_mode)
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
	  if (partial_subreg_p (GET_MODE (spec_target), ext_mode))
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

/* See whether it would be valid to extract the part of OP0 described
   by BITNUM and BITSIZE into a value of mode MODE using a subreg
   operation.  Return the subreg if so, otherwise return null.  */

static rtx
extract_bit_field_as_subreg (machine_mode mode, rtx op0,
			     poly_uint64 bitsize, poly_uint64 bitnum)
{
  poly_uint64 bytenum;
  if (multiple_p (bitnum, BITS_PER_UNIT, &bytenum)
      && known_eq (bitsize, GET_MODE_BITSIZE (mode))
      && lowpart_bit_field_p (bitnum, bitsize, GET_MODE (op0))
      && TRULY_NOOP_TRUNCATION_MODES_P (mode, GET_MODE (op0)))
    return simplify_gen_subreg (mode, op0, GET_MODE (op0), bytenum);
  return NULL_RTX;
}

/* A subroutine of extract_bit_field, with the same arguments.
   If FALLBACK_P is true, fall back to extract_fixed_bit_field
   if we can find no other means of implementing the operation.
   if FALLBACK_P is false, return NULL instead.  */

static rtx
extract_bit_field_1 (rtx str_rtx, poly_uint64 bitsize, poly_uint64 bitnum,
		     int unsignedp, rtx target, machine_mode mode,
		     machine_mode tmode, bool reverse, bool fallback_p,
		     rtx *alt_rtl)
{
  rtx op0 = str_rtx;
  machine_mode mode1;

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
  if (REG_P (op0) && known_ge (bitnum, GET_MODE_BITSIZE (GET_MODE (op0))))
    return gen_reg_rtx (tmode);

  if (REG_P (op0)
      && mode == GET_MODE (op0)
      && known_eq (bitnum, 0U)
      && known_eq (bitsize, GET_MODE_BITSIZE (GET_MODE (op0))))
    {
      if (reverse)
	op0 = flip_storage_order (mode, op0);
      /* We're trying to extract a full register from itself.  */
      return op0;
    }

  /* First try to check for vector from vector extractions.  */
  if (VECTOR_MODE_P (GET_MODE (op0))
      && !MEM_P (op0)
      && VECTOR_MODE_P (tmode)
      && known_eq (bitsize, GET_MODE_BITSIZE (tmode))
      && maybe_gt (GET_MODE_SIZE (GET_MODE (op0)), GET_MODE_SIZE (tmode)))
    {
      machine_mode new_mode = GET_MODE (op0);
      if (GET_MODE_INNER (new_mode) != GET_MODE_INNER (tmode))
	{
	  scalar_mode inner_mode = GET_MODE_INNER (tmode);
	  poly_uint64 nunits;
	  if (!multiple_p (GET_MODE_BITSIZE (GET_MODE (op0)),
			   GET_MODE_UNIT_BITSIZE (tmode), &nunits)
	      || !mode_for_vector (inner_mode, nunits).exists (&new_mode)
	      || !VECTOR_MODE_P (new_mode)
	      || maybe_ne (GET_MODE_SIZE (new_mode),
			   GET_MODE_SIZE (GET_MODE (op0)))
	      || GET_MODE_INNER (new_mode) != GET_MODE_INNER (tmode)
	      || !targetm.vector_mode_supported_p (new_mode))
	    new_mode = VOIDmode;
	}
      poly_uint64 pos;
      if (new_mode != VOIDmode
	  && (convert_optab_handler (vec_extract_optab, new_mode, tmode)
	      != CODE_FOR_nothing)
	  && multiple_p (bitnum, GET_MODE_BITSIZE (tmode), &pos))
	{
	  struct expand_operand ops[3];
	  machine_mode outermode = new_mode;
	  machine_mode innermode = tmode;
	  enum insn_code icode
	    = convert_optab_handler (vec_extract_optab, outermode, innermode);

	  if (new_mode != GET_MODE (op0))
	    op0 = gen_lowpart (new_mode, op0);
	  create_output_operand (&ops[0], target, innermode);
	  ops[0].target = 1;
	  create_input_operand (&ops[1], op0, outermode);
	  create_integer_operand (&ops[2], pos);
	  if (maybe_expand_insn (icode, 3, ops))
	    {
	      if (alt_rtl && ops[0].target)
		*alt_rtl = target;
	      target = ops[0].value;
	      if (GET_MODE (target) != mode)
		return gen_lowpart (tmode, target);
	      return target;
	    }
	}
    }

  /* See if we can get a better vector mode before extracting.  */
  if (VECTOR_MODE_P (GET_MODE (op0))
      && !MEM_P (op0)
      && GET_MODE_INNER (GET_MODE (op0)) != tmode)
    {
      machine_mode new_mode;

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

      FOR_EACH_MODE_FROM (new_mode, new_mode)
	if (known_eq (GET_MODE_SIZE (new_mode), GET_MODE_SIZE (GET_MODE (op0)))
	    && known_eq (GET_MODE_UNIT_SIZE (new_mode), GET_MODE_SIZE (tmode))
	    && targetm.vector_mode_supported_p (new_mode))
	  break;
      if (new_mode != VOIDmode)
	op0 = gen_lowpart (new_mode, op0);
    }

  /* Use vec_extract patterns for extracting parts of vectors whenever
     available.  If that fails, see whether the current modes and bitregion
     give a natural subreg.  */
  machine_mode outermode = GET_MODE (op0);
  if (VECTOR_MODE_P (outermode) && !MEM_P (op0))
    {
      scalar_mode innermode = GET_MODE_INNER (outermode);
      enum insn_code icode
	= convert_optab_handler (vec_extract_optab, outermode, innermode);
      poly_uint64 pos;
      if (icode != CODE_FOR_nothing
	  && known_eq (bitsize, GET_MODE_BITSIZE (innermode))
	  && multiple_p (bitnum, GET_MODE_BITSIZE (innermode), &pos))
	{
	  struct expand_operand ops[3];

	  create_output_operand (&ops[0], target, innermode);
	  ops[0].target = 1;
	  create_input_operand (&ops[1], op0, outermode);
	  create_integer_operand (&ops[2], pos);
	  if (maybe_expand_insn (icode, 3, ops))
	    {
	      if (alt_rtl && ops[0].target)
		*alt_rtl = target;
	      target = ops[0].value;
	      if (GET_MODE (target) != mode)
		return gen_lowpart (tmode, target);
	      return target;
	    }
	}
      /* Using subregs is useful if we're extracting one register vector
	 from a multi-register vector.  extract_bit_field_as_subreg checks
	 for valid bitsize and bitnum, so we don't need to do that here.  */
      if (VECTOR_MODE_P (mode))
	{
	  rtx sub = extract_bit_field_as_subreg (mode, op0, bitsize, bitnum);
	  if (sub)
	    return sub;
	}
    }

  /* Make sure we are playing with integral modes.  Pun with subregs
     if we aren't.  */
  opt_scalar_int_mode op0_mode = int_mode_for_mode (GET_MODE (op0));
  scalar_int_mode imode;
  if (!op0_mode.exists (&imode) || imode != GET_MODE (op0))
    {
      if (MEM_P (op0))
	op0 = adjust_bitfield_address_size (op0, op0_mode.else_blk (),
					    0, MEM_SIZE (op0));
      else if (op0_mode.exists (&imode))
	{
	  op0 = gen_lowpart (imode, op0);

	  /* If we got a SUBREG, force it into a register since we
	     aren't going to be able to do another SUBREG on it.  */
	  if (GET_CODE (op0) == SUBREG)
	    op0 = force_reg (imode, op0);
	}
      else
	{
	  poly_int64 size = GET_MODE_SIZE (GET_MODE (op0));
	  rtx mem = assign_stack_temp (GET_MODE (op0), size);
	  emit_move_insn (mem, op0);
	  op0 = adjust_bitfield_address_size (mem, BLKmode, 0, size);
	}
    }

  /* ??? We currently assume TARGET is at least as big as BITSIZE.
     If that's wrong, the solution is to test for it and set TARGET to 0
     if needed.  */

  /* Get the mode of the field to use for atomic access or subreg
     conversion.  */
  if (!SCALAR_INT_MODE_P (tmode)
      || !mode_for_size (bitsize, GET_MODE_CLASS (tmode), 0).exists (&mode1))
    mode1 = mode;
  gcc_assert (mode1 != BLKmode);

  /* Extraction of a full MODE1 value can be done with a subreg as long
     as the least significant bit of the value is the least significant
     bit of either OP0 or a word of OP0.  */
  if (!MEM_P (op0) && !reverse)
    {
      rtx sub = extract_bit_field_as_subreg (mode1, op0, bitsize, bitnum);
      if (sub)
	return convert_extracted_bit_field (sub, mode, tmode, unsignedp);
    }

  /* Extraction of a full MODE1 value can be done with a load as long as
     the field is on a byte boundary and is sufficiently aligned.  */
  poly_uint64 bytenum;
  if (simple_mem_bitfield_p (op0, bitsize, bitnum, mode1, &bytenum))
    {
      op0 = adjust_bitfield_address (op0, mode1, bytenum);
      if (reverse)
	op0 = flip_storage_order (mode1, op0);
      return convert_extracted_bit_field (op0, mode, tmode, unsignedp);
    }

  /* If we have a memory source and a non-constant bit offset, restrict
     the memory to the referenced bytes.  This is a worst-case fallback
     but is useful for things like vector booleans.  */
  if (MEM_P (op0) && !bitnum.is_constant ())
    {
      bytenum = bits_to_bytes_round_down (bitnum);
      bitnum = num_trailing_bits (bitnum);
      poly_uint64 bytesize = bits_to_bytes_round_up (bitnum + bitsize);
      op0 = adjust_bitfield_address_size (op0, BLKmode, bytenum, bytesize);
      op0_mode = opt_scalar_int_mode ();
    }

  /* It's possible we'll need to handle other cases here for
     polynomial bitnum and bitsize.  */

  /* From here on we need to be looking at a fixed-size insertion.  */
  return extract_integral_bit_field (op0, op0_mode, bitsize.to_constant (),
				     bitnum.to_constant (), unsignedp,
				     target, mode, tmode, reverse, fallback_p);
}

/* Subroutine of extract_bit_field_1, with the same arguments, except
   that BITSIZE and BITNUM are constant.  Handle cases specific to
   integral modes.  If OP0_MODE is defined, it is the mode of OP0,
   otherwise OP0 is a BLKmode MEM.  */

static rtx
extract_integral_bit_field (rtx op0, opt_scalar_int_mode op0_mode,
			    unsigned HOST_WIDE_INT bitsize,
			    unsigned HOST_WIDE_INT bitnum, int unsignedp,
			    rtx target, machine_mode mode, machine_mode tmode,
			    bool reverse, bool fallback_p)
{
  /* Handle fields bigger than a word.  */

  if (bitsize > BITS_PER_WORD)
    {
      /* Here we transfer the words of the field
	 in the order least significant first.
	 This is because the most significant word is the one which may
	 be less than full.  */

      const bool backwards = WORDS_BIG_ENDIAN;
      unsigned int nwords = (bitsize + (BITS_PER_WORD - 1)) / BITS_PER_WORD;
      unsigned int i;
      rtx_insn *last;

      if (target == 0 || !REG_P (target) || !valid_multiword_target_p (target))
	target = gen_reg_rtx (mode);

      /* In case we're about to clobber a base register or something 
	 (see gcc.c-torture/execute/20040625-1.c).   */
      if (reg_mentioned_p (target, op0))
	target = gen_reg_rtx (mode);

      /* Indicate for flow that the entire target reg is being set.  */
      emit_clobber (target);

      /* The mode must be fixed-size, since extract_bit_field_1 handles
	 extractions from variable-sized objects before calling this
	 function.  */
      unsigned int target_size
	= GET_MODE_SIZE (GET_MODE (target)).to_constant ();
      last = get_last_insn ();
      for (i = 0; i < nwords; i++)
	{
	  /* If I is 0, use the low-order word in both field and target;
	     if I is 1, use the next to lowest word; and so on.  */
	  /* Word number in TARGET to use.  */
	  unsigned int wordnum
	    = (backwards ? target_size / UNITS_PER_WORD - i - 1 : i);
	  /* Offset from start of field in OP0.  */
	  unsigned int bit_offset = (backwards ^ reverse
				     ? MAX ((int) bitsize - ((int) i + 1)
					    * BITS_PER_WORD,
					    0)
				     : (int) i * BITS_PER_WORD);
	  rtx target_part = operand_subword (target, wordnum, 1, VOIDmode);
	  rtx result_part
	    = extract_bit_field_1 (op0, MIN (BITS_PER_WORD,
					     bitsize - i * BITS_PER_WORD),
				   bitnum + bit_offset, 1, target_part,
				   mode, word_mode, reverse, fallback_p, NULL);

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
	  if (target_size > nwords * UNITS_PER_WORD)
	    {
	      unsigned int i, total_words;

	      total_words = target_size / UNITS_PER_WORD;
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
  if (!MEM_P (op0) && GET_MODE_SIZE (op0_mode.require ()) > UNITS_PER_WORD)
    {
      if (bitnum % BITS_PER_WORD + bitsize > BITS_PER_WORD)
	{
	  if (!fallback_p)
	    return NULL_RTX;
	  target = extract_split_bit_field (op0, op0_mode, bitsize, bitnum,
					    unsignedp, reverse);
	  return convert_extracted_bit_field (target, mode, tmode, unsignedp);
	}
      op0 = simplify_gen_subreg (word_mode, op0, op0_mode.require (),
				 bitnum / BITS_PER_WORD * UNITS_PER_WORD);
      op0_mode = word_mode;
      bitnum %= BITS_PER_WORD;
    }

  /* From here on we know the desired field is smaller than a word.
     If OP0 is a register, it too fits within a word.  */
  enum extraction_pattern pattern = unsignedp ? EP_extzv : EP_extv;
  extraction_insn extv;
  if (!MEM_P (op0)
      && !reverse
      /* ??? We could limit the structure size to the part of OP0 that
	 contains the field, with appropriate checks for endianness
	 and TARGET_TRULY_NOOP_TRUNCATION.  */
      && get_best_reg_extraction_insn (&extv, pattern,
				       GET_MODE_BITSIZE (op0_mode.require ()),
				       tmode))
    {
      rtx result = extract_bit_field_using_extv (&extv, op0, op0_mode,
						 bitsize, bitnum,
						 unsignedp, target, mode,
						 tmode);
      if (result)
	return result;
    }

  /* If OP0 is a memory, try copying it to a register and seeing if a
     cheap register alternative is available.  */
  if (MEM_P (op0) & !reverse)
    {
      if (get_best_mem_extraction_insn (&extv, pattern, bitsize, bitnum,
					tmode))
	{
	  rtx result = extract_bit_field_using_extv (&extv, op0, op0_mode,
						     bitsize, bitnum,
						     unsignedp, target, mode,
						     tmode);
	  if (result)
	    return result;
	}

      rtx_insn *last = get_last_insn ();

      /* Try loading part of OP0 into a register and extracting the
	 bitfield from that.  */
      unsigned HOST_WIDE_INT bitpos;
      rtx xop0 = adjust_bit_field_mem_for_reg (pattern, op0, bitsize, bitnum,
					       0, 0, tmode, &bitpos);
      if (xop0)
	{
	  xop0 = copy_to_reg (xop0);
	  rtx result = extract_bit_field_1 (xop0, bitsize, bitpos,
					    unsignedp, target,
					    mode, tmode, reverse, false, NULL);
	  if (result)
	    return result;
	  delete_insns_since (last);
	}
    }

  if (!fallback_p)
    return NULL;

  /* Find a correspondingly-sized integer field, so we can apply
     shifts and masks to it.  */
  scalar_int_mode int_mode;
  if (!int_mode_for_mode (tmode).exists (&int_mode))
    /* If this fails, we should probably push op0 out to memory and then
       do a load.  */
    int_mode = int_mode_for_mode (mode).require ();

  target = extract_fixed_bit_field (int_mode, op0, op0_mode, bitsize,
				    bitnum, target, unsignedp, reverse);

  /* Complex values must be reversed piecewise, so we need to undo the global
     reversal, convert to the complex mode and reverse again.  */
  if (reverse && COMPLEX_MODE_P (tmode))
    {
      target = flip_storage_order (int_mode, target);
      target = convert_extracted_bit_field (target, mode, tmode, unsignedp);
      target = flip_storage_order (tmode, target);
    }
  else
    target = convert_extracted_bit_field (target, mode, tmode, unsignedp);

  return target;
}

/* Generate code to extract a byte-field from STR_RTX
   containing BITSIZE bits, starting at BITNUM,
   and put it in TARGET if possible (if TARGET is nonzero).
   Regardless of TARGET, we return the rtx for where the value is placed.

   STR_RTX is the structure containing the byte (a REG or MEM).
   UNSIGNEDP is nonzero if this is an unsigned bit field.
   MODE is the natural mode of the field value once extracted.
   TMODE is the mode the caller would like the value to have;
   but the value may be returned with type MODE instead.

   If REVERSE is true, the extraction is to be done in reverse order.

   If a TARGET is specified and we can store in it at no extra cost,
   we do so, and return TARGET.
   Otherwise, we return a REG of mode TMODE or MODE, with TMODE preferred
   if they are equally easy.  */

rtx
extract_bit_field (rtx str_rtx, poly_uint64 bitsize, poly_uint64 bitnum,
		   int unsignedp, rtx target, machine_mode mode,
		   machine_mode tmode, bool reverse, rtx *alt_rtl)
{
  machine_mode mode1;

  /* Handle -fstrict-volatile-bitfields in the cases where it applies.  */
  if (maybe_ne (GET_MODE_BITSIZE (GET_MODE (str_rtx)), 0))
    mode1 = GET_MODE (str_rtx);
  else if (target && maybe_ne (GET_MODE_BITSIZE (GET_MODE (target)), 0))
    mode1 = GET_MODE (target);
  else
    mode1 = tmode;

  unsigned HOST_WIDE_INT ibitsize, ibitnum;
  scalar_int_mode int_mode;
  if (bitsize.is_constant (&ibitsize)
      && bitnum.is_constant (&ibitnum)
      && is_a <scalar_int_mode> (mode1, &int_mode)
      && strict_volatile_bitfield_p (str_rtx, ibitsize, ibitnum,
				     int_mode, 0, 0))
    {
      /* Extraction of a full INT_MODE value can be done with a simple load.
	 We know here that the field can be accessed with one single
	 instruction.  For targets that support unaligned memory,
	 an unaligned access may be necessary.  */
      if (ibitsize == GET_MODE_BITSIZE (int_mode))
	{
	  rtx result = adjust_bitfield_address (str_rtx, int_mode,
						ibitnum / BITS_PER_UNIT);
	  if (reverse)
	    result = flip_storage_order (int_mode, result);
	  gcc_assert (ibitnum % BITS_PER_UNIT == 0);
	  return convert_extracted_bit_field (result, mode, tmode, unsignedp);
	}

      str_rtx = narrow_bit_field_mem (str_rtx, int_mode, ibitsize, ibitnum,
				      &ibitnum);
      gcc_assert (ibitnum + ibitsize <= GET_MODE_BITSIZE (int_mode));
      str_rtx = copy_to_reg (str_rtx);
      return extract_bit_field_1 (str_rtx, ibitsize, ibitnum, unsignedp,
				  target, mode, tmode, reverse, true, alt_rtl);
    }

  return extract_bit_field_1 (str_rtx, bitsize, bitnum, unsignedp,
			      target, mode, tmode, reverse, true, alt_rtl);
}

/* Use shifts and boolean operations to extract a field of BITSIZE bits
   from bit BITNUM of OP0.  If OP0_MODE is defined, it is the mode of OP0,
   otherwise OP0 is a BLKmode MEM.

   UNSIGNEDP is nonzero for an unsigned bit field (don't sign-extend value).
   If REVERSE is true, the extraction is to be done in reverse order.

   If TARGET is nonzero, attempts to store the value there
   and return TARGET, but this is not guaranteed.
   If TARGET is not used, create a pseudo-reg of mode TMODE for the value.  */

static rtx
extract_fixed_bit_field (machine_mode tmode, rtx op0,
			 opt_scalar_int_mode op0_mode,
			 unsigned HOST_WIDE_INT bitsize,
			 unsigned HOST_WIDE_INT bitnum, rtx target,
			 int unsignedp, bool reverse)
{
  scalar_int_mode mode;
  if (MEM_P (op0))
    {
      if (!get_best_mode (bitsize, bitnum, 0, 0, MEM_ALIGN (op0),
			  BITS_PER_WORD, MEM_VOLATILE_P (op0), &mode))
	/* The only way this should occur is if the field spans word
	   boundaries.  */
	return extract_split_bit_field (op0, op0_mode, bitsize, bitnum,
					unsignedp, reverse);

      op0 = narrow_bit_field_mem (op0, mode, bitsize, bitnum, &bitnum);
    }
  else
    mode = op0_mode.require ();

  return extract_fixed_bit_field_1 (tmode, op0, mode, bitsize, bitnum,
				    target, unsignedp, reverse);
}

/* Helper function for extract_fixed_bit_field, extracts
   the bit field always using MODE, which is the mode of OP0.
   The other arguments are as for extract_fixed_bit_field.  */

static rtx
extract_fixed_bit_field_1 (machine_mode tmode, rtx op0, scalar_int_mode mode,
			   unsigned HOST_WIDE_INT bitsize,
			   unsigned HOST_WIDE_INT bitnum, rtx target,
			   int unsignedp, bool reverse)
{
  /* Note that bitsize + bitnum can be greater than GET_MODE_BITSIZE (mode)
     for invalid input, such as extract equivalent of f5 from
     gcc.dg/pr48335-2.c.  */

  if (reverse ? !BYTES_BIG_ENDIAN : BYTES_BIG_ENDIAN)
    /* BITNUM is the distance between our msb and that of OP0.
       Convert it to the distance from the lsb.  */
    bitnum = GET_MODE_BITSIZE (mode) - bitsize - bitnum;

  /* Now BITNUM is always the distance between the field's lsb and that of OP0.
     We have reduced the big-endian case to the little-endian case.  */
  if (reverse)
    op0 = flip_storage_order (mode, op0);

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
      /* Convert the value to the desired mode.  TMODE must also be a
	 scalar integer for this conversion to make sense, since we
	 shouldn't reinterpret the bits.  */
      scalar_int_mode new_mode = as_a <scalar_int_mode> (tmode);
      if (mode != new_mode)
	op0 = convert_to_mode (new_mode, op0, 1);

      /* Unless the msb of the field used to be the msb when we shifted,
	 mask out the upper bits.  */

      if (GET_MODE_BITSIZE (mode) != bitnum + bitsize)
	return expand_binop (new_mode, and_optab, op0,
			     mask_rtx (new_mode, 0, bitsize, 0),
			     target, 1, OPTAB_LIB_WIDEN);
      return op0;
    }

  /* To extract a signed bit-field, first shift its msb to the msb of the word,
     then arithmetic-shift its lsb to the lsb of the word.  */
  op0 = force_reg (mode, op0);

  /* Find the narrowest integer mode that contains the field.  */

  opt_scalar_int_mode mode_iter;
  FOR_EACH_MODE_IN_CLASS (mode_iter, MODE_INT)
    if (GET_MODE_BITSIZE (mode_iter.require ()) >= bitsize + bitnum)
      break;

  mode = mode_iter.require ();
  op0 = convert_to_mode (mode, op0, 0);

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

/* Return a constant integer (CONST_INT or CONST_DOUBLE) rtx with the value
   VALUE << BITPOS.  */

static rtx
lshift_value (machine_mode mode, unsigned HOST_WIDE_INT value,
	      int bitpos)
{
  return immed_wide_int_const (wi::lshift (value, bitpos), mode);
}

/* Extract a bit field that is split across two words
   and return an RTX for the result.

   OP0 is the REG, SUBREG or MEM rtx for the first of the two words.
   BITSIZE is the field width; BITPOS, position of its first bit, in the word.
   UNSIGNEDP is 1 if should zero-extend the contents; else sign-extend.
   If OP0_MODE is defined, it is the mode of OP0, otherwise OP0 is
   a BLKmode MEM.

   If REVERSE is true, the extraction is to be done in reverse order.  */

static rtx
extract_split_bit_field (rtx op0, opt_scalar_int_mode op0_mode,
			 unsigned HOST_WIDE_INT bitsize,
			 unsigned HOST_WIDE_INT bitpos, int unsignedp,
			 bool reverse)
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
      rtx part;
      unsigned HOST_WIDE_INT thispos;
      unsigned HOST_WIDE_INT offset;

      offset = (bitpos + bitsdone) / unit;
      thispos = (bitpos + bitsdone) % unit;

      /* THISSIZE must not overrun a word boundary.  Otherwise,
	 extract_fixed_bit_field will call us again, and we will mutually
	 recurse forever.  */
      thissize = MIN (bitsize - bitsdone, BITS_PER_WORD);
      thissize = MIN (thissize, unit - thispos);

      /* If OP0 is a register, then handle OFFSET here.  */
      rtx op0_piece = op0;
      opt_scalar_int_mode op0_piece_mode = op0_mode;
      if (SUBREG_P (op0) || REG_P (op0))
	{
	  op0_piece = operand_subword_force (op0, offset, op0_mode.require ());
	  op0_piece_mode = word_mode;
	  offset = 0;
	}

      /* Extract the parts in bit-counting order,
	 whose meaning is determined by BYTES_PER_UNIT.
	 OFFSET is in UNITs, and UNIT is in bits.  */
      part = extract_fixed_bit_field (word_mode, op0_piece, op0_piece_mode,
				      thissize, offset * unit + thispos,
				      0, 1, reverse);
      bitsdone += thissize;

      /* Shift this part into place for the result.  */
      if (reverse ? !BYTES_BIG_ENDIAN : BYTES_BIG_ENDIAN)
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
       a truncation (and is thus subject to TARGET_TRULY_NOOP_TRUNCATION).

   In other words, this routine performs a computation, whereas the
   gen_lowpart* routines are conceptually lvalue or rvalue subreg
   operations.  */

rtx
extract_low_bits (machine_mode mode, machine_mode src_mode, rtx src)
{
  scalar_int_mode int_mode, src_int_mode;

  if (mode == src_mode)
    return src;

  if (CONSTANT_P (src))
    {
      /* simplify_gen_subreg can't be used here, as if simplify_subreg
	 fails, it will happily create (subreg (symbol_ref)) or similar
	 invalid SUBREGs.  */
      poly_uint64 byte = subreg_lowpart_offset (mode, src_mode);
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

  if (known_eq (GET_MODE_BITSIZE (mode), GET_MODE_BITSIZE (src_mode))
      && targetm.modes_tieable_p (mode, src_mode))
    {
      rtx x = gen_lowpart_common (mode, src);
      if (x)
        return x;
    }

  if (!int_mode_for_mode (src_mode).exists (&src_int_mode)
      || !int_mode_for_mode (mode).exists (&int_mode))
    return NULL_RTX;

  if (!targetm.modes_tieable_p (src_int_mode, src_mode))
    return NULL_RTX;
  if (!targetm.modes_tieable_p (int_mode, mode))
    return NULL_RTX;

  src = gen_lowpart (src_int_mode, src);
  if (!validate_subreg (int_mode, src_int_mode, src,
			subreg_lowpart_offset (int_mode, src_int_mode)))
    return NULL_RTX;

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
   Return the rtx for where the value is.
   If that cannot be done, abort the compilation unless MAY_FAIL is true,
   in which case 0 is returned.  */

static rtx
expand_shift_1 (enum tree_code code, machine_mode mode, rtx shifted,
		rtx amount, rtx target, int unsignedp, bool may_fail = false)
{
  rtx op1, temp = 0;
  int left = (code == LSHIFT_EXPR || code == LROTATE_EXPR);
  int rotate = (code == LROTATE_EXPR || code == RROTATE_EXPR);
  optab lshift_optab = ashl_optab;
  optab rshift_arith_optab = ashr_optab;
  optab rshift_uns_optab = lshr_optab;
  optab lrotate_optab = rotl_optab;
  optab rrotate_optab = rotr_optab;
  machine_mode op1_mode;
  scalar_mode scalar_mode = GET_MODE_INNER (mode);
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
	      (unsigned HOST_WIDE_INT) GET_MODE_BITSIZE (scalar_mode)))
	op1 = gen_int_shift_amount (mode,
				    (unsigned HOST_WIDE_INT) INTVAL (op1)
				    % GET_MODE_BITSIZE (scalar_mode));
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
      && IN_RANGE (INTVAL (op1), GET_MODE_BITSIZE (scalar_mode) / 2 + left,
		   GET_MODE_BITSIZE (scalar_mode) - 1))
    {
      op1 = gen_int_shift_amount (mode, (GET_MODE_BITSIZE (scalar_mode)
					 - INTVAL (op1)));
      left = !left;
      code = left ? LROTATE_EXPR : RROTATE_EXPR;
    }

  /* Rotation of 16bit values by 8 bits is effectively equivalent to a bswaphi.
     Note that this is not the case for bigger values.  For instance a rotation
     of 0x01020304 by 16 bits gives 0x03040102 which is different from
     0x04030201 (bswapsi).  */
  if (rotate
      && CONST_INT_P (op1)
      && INTVAL (op1) == BITS_PER_UNIT
      && GET_MODE_SIZE (scalar_mode) == 2
      && optab_handler (bswap_optab, mode) != CODE_FOR_nothing)
    return expand_unop (mode, bswap_optab, shifted, NULL_RTX, unsignedp);

  if (op1 == const0_rtx)
    return shifted;

  /* Check whether its cheaper to implement a left shift by a constant
     bit count by a sequence of additions.  */
  if (code == LSHIFT_EXPR
      && CONST_INT_P (op1)
      && INTVAL (op1) > 0
      && INTVAL (op1) < GET_MODE_PRECISION (scalar_mode)
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
		other_amount = gen_int_shift_amount
		  (mode, GET_MODE_BITSIZE (scalar_mode) - INTVAL (op1));
	      else
		{
		  other_amount
		    = simplify_gen_unary (NEG, GET_MODE (op1),
					  op1, GET_MODE (op1));
		  HOST_WIDE_INT mask = GET_MODE_PRECISION (scalar_mode) - 1;
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

  gcc_assert (temp != NULL_RTX || may_fail);
  return temp;
}

/* Output a shift instruction for expression code CODE,
   with SHIFTED being the rtx for the value to shift,
   and AMOUNT the amount to shift by.
   Store the result in the rtx TARGET, if that is convenient.
   If UNSIGNEDP is nonzero, do a logical shift; otherwise, arithmetic.
   Return the rtx for where the value is.  */

rtx
expand_shift (enum tree_code code, machine_mode mode, rtx shifted,
	      poly_int64 amount, rtx target, int unsignedp)
{
  return expand_shift_1 (code, mode, shifted,
			 gen_int_shift_amount (mode, amount),
			 target, unsignedp);
}

/* Likewise, but return 0 if that cannot be done.  */

static rtx
maybe_expand_shift (enum tree_code code, machine_mode mode, rtx shifted,
		    int amount, rtx target, int unsignedp)
{
  return expand_shift_1 (code, mode,
			 shifted, GEN_INT (amount), target, unsignedp, true);
}

/* Output a shift instruction for expression code CODE,
   with SHIFTED being the rtx for the value to shift,
   and AMOUNT the tree for the amount to shift by.
   Store the result in the rtx TARGET, if that is convenient.
   If UNSIGNEDP is nonzero, do a logical shift; otherwise, arithmetic.
   Return the rtx for where the value is.  */

rtx
expand_variable_shift (enum tree_code code, machine_mode mode, rtx shifted,
		       tree amount, rtx target, int unsignedp)
{
  return expand_shift_1 (code, mode,
			 shifted, expand_normal (amount), target, unsignedp);
}


static void synth_mult (struct algorithm *, unsigned HOST_WIDE_INT,
			const struct mult_cost *, machine_mode mode);
static rtx expand_mult_const (machine_mode, rtx, HOST_WIDE_INT, rtx,
			      const struct algorithm *, enum mult_variant);
static unsigned HOST_WIDE_INT invert_mod2n (unsigned HOST_WIDE_INT, int);
static rtx extract_high_half (scalar_int_mode, rtx);
static rtx expmed_mult_highpart (scalar_int_mode, rtx, rtx, rtx, int, int);
static rtx expmed_mult_highpart_optab (scalar_int_mode, rtx, rtx, rtx,
				       int, int);
/* Compute and return the best algorithm for multiplying by T.
   The algorithm must cost less than cost_limit
   If retval.cost >= COST_LIMIT, no algorithm was found and all
   other field of the returned struct are undefined.
   MODE is the machine mode of the multiplication.  */

static void
synth_mult (struct algorithm *alg_out, unsigned HOST_WIDE_INT t,
	    const struct mult_cost *cost_limit, machine_mode mode)
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
  scalar_int_mode imode;
  struct alg_hash_entry *entry_ptr;

  /* Indicate that no algorithm is yet found.  If no algorithm
     is found, this value will be returned and indicate failure.  */
  alg_out->cost.cost = cost_limit->cost + 1;
  alg_out->cost.latency = cost_limit->latency + 1;

  if (cost_limit->cost < 0
      || (cost_limit->cost == 0 && cost_limit->latency <= 0))
    return;

  /* Be prepared for vector modes.  */
  imode = as_a <scalar_int_mode> (GET_MODE_INNER (mode));

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
      m = ctz_or_zero (t); /* m = number of low zero bits */
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
	      best_cost = alg_in->cost;
	      std::swap (alg_in, best_alg);
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
		  best_cost = alg_in->cost;
		  std::swap (alg_in, best_alg);
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
	  /* T ends with ...111.  Multiply by (T + 1) and subtract T.  */

	  op_cost = add_cost (speed, mode);
	  new_limit.cost = best_cost.cost - op_cost;
	  new_limit.latency = best_cost.latency - op_cost;
	  synth_mult (alg_in, t + 1, &new_limit, mode);

	  alg_in->cost.cost += op_cost;
	  alg_in->cost.latency += op_cost;
	  if (CHEAPER_MULT_COST (&alg_in->cost, &best_cost))
	    {
	      best_cost = alg_in->cost;
	      std::swap (alg_in, best_alg);
	      best_alg->log[best_alg->ops] = 0;
	      best_alg->op[best_alg->ops] = alg_sub_t_m2;
	    }
	}
      else
	{
	  /* T ends with ...01 or ...011.  Multiply by (T - 1) and add T.  */

	  op_cost = add_cost (speed, mode);
	  new_limit.cost = best_cost.cost - op_cost;
	  new_limit.latency = best_cost.latency - op_cost;
	  synth_mult (alg_in, t - 1, &new_limit, mode);

	  alg_in->cost.cost += op_cost;
	  alg_in->cost.latency += op_cost;
	  if (CHEAPER_MULT_COST (&alg_in->cost, &best_cost))
	    {
	      best_cost = alg_in->cost;
	      std::swap (alg_in, best_alg);
	      best_alg->log[best_alg->ops] = 0;
	      best_alg->op[best_alg->ops] = alg_add_t_m2;
	    }
	}

      /* We may be able to calculate a * -7, a * -15, a * -31, etc
	 quickly with a - a * n for some appropriate constant n.  */
      m = exact_log2 (-orig_t + 1);
      if (m >= 0 && m < maxm)
	{
	  op_cost = add_cost (speed, mode) + shift_cost (speed, mode, m);
	  /* If the target has a cheap shift-and-subtract insn use
	     that in preference to a shift insn followed by a sub insn.
	     Assume that the shift-and-sub is "atomic" with a latency
	     equal to it's cost, otherwise assume that on superscalar
	     hardware the shift may be executed concurrently with the
	     earlier steps in the algorithm.  */
	  if (shiftsub1_cost (speed, mode, m) <= op_cost)
	    {
	      op_cost = shiftsub1_cost (speed, mode, m);
	      op_latency = op_cost;
	    }
	  else
	    op_latency = add_cost (speed, mode);

	  new_limit.cost = best_cost.cost - op_cost;
	  new_limit.latency = best_cost.latency - op_latency;
	  synth_mult (alg_in, (unsigned HOST_WIDE_INT) (-orig_t + 1) >> m,
		      &new_limit, mode);

	  alg_in->cost.cost += op_cost;
	  alg_in->cost.latency += op_latency;
	  if (CHEAPER_MULT_COST (&alg_in->cost, &best_cost))
	    {
	      best_cost = alg_in->cost;
	      std::swap (alg_in, best_alg);
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

      d = (HOST_WIDE_INT_1U << m) + 1;
      if (t % d == 0 && t > d && m < maxm
	  && (!cache_hit || cache_alg == alg_add_factor))
	{
	  op_cost = add_cost (speed, mode) + shift_cost (speed, mode, m);
	  if (shiftadd_cost (speed, mode, m) <= op_cost)
	    op_cost = shiftadd_cost (speed, mode, m);

	  op_latency = op_cost;


	  new_limit.cost = best_cost.cost - op_cost;
	  new_limit.latency = best_cost.latency - op_latency;
	  synth_mult (alg_in, t / d, &new_limit, mode);

	  alg_in->cost.cost += op_cost;
	  alg_in->cost.latency += op_latency;
	  if (alg_in->cost.latency < op_cost)
	    alg_in->cost.latency = op_cost;
	  if (CHEAPER_MULT_COST (&alg_in->cost, &best_cost))
	    {
	      best_cost = alg_in->cost;
	      std::swap (alg_in, best_alg);
	      best_alg->log[best_alg->ops] = m;
	      best_alg->op[best_alg->ops] = alg_add_factor;
	    }
	  /* Other factors will have been taken care of in the recursion.  */
	  break;
	}

      d = (HOST_WIDE_INT_1U << m) - 1;
      if (t % d == 0 && t > d && m < maxm
	  && (!cache_hit || cache_alg == alg_sub_factor))
	{
	  op_cost = add_cost (speed, mode) + shift_cost (speed, mode, m);
	  if (shiftsub0_cost (speed, mode, m) <= op_cost)
	    op_cost = shiftsub0_cost (speed, mode, m);

	  op_latency = op_cost;

	  new_limit.cost = best_cost.cost - op_cost;
	  new_limit.latency = best_cost.latency - op_latency;
	  synth_mult (alg_in, t / d, &new_limit, mode);

	  alg_in->cost.cost += op_cost;
	  alg_in->cost.latency += op_latency;
	  if (alg_in->cost.latency < op_cost)
	    alg_in->cost.latency = op_cost;
	  if (CHEAPER_MULT_COST (&alg_in->cost, &best_cost))
	    {
	      best_cost = alg_in->cost;
	      std::swap (alg_in, best_alg);
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
      m = ctz_hwi (q);
      if (q && m < maxm)
	{
	  op_cost = shiftadd_cost (speed, mode, m);
	  new_limit.cost = best_cost.cost - op_cost;
	  new_limit.latency = best_cost.latency - op_cost;
	  synth_mult (alg_in, (t - 1) >> m, &new_limit, mode);

	  alg_in->cost.cost += op_cost;
	  alg_in->cost.latency += op_cost;
	  if (CHEAPER_MULT_COST (&alg_in->cost, &best_cost))
	    {
	      best_cost = alg_in->cost;
	      std::swap (alg_in, best_alg);
	      best_alg->log[best_alg->ops] = m;
	      best_alg->op[best_alg->ops] = alg_add_t2_m;
	    }
	}
      if (cache_hit)
	goto done;

    do_alg_sub_t2_m:
      q = t + 1;
      m = ctz_hwi (q);
      if (q && m < maxm)
	{
	  op_cost = shiftsub0_cost (speed, mode, m);
	  new_limit.cost = best_cost.cost - op_cost;
	  new_limit.latency = best_cost.latency - op_cost;
	  synth_mult (alg_in, (t + 1) >> m, &new_limit, mode);

	  alg_in->cost.cost += op_cost;
	  alg_in->cost.latency += op_cost;
	  if (CHEAPER_MULT_COST (&alg_in->cost, &best_cost))
	    {
	      best_cost = alg_in->cost;
	      std::swap (alg_in, best_alg);
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

bool
choose_mult_variant (machine_mode mode, HOST_WIDE_INT val,
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
      op_cost = neg_cost (speed, mode);
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
expand_mult_const (machine_mode mode, rtx op0, HOST_WIDE_INT val,
		   rtx target, const struct algorithm *alg,
		   enum mult_variant variant)
{
  unsigned HOST_WIDE_INT val_so_far;
  rtx_insn *insn;
  rtx accum, tem;
  int opno;
  machine_mode nmode;

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
	  val_so_far += HOST_WIDE_INT_1U << log;
	  break;

	case alg_sub_t_m2:
	  tem = expand_shift (LSHIFT_EXPR, mode, op0, log, NULL_RTX, 0);
	  accum = force_operand (gen_rtx_MINUS (mode, accum, tem),
				 add_target ? add_target : accum_target);
	  val_so_far -= HOST_WIDE_INT_1U << log;
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
	  wide_int wval_so_far
	    = wi::uhwi (val_so_far,
			GET_MODE_PRECISION (as_a <scalar_mode> (nmode)));
	  rtx c = immed_wide_int_const (wval_so_far, nmode);
	  set_dst_reg_note (insn, REG_EQUAL, gen_rtx_MULT (nmode, tem, c),
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
  val &= GET_MODE_MASK (nmode);
  val_so_far &= GET_MODE_MASK (nmode);
  gcc_assert (val == (HOST_WIDE_INT) val_so_far);

  return accum;
}

/* Perform a multiplication and return an rtx for the result.
   MODE is mode of value; OP0 and OP1 are what to multiply (rtx's);
   TARGET is a suggestion for where to store the result (an rtx).

   We check specially for a constant integer as OP1.
   If you want this check for OP0 as well, then before calling
   you should swap the two operands if OP0 would be constant.  */

rtx
expand_mult (machine_mode mode, rtx op0, rtx op1, rtx target,
	     int unsignedp, bool no_libcall)
{
  enum mult_variant variant;
  struct algorithm algorithm;
  rtx scalar_op1;
  int max_cost;
  bool speed = optimize_insn_for_speed_p ();
  bool do_trapv = flag_trapv && SCALAR_INT_MODE_P (mode) && !unsignedp;

  if (CONSTANT_P (op0))
    std::swap (op0, op1);

  /* For vectors, there are several simplifications that can be made if
     all elements of the vector constant are identical.  */
  scalar_op1 = unwrap_const_vec_duplicate (op1);

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

      /* If mode is integer vector mode, check if the backend supports
	 vector lshift (by scalar or vector) at all.  If not, we can't use
	 synthetized multiply.  */
      if (GET_MODE_CLASS (mode) == MODE_VECTOR_INT
	  && optab_handler (vashl_optab, mode) == CODE_FOR_nothing
	  && optab_handler (ashl_optab, mode) == CODE_FOR_nothing)
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
#if TARGET_SUPPORTS_WIDE_INT
      else if (CONST_WIDE_INT_P (scalar_op1))
#else
      else if (CONST_DOUBLE_AS_INT_P (scalar_op1))
#endif
	{
	  int shift = wi::exact_log2 (rtx_mode_t (scalar_op1, mode));
	  /* Perfect power of 2 (other than 1, which is handled above).  */
	  if (shift > 0)
	    return expand_shift (LSHIFT_EXPR, mode, op0,
				 shift, target, unsignedp);
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
	  max_cost = (set_src_cost (gen_rtx_MULT (mode, fake_reg, op1),
				    mode, speed)
		      - neg_cost (speed, mode));
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
      max_cost = set_src_cost (gen_rtx_MULT (mode, fake_reg, op1), mode, speed);
      if (choose_mult_variant (mode, coeff, &algorithm, &variant, max_cost))
	return expand_mult_const (mode, op0, coeff, target,
				  &algorithm, variant);
    }
 skip_synth:

  /* Expand x*2.0 as x+x.  */
  if (CONST_DOUBLE_AS_FLOAT_P (scalar_op1)
      && real_equal (CONST_DOUBLE_REAL_VALUE (scalar_op1), &dconst2))
    {
      op0 = force_reg (GET_MODE (op0), op0);
      return expand_binop (mode, add_optab, op0, op0,
			   target, unsignedp,
			   no_libcall ? OPTAB_WIDEN : OPTAB_LIB_WIDEN);
    }

  /* This used to use umul_optab if unsigned, but for non-widening multiply
     there is no difference between signed and unsigned.  */
  op0 = expand_binop (mode, do_trapv ? smulv_optab : smul_optab,
		      op0, op1, target, unsignedp,
		      no_libcall ? OPTAB_WIDEN : OPTAB_LIB_WIDEN);
  gcc_assert (op0 || no_libcall);
  return op0;
}

/* Return a cost estimate for multiplying a register by the given
   COEFFicient in the given MODE and SPEED.  */

int
mult_by_coeff_cost (HOST_WIDE_INT coeff, machine_mode mode, bool speed)
{
  int max_cost;
  struct algorithm algorithm;
  enum mult_variant variant;

  rtx fake_reg = gen_raw_REG (mode, LAST_VIRTUAL_REGISTER + 1);
  max_cost = set_src_cost (gen_rtx_MULT (mode, fake_reg, fake_reg),
			   mode, speed);
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
expand_widening_mult (machine_mode mode, rtx op0, rtx op1, rtx target,
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

      if (coeff == 0)
	return CONST0_RTX (mode);

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
  int lgup, post_shift;
  int pow, pow2;

  /* lgup = ceil(log2(divisor)); */
  lgup = ceil_log2 (d);

  gcc_assert (lgup <= n);

  pow = n + lgup;
  pow2 = n + lgup - precision;

  /* mlow = 2^(N + lgup)/d */
  wide_int val = wi::set_bit_in_zero (pow, HOST_BITS_PER_DOUBLE_INT);
  wide_int mlow = wi::udiv_trunc (val, d);

  /* mhigh = (2^(N + lgup) + 2^(N + lgup - precision))/d */
  val |= wi::set_bit_in_zero (pow2, HOST_BITS_PER_DOUBLE_INT);
  wide_int mhigh = wi::udiv_trunc (val, d);

  /* If precision == N, then mlow, mhigh exceed 2^N
     (but they do not exceed 2^(N+1)).  */

  /* Reduce to lowest terms.  */
  for (post_shift = lgup; post_shift > 0; post_shift--)
    {
      unsigned HOST_WIDE_INT ml_lo = wi::extract_uhwi (mlow, 1,
						       HOST_BITS_PER_WIDE_INT);
      unsigned HOST_WIDE_INT mh_lo = wi::extract_uhwi (mhigh, 1,
						       HOST_BITS_PER_WIDE_INT);
      if (ml_lo >= mh_lo)
	break;

      mlow = wi::uhwi (ml_lo, HOST_BITS_PER_DOUBLE_INT);
      mhigh = wi::uhwi (mh_lo, HOST_BITS_PER_DOUBLE_INT);
    }

  *post_shift_ptr = post_shift;
  *lgup_ptr = lgup;
  if (n < HOST_BITS_PER_WIDE_INT)
    {
      unsigned HOST_WIDE_INT mask = (HOST_WIDE_INT_1U << n) - 1;
      *multiplier_ptr = mhigh.to_uhwi () & mask;
      return mhigh.to_uhwi () > mask;
    }
  else
    {
      *multiplier_ptr = mhigh.to_uhwi ();
      return wi::extract_uhwi (mhigh, HOST_BITS_PER_WIDE_INT, 1);
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
	  ? HOST_WIDE_INT_M1U
	  : (HOST_WIDE_INT_1U << n) - 1);

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
expand_mult_highpart_adjust (scalar_int_mode mode, rtx adj_operand, rtx op0,
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
extract_high_half (scalar_int_mode mode, rtx op)
{
  if (mode == word_mode)
    return gen_highpart (mode, op);

  scalar_int_mode wider_mode = GET_MODE_WIDER_MODE (mode).require ();

  op = expand_shift (RSHIFT_EXPR, wider_mode, op,
		     GET_MODE_BITSIZE (mode), 0, 1);
  return convert_modes (mode, wider_mode, op, 0);
}

/* Like expmed_mult_highpart, but only consider using a multiplication
   optab.  OP1 is an rtx for the constant operand.  */

static rtx
expmed_mult_highpart_optab (scalar_int_mode mode, rtx op0, rtx op1,
			    rtx target, int unsignedp, int max_cost)
{
  rtx narrow_op1 = gen_int_mode (INTVAL (op1), mode);
  optab moptab;
  rtx tem;
  int size;
  bool speed = optimize_insn_for_speed_p ();

  scalar_int_mode wider_mode = GET_MODE_WIDER_MODE (mode).require ();

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
  if (convert_optab_handler (moptab, wider_mode, mode) != CODE_FOR_nothing
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
      rtx_insn *insns;
      rtx wop0, wop1;

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
  if (convert_optab_handler (moptab, wider_mode, mode) != CODE_FOR_nothing
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
expmed_mult_highpart (scalar_int_mode mode, rtx op0, rtx op1,
		      rtx target, int unsignedp, int max_cost)
{
  unsigned HOST_WIDE_INT cnst1;
  int extra_cost;
  bool sign_adjust = false;
  enum mult_variant variant;
  struct algorithm alg;
  rtx tem;
  bool speed = optimize_insn_for_speed_p ();

  /* We can't support modes wider than HOST_BITS_PER_INT.  */
  gcc_assert (HWI_COMPUTABLE_MODE_P (mode));

  cnst1 = INTVAL (op1) & GET_MODE_MASK (mode);

  /* We can't optimize modes wider than BITS_PER_WORD.
     ??? We might be able to perform double-word arithmetic if
     mode == word_mode, however all the cost calculations in
     synth_mult etc. assume single-word operations.  */
  scalar_int_mode wider_mode = GET_MODE_WIDER_MODE (mode).require ();
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
expand_smod_pow2 (scalar_int_mode mode, rtx op0, HOST_WIDE_INT d)
{
  rtx result, temp, shift;
  rtx_code_label *label;
  int logd;
  int prec = GET_MODE_PRECISION (mode);

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
	  HOST_WIDE_INT masklow = (HOST_WIDE_INT_1 << logd) - 1;
	  signmask = force_reg (mode, signmask);
	  shift = gen_int_shift_amount (mode, GET_MODE_BITSIZE (mode) - logd);

	  /* Use the rtx_cost of a LSHIFTRT instruction to determine
	     which instruction sequence to use.  If logical right shifts
	     are expensive the use 2 XORs, 2 SUBs and an AND, otherwise
	     use a LSHIFTRT, 1 ADD, 1 SUB and an AND.  */

	  temp = gen_rtx_LSHIFTRT (mode, result, shift);
	  if (optab_handler (lshr_optab, mode) == CODE_FOR_nothing
	      || (set_src_cost (temp, mode, optimize_insn_for_speed_p ())
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
  wide_int mask = wi::mask (logd, false, prec);
  mask = wi::set_bit (mask, prec - 1);

  temp = expand_binop (mode, and_optab, op0,
		       immed_wide_int_const (mask, mode),
		       result, 1, OPTAB_LIB_WIDEN);
  if (temp != result)
    emit_move_insn (result, temp);

  label = gen_label_rtx ();
  do_cmp_and_jump (result, const0_rtx, GE, mode, label);

  temp = expand_binop (mode, sub_optab, result, const1_rtx, result,
		       0, OPTAB_LIB_WIDEN);

  mask = wi::mask (logd, true, prec);
  temp = expand_binop (mode, ior_optab, temp,
		       immed_wide_int_const (mask, mode),
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
expand_sdiv_pow2 (scalar_int_mode mode, rtx op0, HOST_WIDE_INT d)
{
  rtx temp;
  rtx_code_label *label;
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

  if (HAVE_conditional_move
      && BRANCH_COST (optimize_insn_for_speed_p (), false) >= 2)
    {
      rtx temp2;

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
	  rtx_insn *seq = get_insns ();
	  end_sequence ();
	  emit_insn (seq);
	  return expand_shift (RSHIFT_EXPR, mode, temp2, logd, NULL_RTX, 0);
	}
      end_sequence ();
    }

  if (BRANCH_COST (optimize_insn_for_speed_p (),
		   false) >= 2)
    {
      int ushift = GET_MODE_BITSIZE (mode) - logd;

      temp = gen_reg_rtx (mode);
      temp = emit_store_flag (temp, LT, op0, const0_rtx, mode, 0, -1);
      if (GET_MODE_BITSIZE (mode) >= BITS_PER_WORD
	  || shift_cost (optimize_insn_for_speed_p (), mode, ushift)
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
expand_divmod (int rem_flag, enum tree_code code, machine_mode mode,
	       rtx op0, rtx op1, rtx target, int unsignedp)
{
  machine_mode compute_mode;
  rtx tquotient;
  rtx quotient = 0, remainder = 0;
  rtx_insn *last;
  rtx_insn *insn;
  optab optab1, optab2;
  int op1_is_constant, op1_is_pow2 = 0;
  int max_cost, extra_cost;
  static HOST_WIDE_INT last_div_const = 0;
  bool speed = optimize_insn_for_speed_p ();

  op1_is_constant = CONST_INT_P (op1);
  if (op1_is_constant)
    {
      wide_int ext_op1 = rtx_mode_t (op1, mode);
      op1_is_pow2 = (wi::popcount (ext_op1) == 1
		     || (! unsignedp
			 && wi::popcount (wi::neg (ext_op1)) == 1));
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
      return expand_unop (mode, flag_trapv && GET_MODE_CLASS (mode) == MODE_INT
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

  optab1 = (op1_is_pow2
	    ? (unsignedp ? lshr_optab : ashr_optab)
	    : (unsignedp ? udiv_optab : sdiv_optab));
  optab2 = (op1_is_pow2 ? optab1
	    : (unsignedp ? udivmod_optab : sdivmod_optab));

  FOR_EACH_MODE_FROM (compute_mode, mode)
    if (optab_handler (optab1, compute_mode) != CODE_FOR_nothing
	|| optab_handler (optab2, compute_mode) != CODE_FOR_nothing)
      break;

  if (compute_mode == VOIDmode)
    FOR_EACH_MODE_FROM (compute_mode, mode)
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
      if (op1_is_constant)
	{
	  wide_int ext_op1 = rtx_mode_t (op1, compute_mode);
	  op1_is_pow2 = (wi::popcount (ext_op1) == 1
			 || (! unsignedp
			     && wi::popcount (wi::neg (ext_op1)) == 1));
	}
      else
	op1_is_pow2 = 0;
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
	    scalar_int_mode int_mode = as_a <scalar_int_mode> (compute_mode);
	    int size = GET_MODE_BITSIZE (int_mode);
	    if (unsignedp)
	      {
		unsigned HOST_WIDE_INT mh, ml;
		int pre_shift, post_shift;
		int dummy;
		wide_int wd = rtx_mode_t (op1, int_mode);
		unsigned HOST_WIDE_INT d = wd.to_uhwi ();

		if (wi::popcount (wd) == 1)
		  {
		    pre_shift = floor_log2 (d);
		    if (rem_flag)
		      {
			unsigned HOST_WIDE_INT mask
			  = (HOST_WIDE_INT_1U << pre_shift) - 1;
			remainder
			  = expand_binop (int_mode, and_optab, op0,
					  gen_int_mode (mask, int_mode),
					  remainder, 1,
					  OPTAB_LIB_WIDEN);
			if (remainder)
			  return gen_lowpart (mode, remainder);
		      }
		    quotient = expand_shift (RSHIFT_EXPR, int_mode, op0,
					     pre_shift, tquotient, 1);
		  }
		else if (size <= HOST_BITS_PER_WIDE_INT)
		  {
		    if (d >= (HOST_WIDE_INT_1U << (size - 1)))
		      {
			/* Most significant bit of divisor is set; emit an scc
			   insn.  */
			quotient = emit_store_flag_force (tquotient, GEU, op0, op1,
							  int_mode, 1, 1);
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
			    pre_shift = ctz_or_zero (d);
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
			      = (shift_cost (speed, int_mode, post_shift - 1)
				 + shift_cost (speed, int_mode, 1)
				 + 2 * add_cost (speed, int_mode));
			    t1 = expmed_mult_highpart
			      (int_mode, op0, gen_int_mode (ml, int_mode),
			       NULL_RTX, 1, max_cost - extra_cost);
			    if (t1 == 0)
			      goto fail1;
			    t2 = force_operand (gen_rtx_MINUS (int_mode,
							       op0, t1),
						NULL_RTX);
			    t3 = expand_shift (RSHIFT_EXPR, int_mode,
					       t2, 1, NULL_RTX, 1);
			    t4 = force_operand (gen_rtx_PLUS (int_mode,
							      t1, t3),
						NULL_RTX);
			    quotient = expand_shift
			      (RSHIFT_EXPR, int_mode, t4,
			       post_shift - 1, tquotient, 1);
			  }
			else
			  {
			    rtx t1, t2;

			    if (pre_shift >= BITS_PER_WORD
				|| post_shift >= BITS_PER_WORD)
			      goto fail1;

			    t1 = expand_shift
			      (RSHIFT_EXPR, int_mode, op0,
			       pre_shift, NULL_RTX, 1);
			    extra_cost
			      = (shift_cost (speed, int_mode, pre_shift)
				 + shift_cost (speed, int_mode, post_shift));
			    t2 = expmed_mult_highpart
			      (int_mode, t1,
			       gen_int_mode (ml, int_mode),
			       NULL_RTX, 1, max_cost - extra_cost);
			    if (t2 == 0)
			      goto fail1;
			    quotient = expand_shift
			      (RSHIFT_EXPR, int_mode, t2,
			       post_shift, tquotient, 1);
			  }
		      }
		  }
		else		/* Too wide mode to use tricky code */
		  break;

		insn = get_last_insn ();
		if (insn != last)
		  set_dst_reg_note (insn, REG_EQUAL,
				    gen_rtx_UDIV (int_mode, op0, op1),
				    quotient);
	      }
	    else		/* TRUNC_DIV, signed */
	      {
		unsigned HOST_WIDE_INT ml;
		int lgup, post_shift;
		rtx mlr;
		HOST_WIDE_INT d = INTVAL (op1);
		unsigned HOST_WIDE_INT abs_d;

		/* Not prepared to handle division/remainder by
		   0xffffffffffffffff8000000000000000 etc.  */
		if (d == HOST_WIDE_INT_MIN && size > HOST_BITS_PER_WIDE_INT)
		  break;

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
		    op1 = gen_int_mode (abs_d, int_mode);
		  }

		if (d == 1)
		  quotient = op0;
		else if (d == -1)
		  quotient = expand_unop (int_mode, neg_optab, op0,
					  tquotient, 0);
		else if (size <= HOST_BITS_PER_WIDE_INT
			 && abs_d == HOST_WIDE_INT_1U << (size - 1))
		  {
		    /* This case is not handled correctly below.  */
		    quotient = emit_store_flag (tquotient, EQ, op0, op1,
						int_mode, 1, 1);
		    if (quotient == 0)
		      goto fail1;
		  }
		else if (EXACT_POWER_OF_2_OR_ZERO_P (d)
			 && (size <= HOST_BITS_PER_WIDE_INT || d >= 0)
			 && (rem_flag
			     ? smod_pow2_cheap (speed, int_mode)
			     : sdiv_pow2_cheap (speed, int_mode))
			 /* We assume that cheap metric is true if the
			    optab has an expander for this mode.  */
			 && ((optab_handler ((rem_flag ? smod_optab
					      : sdiv_optab),
					     int_mode)
			      != CODE_FOR_nothing)
			     || (optab_handler (sdivmod_optab, int_mode)
				 != CODE_FOR_nothing)))
		  ;
		else if (EXACT_POWER_OF_2_OR_ZERO_P (abs_d))
		  {
		    if (rem_flag)
		      {
			remainder = expand_smod_pow2 (int_mode, op0, d);
			if (remainder)
			  return gen_lowpart (mode, remainder);
		      }

		    if (sdiv_pow2_cheap (speed, int_mode)
			&& ((optab_handler (sdiv_optab, int_mode)
			     != CODE_FOR_nothing)
			    || (optab_handler (sdivmod_optab, int_mode)
				!= CODE_FOR_nothing)))
		      quotient = expand_divmod (0, TRUNC_DIV_EXPR,
						int_mode, op0,
						gen_int_mode (abs_d,
							      int_mode),
						NULL_RTX, 0);
		    else
		      quotient = expand_sdiv_pow2 (int_mode, op0, abs_d);

		    /* We have computed OP0 / abs(OP1).  If OP1 is negative,
		       negate the quotient.  */
		    if (d < 0)
		      {
			insn = get_last_insn ();
			if (insn != last
			    && abs_d < (HOST_WIDE_INT_1U
					<< (HOST_BITS_PER_WIDE_INT - 1)))
			  set_dst_reg_note (insn, REG_EQUAL,
					    gen_rtx_DIV (int_mode, op0,
							 gen_int_mode
							   (abs_d,
							    int_mode)),
					    quotient);

			quotient = expand_unop (int_mode, neg_optab,
						quotient, quotient, 0);
		      }
		  }
		else if (size <= HOST_BITS_PER_WIDE_INT)
		  {
		    choose_multiplier (abs_d, size, size - 1,
				       &ml, &post_shift, &lgup);
		    if (ml < HOST_WIDE_INT_1U << (size - 1))
		      {
			rtx t1, t2, t3;

			if (post_shift >= BITS_PER_WORD
			    || size - 1 >= BITS_PER_WORD)
			  goto fail1;

			extra_cost = (shift_cost (speed, int_mode, post_shift)
				      + shift_cost (speed, int_mode, size - 1)
				      + add_cost (speed, int_mode));
			t1 = expmed_mult_highpart
			  (int_mode, op0, gen_int_mode (ml, int_mode),
			   NULL_RTX, 0, max_cost - extra_cost);
			if (t1 == 0)
			  goto fail1;
			t2 = expand_shift
			  (RSHIFT_EXPR, int_mode, t1,
			   post_shift, NULL_RTX, 0);
			t3 = expand_shift
			  (RSHIFT_EXPR, int_mode, op0,
			   size - 1, NULL_RTX, 0);
			if (d < 0)
			  quotient
			    = force_operand (gen_rtx_MINUS (int_mode, t3, t2),
					     tquotient);
			else
			  quotient
			    = force_operand (gen_rtx_MINUS (int_mode, t2, t3),
					     tquotient);
		      }
		    else
		      {
			rtx t1, t2, t3, t4;

			if (post_shift >= BITS_PER_WORD
			    || size - 1 >= BITS_PER_WORD)
			  goto fail1;

			ml |= HOST_WIDE_INT_M1U << (size - 1);
			mlr = gen_int_mode (ml, int_mode);
			extra_cost = (shift_cost (speed, int_mode, post_shift)
				      + shift_cost (speed, int_mode, size - 1)
				      + 2 * add_cost (speed, int_mode));
			t1 = expmed_mult_highpart (int_mode, op0, mlr,
						   NULL_RTX, 0,
						   max_cost - extra_cost);
			if (t1 == 0)
			  goto fail1;
			t2 = force_operand (gen_rtx_PLUS (int_mode, t1, op0),
					    NULL_RTX);
			t3 = expand_shift
			  (RSHIFT_EXPR, int_mode, t2,
			   post_shift, NULL_RTX, 0);
			t4 = expand_shift
			  (RSHIFT_EXPR, int_mode, op0,
			   size - 1, NULL_RTX, 0);
			if (d < 0)
			  quotient
			    = force_operand (gen_rtx_MINUS (int_mode, t4, t3),
					     tquotient);
			else
			  quotient
			    = force_operand (gen_rtx_MINUS (int_mode, t3, t4),
					     tquotient);
		      }
		  }
		else		/* Too wide mode to use tricky code */
		  break;

		insn = get_last_insn ();
		if (insn != last)
		  set_dst_reg_note (insn, REG_EQUAL,
				    gen_rtx_DIV (int_mode, op0, op1),
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
	if (op1_is_constant && HWI_COMPUTABLE_MODE_P (compute_mode))
	  {
	    scalar_int_mode int_mode = as_a <scalar_int_mode> (compute_mode);
	    int size = GET_MODE_BITSIZE (int_mode);
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
			  = (HOST_WIDE_INT_1U << pre_shift) - 1;
			remainder = expand_binop
			  (int_mode, and_optab, op0,
			   gen_int_mode (mask, int_mode),
			   remainder, 0, OPTAB_LIB_WIDEN);
			if (remainder)
			  return gen_lowpart (mode, remainder);
		      }
		    quotient = expand_shift
		      (RSHIFT_EXPR, int_mode, op0,
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
			  (RSHIFT_EXPR, int_mode, op0,
			   size - 1, NULL_RTX, 0);
			t2 = expand_binop (int_mode, xor_optab, op0, t1,
					   NULL_RTX, 0, OPTAB_WIDEN);
			extra_cost = (shift_cost (speed, int_mode, post_shift)
				      + shift_cost (speed, int_mode, size - 1)
				      + 2 * add_cost (speed, int_mode));
			t3 = expmed_mult_highpart
			  (int_mode, t2, gen_int_mode (ml, int_mode),
			   NULL_RTX, 1, max_cost - extra_cost);
			if (t3 != 0)
			  {
			    t4 = expand_shift
			      (RSHIFT_EXPR, int_mode, t3,
			       post_shift, NULL_RTX, 1);
			    quotient = expand_binop (int_mode, xor_optab,
						     t4, t1, tquotient, 0,
						     OPTAB_WIDEN);
			  }
		      }
		  }
	      }
	    else
	      {
		rtx nsign, t1, t2, t3, t4;
		t1 = force_operand (gen_rtx_PLUS (int_mode,
						  op0, constm1_rtx), NULL_RTX);
		t2 = expand_binop (int_mode, ior_optab, op0, t1, NULL_RTX,
				   0, OPTAB_WIDEN);
		nsign = expand_shift (RSHIFT_EXPR, int_mode, t2,
				      size - 1, NULL_RTX, 0);
		t3 = force_operand (gen_rtx_MINUS (int_mode, t1, nsign),
				    NULL_RTX);
		t4 = expand_divmod (0, TRUNC_DIV_EXPR, int_mode, t3, op1,
				    NULL_RTX, 0);
		if (t4)
		  {
		    rtx t5;
		    t5 = expand_unop (int_mode, one_cmpl_optab, nsign,
				      NULL_RTX, 0);
		    quotient = force_operand (gen_rtx_PLUS (int_mode, t4, t5),
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
	    rtx_code_label *label = gen_label_rtx ();
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
	  rtx_code_label *label1, *label2, *label3, *label4, *label5;
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
	  emit_jump_insn (targetm.gen_jump (label5));
	  emit_barrier ();
	  emit_label (label1);
	  expand_inc (adjusted_op0, const1_rtx);
	  emit_jump_insn (targetm.gen_jump (label4));
	  emit_barrier ();
	  emit_label (label2);
	  do_cmp_and_jump (adjusted_op0, const0_rtx, GT, compute_mode, label3);
	  tem = expand_binop (compute_mode, sdiv_optab, adjusted_op0, op1,
			      quotient, 0, OPTAB_LIB_WIDEN);
	  if (tem != quotient)
	    emit_move_insn (quotient, tem);
	  emit_jump_insn (targetm.gen_jump (label5));
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
	    if (op1_is_constant
		&& EXACT_POWER_OF_2_OR_ZERO_P (INTVAL (op1))
		&& (HWI_COMPUTABLE_MODE_P (compute_mode)
		    || INTVAL (op1) >= 0))
	      {
		scalar_int_mode int_mode
		  = as_a <scalar_int_mode> (compute_mode);
		rtx t1, t2, t3;
		unsigned HOST_WIDE_INT d = INTVAL (op1);
		t1 = expand_shift (RSHIFT_EXPR, int_mode, op0,
				   floor_log2 (d), tquotient, 1);
		t2 = expand_binop (int_mode, and_optab, op0,
				   gen_int_mode (d - 1, int_mode),
				   NULL_RTX, 1, OPTAB_LIB_WIDEN);
		t3 = gen_reg_rtx (int_mode);
		t3 = emit_store_flag (t3, NE, t2, const0_rtx, int_mode, 1, 1);
		if (t3 == 0)
		  {
		    rtx_code_label *lab;
		    lab = gen_label_rtx ();
		    do_cmp_and_jump (t2, const0_rtx, EQ, int_mode, lab);
		    expand_inc (t1, const1_rtx);
		    emit_label (lab);
		    quotient = t1;
		  }
		else
		  quotient = force_operand (gen_rtx_PLUS (int_mode, t1, t3),
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
		rtx_code_label *label = gen_label_rtx ();
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
	      rtx_code_label *label1, *label2;
	      rtx adjusted_op0, tem;

	      quotient = gen_reg_rtx (compute_mode);
	      adjusted_op0 = copy_to_mode_reg (compute_mode, op0);
	      label1 = gen_label_rtx ();
	      label2 = gen_label_rtx ();
	      do_cmp_and_jump (adjusted_op0, const0_rtx, NE,
			       compute_mode, label1);
	      emit_move_insn  (quotient, const0_rtx);
	      emit_jump_insn (targetm.gen_jump (label2));
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
		    rtx_code_label *lab;
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
		rtx_code_label *label = gen_label_rtx ();
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
	      rtx_code_label *label1, *label2, *label3, *label4, *label5;
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
	      emit_jump_insn (targetm.gen_jump (label5));
	      emit_barrier ();
	      emit_label (label1);
	      expand_dec (adjusted_op0, const1_rtx);
	      emit_jump_insn (targetm.gen_jump (label4));
	      emit_barrier ();
	      emit_label (label2);
	      do_cmp_and_jump (adjusted_op0, const0_rtx, LT,
			       compute_mode, label3);
	      tem = expand_binop (compute_mode, sdiv_optab, adjusted_op0, op1,
				  quotient, 0, OPTAB_LIB_WIDEN);
	      if (tem != quotient)
		emit_move_insn (quotient, tem);
	      emit_jump_insn (targetm.gen_jump (label5));
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
	if (op1_is_constant && HWI_COMPUTABLE_MODE_P (compute_mode))
	  {
	    scalar_int_mode int_mode = as_a <scalar_int_mode> (compute_mode);
	    int size = GET_MODE_BITSIZE (int_mode);
	    HOST_WIDE_INT d = INTVAL (op1);
	    unsigned HOST_WIDE_INT ml;
	    int pre_shift;
	    rtx t1;

	    pre_shift = ctz_or_zero (d);
	    ml = invert_mod2n (d >> pre_shift, size);
	    t1 = expand_shift (RSHIFT_EXPR, int_mode, op0,
			       pre_shift, NULL_RTX, unsignedp);
	    quotient = expand_mult (int_mode, t1, gen_int_mode (ml, int_mode),
				    NULL_RTX, 1);

	    insn = get_last_insn ();
	    set_dst_reg_note (insn, REG_EQUAL,
			      gen_rtx_fmt_ee (unsignedp ? UDIV : DIV,
					      int_mode, op0, op1),
			      quotient);
	  }
	break;

      case ROUND_DIV_EXPR:
      case ROUND_MOD_EXPR:
	if (unsignedp)
	  {
	    scalar_int_mode int_mode = as_a <scalar_int_mode> (compute_mode);
	    rtx tem;
	    rtx_code_label *label;
	    label = gen_label_rtx ();
	    quotient = gen_reg_rtx (int_mode);
	    remainder = gen_reg_rtx (int_mode);
	    if (expand_twoval_binop (udivmod_optab, op0, op1, quotient, remainder, 1) == 0)
	      {
		rtx tem;
		quotient = expand_binop (int_mode, udiv_optab, op0, op1,
					 quotient, 1, OPTAB_LIB_WIDEN);
		tem = expand_mult (int_mode, quotient, op1, NULL_RTX, 1);
		remainder = expand_binop (int_mode, sub_optab, op0, tem,
					  remainder, 1, OPTAB_LIB_WIDEN);
	      }
	    tem = plus_constant (int_mode, op1, -1);
	    tem = expand_shift (RSHIFT_EXPR, int_mode, tem, 1, NULL_RTX, 1);
	    do_cmp_and_jump (remainder, tem, LEU, int_mode, label);
	    expand_inc (quotient, const1_rtx);
	    expand_dec (remainder, op1);
	    emit_label (label);
	  }
	else
	  {
	    scalar_int_mode int_mode = as_a <scalar_int_mode> (compute_mode);
	    int size = GET_MODE_BITSIZE (int_mode);
	    rtx abs_rem, abs_op1, tem, mask;
	    rtx_code_label *label;
	    label = gen_label_rtx ();
	    quotient = gen_reg_rtx (int_mode);
	    remainder = gen_reg_rtx (int_mode);
	    if (expand_twoval_binop (sdivmod_optab, op0, op1, quotient, remainder, 0) == 0)
	      {
		rtx tem;
		quotient = expand_binop (int_mode, sdiv_optab, op0, op1,
					 quotient, 0, OPTAB_LIB_WIDEN);
		tem = expand_mult (int_mode, quotient, op1, NULL_RTX, 0);
		remainder = expand_binop (int_mode, sub_optab, op0, tem,
					  remainder, 0, OPTAB_LIB_WIDEN);
	      }
	    abs_rem = expand_abs (int_mode, remainder, NULL_RTX, 1, 0);
	    abs_op1 = expand_abs (int_mode, op1, NULL_RTX, 1, 0);
	    tem = expand_shift (LSHIFT_EXPR, int_mode, abs_rem,
				1, NULL_RTX, 1);
	    do_cmp_and_jump (tem, abs_op1, LTU, int_mode, label);
	    tem = expand_binop (int_mode, xor_optab, op0, op1,
				NULL_RTX, 0, OPTAB_WIDEN);
	    mask = expand_shift (RSHIFT_EXPR, int_mode, tem,
				 size - 1, NULL_RTX, 0);
	    tem = expand_binop (int_mode, xor_optab, mask, const1_rtx,
				NULL_RTX, 0, OPTAB_WIDEN);
	    tem = expand_binop (int_mode, sub_optab, tem, mask,
				NULL_RTX, 0, OPTAB_WIDEN);
	    expand_inc (quotient, tem);
	    tem = expand_binop (int_mode, xor_optab, mask, op1,
				NULL_RTX, 0, OPTAB_WIDEN);
	    tem = expand_binop (int_mode, sub_optab, tem, mask,
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
    case CONST_WIDE_INT:
      t = wide_int_to_tree (type, rtx_mode_t (x, TYPE_MODE (type)));
      return t;

    case CONST_DOUBLE:
      STATIC_ASSERT (HOST_BITS_PER_WIDE_INT * 2 <= MAX_BITSIZE_MODE_ANY_INT);
      if (TARGET_SUPPORTS_WIDE_INT == 0 && GET_MODE (x) == VOIDmode)
	t = wide_int_to_tree (type,
			      wide_int::from_array (&CONST_DOUBLE_LOW (x), 2,
						    HOST_BITS_PER_WIDE_INT * 2));
      else
	t = build_real (type, *CONST_DOUBLE_REAL_VALUE (x));

      return t;

    case CONST_VECTOR:
      {
	unsigned int npatterns = CONST_VECTOR_NPATTERNS (x);
	unsigned int nelts_per_pattern = CONST_VECTOR_NELTS_PER_PATTERN (x);
	tree itype = TREE_TYPE (type);

	/* Build a tree with vector elements.  */
	tree_vector_builder elts (type, npatterns, nelts_per_pattern);
	unsigned int count = elts.encoded_nelts ();
	for (unsigned int i = 0; i < count; ++i)
	  {
	    rtx elt = CONST_VECTOR_ELT (x, i);
	    elts.quick_push (make_tree (itype, elt));
	  }

	return elts.build ();
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
      /* fall through.  */

    default:
      if (CONST_POLY_INT_P (x))
	return wide_int_to_tree (t, const_poly_int_value (x));

      t = build_decl (RTL_LOCATION (x), VAR_DECL, NULL_TREE, type);

      /* If TYPE is a POINTER_TYPE, we might need to convert X from
	 address mode to pointer mode.  */
      if (POINTER_TYPE_P (type))
	x = convert_memory_address_addr_space
	  (SCALAR_INT_TYPE_MODE (type), x, TYPE_ADDR_SPACE (TREE_TYPE (type)));

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
expand_and (machine_mode mode, rtx op0, rtx op1, rtx target)
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
rtx
emit_cstore (rtx target, enum insn_code icode, enum rtx_code code,
	     machine_mode mode, machine_mode compare_mode,
	     int unsignedp, rtx x, rtx y, int normalizep,
	     machine_mode target_mode)
{
  struct expand_operand ops[4];
  rtx op0, comparison, subtarget;
  rtx_insn *last;
  scalar_int_mode result_mode = targetm.cstore_mode (icode);
  scalar_int_mode int_target_mode;

  last = get_last_insn ();
  x = prepare_operand (icode, x, 2, mode, compare_mode, unsignedp);
  y = prepare_operand (icode, y, 3, mode, compare_mode, unsignedp);
  if (!x || !y)
    {
      delete_insns_since (last);
      return NULL_RTX;
    }

  if (target_mode == VOIDmode)
    int_target_mode = result_mode;
  else
    int_target_mode = as_a <scalar_int_mode> (target_mode);
  if (!target)
    target = gen_reg_rtx (int_target_mode);

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
     INT_TARGET_MODE, then normalize.  This produces better combining
     opportunities on machines that have a SIGN_EXTRACT when we are
     testing a single bit.  This mostly benefits the 68k.

     If STORE_FLAG_VALUE does not have the sign bit set when
     interpreted in MODE, we can do this conversion as unsigned, which
     is usually more efficient.  */
  if (GET_MODE_PRECISION (int_target_mode) > GET_MODE_PRECISION (result_mode))
    {
      gcc_assert (GET_MODE_PRECISION (result_mode) != 1
		  || STORE_FLAG_VALUE == 1 || STORE_FLAG_VALUE == -1);

      bool unsignedp = (STORE_FLAG_VALUE >= 0);
      convert_move (target, subtarget, unsignedp);

      op0 = target;
      result_mode = int_target_mode;
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
  if (int_target_mode != result_mode)
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
		   machine_mode mode, int unsignedp, int normalizep,
		   machine_mode target_mode)
{
  rtx subtarget;
  enum insn_code icode;
  machine_mode compare_mode;
  enum mode_class mclass;
  enum rtx_code scode;

  if (unsignedp)
    code = unsigned_condition (code);
  scode = swap_condition (code);

  /* If one operand is constant, make it the second one.  Only do this
     if the other operand is not constant as well.  */

  if (swap_commutative_operands_p (op0, op1))
    {
      std::swap (op0, op1);
      code = swap_condition (code);
    }

  if (mode == VOIDmode)
    mode = GET_MODE (op0);

  if (CONST_SCALAR_INT_P (op1))
    canonicalize_comparison (mode, &code, &op1);

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
  scalar_int_mode int_mode;
  if (is_int_mode (mode, &int_mode)
      && GET_MODE_BITSIZE (int_mode) == BITS_PER_WORD * 2
      && (!MEM_P (op0) || ! MEM_VOLATILE_P (op0)))
    {
      rtx tem;
      if ((code == EQ || code == NE)
	  && (op1 == const0_rtx || op1 == constm1_rtx))
	{
	  rtx op00, op01;

	  /* Do a logical OR or AND of the two words and compare the
	     result.  */
	  op00 = simplify_gen_subreg (word_mode, op0, int_mode, 0);
	  op01 = simplify_gen_subreg (word_mode, op0, int_mode, UNITS_PER_WORD);
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
	  op0h = simplify_gen_subreg (word_mode, op0, int_mode,
				      subreg_highpart_offset (word_mode,
							      int_mode));
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
      && is_int_mode (mode, &int_mode)
      && (normalizep || STORE_FLAG_VALUE == 1
	  || val_signbit_p (int_mode, STORE_FLAG_VALUE)))
    {
      scalar_int_mode int_target_mode;
      subtarget = target;

      if (!target)
	int_target_mode = int_mode;
      else
	{
	  /* If the result is to be wider than OP0, it is best to convert it
	     first.  If it is to be narrower, it is *incorrect* to convert it
	     first.  */
	  int_target_mode = as_a <scalar_int_mode> (target_mode);
	  if (GET_MODE_SIZE (int_target_mode) > GET_MODE_SIZE (int_mode))
	    {
	      op0 = convert_modes (int_target_mode, int_mode, op0, 0);
	      int_mode = int_target_mode;
	    }
	}

      if (int_target_mode != int_mode)
	subtarget = 0;

      if (code == GE)
	op0 = expand_unop (int_mode, one_cmpl_optab, op0,
			   ((STORE_FLAG_VALUE == 1 || normalizep)
			    ? 0 : subtarget), 0);

      if (STORE_FLAG_VALUE == 1 || normalizep)
	/* If we are supposed to produce a 0/1 value, we want to do
	   a logical shift from the sign bit to the low-order bit; for
	   a -1/0 value, we do an arithmetic shift.  */
	op0 = expand_shift (RSHIFT_EXPR, int_mode, op0,
			    GET_MODE_BITSIZE (int_mode) - 1,
			    subtarget, normalizep != -1);

      if (int_mode != int_target_mode)
	op0 = convert_modes (int_target_mode, int_mode, op0, 0);

      return op0;
    }

  mclass = GET_MODE_CLASS (mode);
  FOR_EACH_MODE_FROM (compare_mode, mode)
    {
     machine_mode optab_mode = mclass == MODE_CC ? CCmode : compare_mode;
     icode = optab_handler (cstore_optab, optab_mode);
     if (icode != CODE_FOR_nothing)
	{
	  do_pending_stack_adjust ();
	  rtx tem = emit_cstore (target, icode, code, mode, compare_mode,
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

/* Subroutine of emit_store_flag that handles cases in which the operands
   are scalar integers.  SUBTARGET is the target to use for temporary
   operations and TRUEVAL is the value to store when the condition is
   true.  All other arguments are as for emit_store_flag.  */

rtx
emit_store_flag_int (rtx target, rtx subtarget, enum rtx_code code, rtx op0,
		     rtx op1, scalar_int_mode mode, int unsignedp,
		     int normalizep, rtx trueval)
{
  machine_mode target_mode = target ? GET_MODE (target) : VOIDmode;
  rtx_insn *last = get_last_insn ();

  /* If this is an equality comparison of integers, we can try to exclusive-or
     (or subtract) the two operands and use a recursive call to try the
     comparison with zero.  Don't do any of these cases if branches are
     very cheap.  */

  if ((code == EQ || code == NE) && op1 != const0_rtx)
    {
      rtx tem = expand_binop (mode, xor_optab, op0, op1, subtarget, 1,
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
  rtx_code rcode = reverse_condition (code);
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
	  && rtx_cost (GEN_INT (normalizep), mode, PLUS, 1,
		       optimize_insn_for_speed_p ()) == 0)
	{
	  rtx tem = emit_store_flag_1 (subtarget, rcode, op0, op1, mode, 0,
				       STORE_FLAG_VALUE, target_mode);
	  if (tem != 0)
	    tem = expand_binop (target_mode, add_optab, tem,
				gen_int_mode (normalizep, target_mode),
				target, 0, OPTAB_WIDEN);
	  if (tem != 0)
	    return tem;
	}
      else if (!want_add
	       && rtx_cost (trueval, mode, XOR, 1,
			    optimize_insn_for_speed_p ()) == 0)
	{
	  rtx tem = emit_store_flag_1 (subtarget, rcode, op0, op1, mode, 0,
				       normalizep, target_mode);
	  if (tem != 0)
	    tem = expand_binop (target_mode, xor_optab, tem, trueval, target,
				INTVAL (trueval) >= 0, OPTAB_WIDEN);
	  if (tem != 0)
	    return tem;
	}

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

  rtx tem = 0;

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

      tem = maybe_expand_shift (RSHIFT_EXPR, mode, op0,
				GET_MODE_BITSIZE (mode) - 1,
				subtarget, 0);
      if (tem)
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
    tem = maybe_expand_shift (RSHIFT_EXPR, mode, tem,
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
		 machine_mode mode, int unsignedp, int normalizep)
{
  machine_mode target_mode = target ? GET_MODE (target) : VOIDmode;
  enum rtx_code rcode;
  rtx subtarget;
  rtx tem, trueval;
  rtx_insn *last;

  /* If we compare constants, we shouldn't use a store-flag operation,
     but a constant load.  We can get there via the vanilla route that
     usually generates a compare-branch sequence, but will in this case
     fold the comparison to a constant, and thus elide the branch.  */
  if (CONSTANT_P (op0) && CONSTANT_P (op1))
    return NULL_RTX;

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
     down the compiler, since there are more pseudos.  */
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
	      && rtx_cost (GEN_INT (normalizep), mode, PLUS, 1,
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
		   && rtx_cost (trueval, mode, XOR, 1,
				optimize_insn_for_speed_p ()) == 0)
	    {
	      tem = emit_store_flag_1 (subtarget, rcode, op0, op1, mode, 0,
				       normalizep, target_mode);
	      if (tem)
		return expand_binop (target_mode, xor_optab, tem, trueval,
				     target, INTVAL (trueval) >= 0,
				     OPTAB_WIDEN);
	    }
	}

      delete_insns_since (last);

      /* Cannot split ORDERED and UNORDERED, only try the above trick.  */
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

      if (!HAVE_conditional_move)
	return 0;

      /* Do not turn a trapping comparison into a non-trapping one.  */
      if ((code != EQ && code != NE && code != UNEQ && code != LTGT)
	  && flag_trapping_math)
	return 0;

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
    }

  /* The remaining tricks only apply to integer comparisons.  */

  scalar_int_mode int_mode;
  if (is_int_mode (mode, &int_mode))
    return emit_store_flag_int (target, subtarget, code, op0, op1, int_mode,
				unsignedp, normalizep, trueval);

  return 0;
}

/* Like emit_store_flag, but always succeeds.  */

rtx
emit_store_flag_force (rtx target, enum rtx_code code, rtx op0, rtx op1,
		       machine_mode mode, int unsignedp, int normalizep)
{
  rtx tem;
  rtx_code_label *label;
  rtx trueval, falseval;

  /* First see if emit_store_flag can do the job.  */
  tem = emit_store_flag (target, code, op0, op1, mode, unsignedp, normalizep);
  if (tem != 0)
    return tem;

  /* If one operand is constant, make it the second one.  Only do this
     if the other operand is not constant as well.  */
  if (swap_commutative_operands_p (op0, op1))
    {
      std::swap (op0, op1);
      code = swap_condition (code);
    }

  if (mode == VOIDmode)
    mode = GET_MODE (op0);

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
      do_compare_rtx_and_jump (target, const0_rtx, EQ, unsignedp, mode,
			       NULL_RTX, NULL, label,
			       profile_probability::uninitialized ());
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
  do_compare_rtx_and_jump (op0, op1, code, unsignedp, mode, NULL_RTX, NULL,
			   label, profile_probability::uninitialized ());

  emit_move_insn (target, falseval);
  emit_label (label);

  return target;
}

/* Helper function for canonicalize_cmp_for_target.  Swap between inclusive
   and exclusive ranges in order to create an equivalent comparison.  See
   canonicalize_cmp_for_target for the possible cases.  */

static enum rtx_code
equivalent_cmp_code (enum rtx_code code)
{
  switch (code)
    {
    case GT:
      return GE;
    case GE:
      return GT;
    case LT:
      return LE;
    case LE:
      return LT;
    case GTU:
      return GEU;
    case GEU:
      return GTU;
    case LTU:
      return LEU;
    case LEU:
      return LTU;

    default:
      return code;
    }
}

/* Choose the more appropiate immediate in scalar integer comparisons.  The
   purpose of this is to end up with an immediate which can be loaded into a
   register in fewer moves, if possible.

   For each integer comparison there exists an equivalent choice:
     i)   a >  b or a >= b + 1
     ii)  a <= b or a <  b + 1
     iii) a >= b or a >  b - 1
     iv)  a <  b or a <= b - 1

   MODE is the mode of the first operand.
   CODE points to the comparison code.
   IMM points to the rtx containing the immediate.  *IMM must satisfy
   CONST_SCALAR_INT_P on entry and continues to satisfy CONST_SCALAR_INT_P
   on exit.  */

void
canonicalize_comparison (machine_mode mode, enum rtx_code *code, rtx *imm)
{
  if (!SCALAR_INT_MODE_P (mode))
    return;

  int to_add = 0;
  enum signop sgn = unsigned_condition_p (*code) ? UNSIGNED : SIGNED;

  /* Extract the immediate value from the rtx.  */
  wide_int imm_val = rtx_mode_t (*imm, mode);

  if (*code == GT || *code == GTU || *code == LE || *code == LEU)
    to_add = 1;
  else if (*code == GE || *code == GEU || *code == LT || *code == LTU)
    to_add = -1;
  else
    return;

  /* Check for overflow/underflow in the case of signed values and
     wrapping around in the case of unsigned values.  If any occur
     cancel the optimization.  */
  wi::overflow_type overflow = wi::OVF_NONE;
  wide_int imm_modif;

  if (to_add == 1)
    imm_modif = wi::add (imm_val, 1, sgn, &overflow);
  else
    imm_modif = wi::sub (imm_val, 1, sgn, &overflow);

  if (overflow)
    return;

  /* The following creates a pseudo; if we cannot do that, bail out.  */
  if (!can_create_pseudo_p ())
    return;

  rtx reg = gen_rtx_REG (mode, LAST_VIRTUAL_REGISTER + 1);
  rtx new_imm = immed_wide_int_const (imm_modif, mode);

  rtx_insn *old_rtx = gen_move_insn (reg, *imm);
  rtx_insn *new_rtx = gen_move_insn (reg, new_imm);

  /* Update the immediate and the code.  */
  if (insn_cost (old_rtx, true) > insn_cost (new_rtx, true))
    {
      *code = equivalent_cmp_code (*code);
      *imm = new_imm;
    }
}



/* Perform possibly multi-word comparison and conditional jump to LABEL
   if ARG1 OP ARG2 true where ARG1 and ARG2 are of mode MODE.  This is
   now a thin wrapper around do_compare_rtx_and_jump.  */

static void
do_cmp_and_jump (rtx arg1, rtx arg2, enum rtx_code op, machine_mode mode,
		 rtx_code_label *label)
{
  int unsignedp = (op == LTU || op == LEU || op == GTU || op == GEU);
  do_compare_rtx_and_jump (arg1, arg2, op, unsignedp, mode, NULL_RTX,
			   NULL, label, profile_probability::uninitialized ());
}
