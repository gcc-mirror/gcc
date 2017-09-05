/* IR-agnostic target query functions relating to optabs
   Copyright (C) 1987-2017 Free Software Foundation, Inc.

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
#include "target.h"
#include "insn-codes.h"
#include "optabs-query.h"
#include "optabs-libfuncs.h"
#include "insn-config.h"
#include "rtl.h"
#include "recog.h"

struct target_optabs default_target_optabs;
struct target_optabs *this_fn_optabs = &default_target_optabs;
#if SWITCHABLE_TARGET
struct target_optabs *this_target_optabs = &default_target_optabs;
#endif

/* Return the insn used to perform conversion OP from mode FROM_MODE
   to mode TO_MODE; return CODE_FOR_nothing if the target does not have
   such an insn, or if it is unsuitable for optimization type OPT_TYPE.  */

insn_code
convert_optab_handler (convert_optab optab, machine_mode to_mode,
		       machine_mode from_mode, optimization_type opt_type)
{
  insn_code icode = convert_optab_handler (optab, to_mode, from_mode);
  if (icode == CODE_FOR_nothing
      || !targetm.optab_supported_p (optab, to_mode, from_mode, opt_type))
    return CODE_FOR_nothing;
  return icode;
}

/* Return the insn used to implement mode MODE of OP; return
   CODE_FOR_nothing if the target does not have such an insn,
   or if it is unsuitable for optimization type OPT_TYPE.  */

insn_code
direct_optab_handler (convert_optab optab, machine_mode mode,
		      optimization_type opt_type)
{
  insn_code icode = direct_optab_handler (optab, mode);
  if (icode == CODE_FOR_nothing
      || !targetm.optab_supported_p (optab, mode, mode, opt_type))
    return CODE_FOR_nothing;
  return icode;
}

/* Enumerates the possible types of structure operand to an
   extraction_insn.  */
enum extraction_type { ET_unaligned_mem, ET_reg };

/* Check whether insv, extv or extzv pattern ICODE can be used for an
   insertion or extraction of type TYPE on a structure of mode MODE.
   Return true if so and fill in *INSN accordingly.  STRUCT_OP is the
   operand number of the structure (the first sign_extract or zero_extract
   operand) and FIELD_OP is the operand number of the field (the other
   side of the set from the sign_extract or zero_extract).  */

static bool
get_traditional_extraction_insn (extraction_insn *insn,
				 enum extraction_type type,
				 machine_mode mode,
				 enum insn_code icode,
				 int struct_op, int field_op)
{
  const struct insn_data_d *data = &insn_data[icode];

  machine_mode struct_mode = data->operand[struct_op].mode;
  if (struct_mode == VOIDmode)
    struct_mode = word_mode;
  if (mode != struct_mode)
    return false;

  machine_mode field_mode = data->operand[field_op].mode;
  if (field_mode == VOIDmode)
    field_mode = word_mode;

  machine_mode pos_mode = data->operand[struct_op + 2].mode;
  if (pos_mode == VOIDmode)
    pos_mode = word_mode;

  insn->icode = icode;
  insn->field_mode = as_a <scalar_int_mode> (field_mode);
  if (type == ET_unaligned_mem)
    insn->struct_mode = byte_mode;
  else if (struct_mode == BLKmode)
    insn->struct_mode = opt_scalar_int_mode ();
  else
    insn->struct_mode = as_a <scalar_int_mode> (struct_mode);
  insn->pos_mode = as_a <scalar_int_mode> (pos_mode);
  return true;
}

/* Return true if an optab exists to perform an insertion or extraction
   of type TYPE in mode MODE.  Describe the instruction in *INSN if so.

   REG_OPTAB is the optab to use for register structures and
   MISALIGN_OPTAB is the optab to use for misaligned memory structures.
   POS_OP is the operand number of the bit position.  */

static bool
get_optab_extraction_insn (struct extraction_insn *insn,
			   enum extraction_type type,
			   machine_mode mode, direct_optab reg_optab,
			   direct_optab misalign_optab, int pos_op)
{
  direct_optab optab = (type == ET_unaligned_mem ? misalign_optab : reg_optab);
  enum insn_code icode = direct_optab_handler (optab, mode);
  if (icode == CODE_FOR_nothing)
    return false;

  const struct insn_data_d *data = &insn_data[icode];

  machine_mode pos_mode = data->operand[pos_op].mode;
  if (pos_mode == VOIDmode)
    pos_mode = word_mode;

  insn->icode = icode;
  insn->field_mode = as_a <scalar_int_mode> (mode);
  if (type == ET_unaligned_mem)
    insn->struct_mode = opt_scalar_int_mode ();
  else
    insn->struct_mode = insn->field_mode;
  insn->pos_mode = as_a <scalar_int_mode> (pos_mode);
  return true;
}

/* Return true if an instruction exists to perform an insertion or
   extraction (PATTERN says which) of type TYPE in mode MODE.
   Describe the instruction in *INSN if so.  */

static bool
get_extraction_insn (extraction_insn *insn,
		     enum extraction_pattern pattern,
		     enum extraction_type type,
		     machine_mode mode)
{
  switch (pattern)
    {
    case EP_insv:
      if (targetm.have_insv ()
	  && get_traditional_extraction_insn (insn, type, mode,
					      targetm.code_for_insv, 0, 3))
	return true;
      return get_optab_extraction_insn (insn, type, mode, insv_optab,
					insvmisalign_optab, 2);

    case EP_extv:
      if (targetm.have_extv ()
	  && get_traditional_extraction_insn (insn, type, mode,
					      targetm.code_for_extv, 1, 0))
	return true;
      return get_optab_extraction_insn (insn, type, mode, extv_optab,
					extvmisalign_optab, 3);

    case EP_extzv:
      if (targetm.have_extzv ()
	  && get_traditional_extraction_insn (insn, type, mode,
					      targetm.code_for_extzv, 1, 0))
	return true;
      return get_optab_extraction_insn (insn, type, mode, extzv_optab,
					extzvmisalign_optab, 3);

    default:
      gcc_unreachable ();
    }
}

/* Return true if an instruction exists to access a field of mode
   FIELDMODE in a structure that has STRUCT_BITS significant bits.
   Describe the "best" such instruction in *INSN if so.  PATTERN and
   TYPE describe the type of insertion or extraction we want to perform.

   For an insertion, the number of significant structure bits includes
   all bits of the target.  For an extraction, it need only include the
   most significant bit of the field.  Larger widths are acceptable
   in both cases.  */

static bool
get_best_extraction_insn (extraction_insn *insn,
			  enum extraction_pattern pattern,
			  enum extraction_type type,
			  unsigned HOST_WIDE_INT struct_bits,
			  machine_mode field_mode)
{
  opt_scalar_int_mode mode_iter;
  FOR_EACH_MODE_FROM (mode_iter, smallest_int_mode_for_size (struct_bits))
    {
      scalar_int_mode mode = mode_iter.require ();
      if (get_extraction_insn (insn, pattern, type, mode))
	{
	  FOR_EACH_MODE_FROM (mode_iter, mode)
	    {
	      mode = mode_iter.require ();
	      if (GET_MODE_SIZE (mode) > GET_MODE_SIZE (field_mode)
		  || TRULY_NOOP_TRUNCATION_MODES_P (insn->field_mode,
						    field_mode))
		break;
	      get_extraction_insn (insn, pattern, type, mode);
	    }
	  return true;
	}
    }
  return false;
}

/* Return true if an instruction exists to access a field of mode
   FIELDMODE in a register structure that has STRUCT_BITS significant bits.
   Describe the "best" such instruction in *INSN if so.  PATTERN describes
   the type of insertion or extraction we want to perform.

   For an insertion, the number of significant structure bits includes
   all bits of the target.  For an extraction, it need only include the
   most significant bit of the field.  Larger widths are acceptable
   in both cases.  */

bool
get_best_reg_extraction_insn (extraction_insn *insn,
			      enum extraction_pattern pattern,
			      unsigned HOST_WIDE_INT struct_bits,
			      machine_mode field_mode)
{
  return get_best_extraction_insn (insn, pattern, ET_reg, struct_bits,
				   field_mode);
}

/* Return true if an instruction exists to access a field of BITSIZE
   bits starting BITNUM bits into a memory structure.  Describe the
   "best" such instruction in *INSN if so.  PATTERN describes the type
   of insertion or extraction we want to perform and FIELDMODE is the
   natural mode of the extracted field.

   The instructions considered here only access bytes that overlap
   the bitfield; they do not touch any surrounding bytes.  */

bool
get_best_mem_extraction_insn (extraction_insn *insn,
			      enum extraction_pattern pattern,
			      HOST_WIDE_INT bitsize, HOST_WIDE_INT bitnum,
			      machine_mode field_mode)
{
  unsigned HOST_WIDE_INT struct_bits = (bitnum % BITS_PER_UNIT
					+ bitsize
					+ BITS_PER_UNIT - 1);
  struct_bits -= struct_bits % BITS_PER_UNIT;
  return get_best_extraction_insn (insn, pattern, ET_unaligned_mem,
				   struct_bits, field_mode);
}

/* Return the insn code used to extend FROM_MODE to TO_MODE.
   UNSIGNEDP specifies zero-extension instead of sign-extension.  If
   no such operation exists, CODE_FOR_nothing will be returned.  */

enum insn_code
can_extend_p (machine_mode to_mode, machine_mode from_mode,
	      int unsignedp)
{
  if (unsignedp < 0 && targetm.have_ptr_extend ())
    return targetm.code_for_ptr_extend;

  convert_optab tab = unsignedp ? zext_optab : sext_optab;
  return convert_optab_handler (tab, to_mode, from_mode);
}

/* Return the insn code to convert fixed-point mode FIXMODE to floating-point
   mode FLTMODE, or CODE_FOR_nothing if no such instruction exists.
   UNSIGNEDP specifies whether FIXMODE is unsigned.  */

enum insn_code
can_float_p (machine_mode fltmode, machine_mode fixmode,
	     int unsignedp)
{
  convert_optab tab = unsignedp ? ufloat_optab : sfloat_optab;
  return convert_optab_handler (tab, fltmode, fixmode);
}

/* Return the insn code to convert floating-point mode FLTMODE to fixed-point
   mode FIXMODE, or CODE_FOR_nothing if no such instruction exists.
   UNSIGNEDP specifies whether FIXMODE is unsigned.

   On a successful return, set *TRUNCP_PTR to true if it is necessary to
   output an explicit FTRUNC before the instruction.  */

enum insn_code
can_fix_p (machine_mode fixmode, machine_mode fltmode,
	   int unsignedp, bool *truncp_ptr)
{
  convert_optab tab;
  enum insn_code icode;

  tab = unsignedp ? ufixtrunc_optab : sfixtrunc_optab;
  icode = convert_optab_handler (tab, fixmode, fltmode);
  if (icode != CODE_FOR_nothing)
    {
      *truncp_ptr = false;
      return icode;
    }

  /* FIXME: This requires a port to define both FIX and FTRUNC pattern
     for this to work.  We need to rework the fix* and ftrunc* patterns
     and documentation.  */
  tab = unsignedp ? ufix_optab : sfix_optab;
  icode = convert_optab_handler (tab, fixmode, fltmode);
  if (icode != CODE_FOR_nothing
      && optab_handler (ftrunc_optab, fltmode) != CODE_FOR_nothing)
    {
      *truncp_ptr = true;
      return icode;
    }

  return CODE_FOR_nothing;
}

/* Return nonzero if a conditional move of mode MODE is supported.

   This function is for combine so it can tell whether an insn that looks
   like a conditional move is actually supported by the hardware.  If we
   guess wrong we lose a bit on optimization, but that's it.  */
/* ??? sparc64 supports conditionally moving integers values based on fp
   comparisons, and vice versa.  How do we handle them?  */

bool
can_conditionally_move_p (machine_mode mode)
{
  return direct_optab_handler (movcc_optab, mode) != CODE_FOR_nothing;
}

/* Return true if VEC_PERM_EXPR of arbitrary input vectors can be
   expanded using SIMD extensions of the CPU.  SEL may be NULL, which
   stands for an unknown constant.  Note that additional permutations
   representing whole-vector shifts may also be handled via the vec_shr
   optab, but only where the second input vector is entirely constant
   zeroes; this case is not dealt with here.  */

bool
can_vec_perm_p (machine_mode mode, bool variable,
		const unsigned char *sel)
{
  machine_mode qimode;

  /* If the target doesn't implement a vector mode for the vector type,
     then no operations are supported.  */
  if (!VECTOR_MODE_P (mode))
    return false;

  if (!variable)
    {
      if (direct_optab_handler (vec_perm_const_optab, mode) != CODE_FOR_nothing
	  && (sel == NULL
	      || targetm.vectorize.vec_perm_const_ok == NULL
	      || targetm.vectorize.vec_perm_const_ok (mode, sel)))
	return true;
    }

  if (direct_optab_handler (vec_perm_optab, mode) != CODE_FOR_nothing)
    return true;

  /* We allow fallback to a QI vector mode, and adjust the mask.  */
  if (GET_MODE_INNER (mode) == QImode
      || !mode_for_vector (QImode, GET_MODE_SIZE (mode)).exists (&qimode)
      || !VECTOR_MODE_P (qimode))
    return false;

  /* ??? For completeness, we ought to check the QImode version of
      vec_perm_const_optab.  But all users of this implicit lowering
      feature implement the variable vec_perm_optab.  */
  if (direct_optab_handler (vec_perm_optab, qimode) == CODE_FOR_nothing)
    return false;

  /* In order to support the lowering of variable permutations,
     we need to support shifts and adds.  */
  if (variable)
    {
      if (GET_MODE_UNIT_SIZE (mode) > 2
	  && optab_handler (ashl_optab, mode) == CODE_FOR_nothing
	  && optab_handler (vashl_optab, mode) == CODE_FOR_nothing)
	return false;
      if (optab_handler (add_optab, qimode) == CODE_FOR_nothing)
	return false;
    }

  return true;
}

/* Like optab_handler, but for widening_operations that have a
   TO_MODE and a FROM_MODE.  */

enum insn_code
widening_optab_handler (optab op, machine_mode to_mode,
			machine_mode from_mode)
{
  unsigned scode = (op << 16) | to_mode;
  if (to_mode != from_mode && from_mode != VOIDmode)
    {
      /* ??? Why does find_widening_optab_handler_and_mode attempt to
	 widen things that can't be widened?  E.g. add_optab... */
      if (op > LAST_CONV_OPTAB)
	return CODE_FOR_nothing;
      scode |= from_mode << 8;
    }
  return raw_optab_handler (scode);
}

/* Find a widening optab even if it doesn't widen as much as we want.
   E.g. if from_mode is HImode, and to_mode is DImode, and there is no
   direct HI->SI insn, then return SI->DI, if that exists.
   If PERMIT_NON_WIDENING is non-zero then this can be used with
   non-widening optabs also.  */

enum insn_code
find_widening_optab_handler_and_mode (optab op, machine_mode to_mode,
				      machine_mode from_mode,
				      int permit_non_widening,
				      machine_mode *found_mode)
{
  for (; (permit_non_widening || from_mode != to_mode)
	 && GET_MODE_SIZE (from_mode) <= GET_MODE_SIZE (to_mode)
	 && from_mode != VOIDmode;
       from_mode = GET_MODE_WIDER_MODE (from_mode).else_void ())
    {
      enum insn_code handler = widening_optab_handler (op, to_mode,
						       from_mode);

      if (handler != CODE_FOR_nothing)
	{
	  if (found_mode)
	    *found_mode = from_mode;
	  return handler;
	}
    }

  return CODE_FOR_nothing;
}

/* Return non-zero if a highpart multiply is supported of can be synthisized.
   For the benefit of expand_mult_highpart, the return value is 1 for direct,
   2 for even/odd widening, and 3 for hi/lo widening.  */

int
can_mult_highpart_p (machine_mode mode, bool uns_p)
{
  optab op;
  unsigned char *sel;
  unsigned i, nunits;

  op = uns_p ? umul_highpart_optab : smul_highpart_optab;
  if (optab_handler (op, mode) != CODE_FOR_nothing)
    return 1;

  /* If the mode is an integral vector, synth from widening operations.  */
  if (GET_MODE_CLASS (mode) != MODE_VECTOR_INT)
    return 0;

  nunits = GET_MODE_NUNITS (mode);
  sel = XALLOCAVEC (unsigned char, nunits);

  op = uns_p ? vec_widen_umult_even_optab : vec_widen_smult_even_optab;
  if (optab_handler (op, mode) != CODE_FOR_nothing)
    {
      op = uns_p ? vec_widen_umult_odd_optab : vec_widen_smult_odd_optab;
      if (optab_handler (op, mode) != CODE_FOR_nothing)
	{
	  for (i = 0; i < nunits; ++i)
	    sel[i] = !BYTES_BIG_ENDIAN + (i & ~1) + ((i & 1) ? nunits : 0);
	  if (can_vec_perm_p (mode, false, sel))
	    return 2;
	}
    }

  op = uns_p ? vec_widen_umult_hi_optab : vec_widen_smult_hi_optab;
  if (optab_handler (op, mode) != CODE_FOR_nothing)
    {
      op = uns_p ? vec_widen_umult_lo_optab : vec_widen_smult_lo_optab;
      if (optab_handler (op, mode) != CODE_FOR_nothing)
	{
	  for (i = 0; i < nunits; ++i)
	    sel[i] = 2 * i + (BYTES_BIG_ENDIAN ? 0 : 1);
	  if (can_vec_perm_p (mode, false, sel))
	    return 3;
	}
    }

  return 0;
}

/* Return true if target supports vector masked load/store for mode.  */

bool
can_vec_mask_load_store_p (machine_mode mode,
			   machine_mode mask_mode,
			   bool is_load)
{
  optab op = is_load ? maskload_optab : maskstore_optab;
  machine_mode vmode;
  unsigned int vector_sizes;

  /* If mode is vector mode, check it directly.  */
  if (VECTOR_MODE_P (mode))
    return convert_optab_handler (op, mode, mask_mode) != CODE_FOR_nothing;

  /* Otherwise, return true if there is some vector mode with
     the mask load/store supported.  */

  /* See if there is any chance the mask load or store might be
     vectorized.  If not, punt.  */
  scalar_mode smode;
  if (!is_a <scalar_mode> (mode, &smode))
    return false;

  vmode = targetm.vectorize.preferred_simd_mode (smode);
  if (!VECTOR_MODE_P (vmode))
    return false;

  if ((targetm.vectorize.get_mask_mode
       (GET_MODE_NUNITS (vmode), GET_MODE_SIZE (vmode)).exists (&mask_mode))
      && convert_optab_handler (op, vmode, mask_mode) != CODE_FOR_nothing)
    return true;

  vector_sizes = targetm.vectorize.autovectorize_vector_sizes ();
  while (vector_sizes != 0)
    {
      unsigned int cur = 1 << floor_log2 (vector_sizes);
      vector_sizes &= ~cur;
      if (cur <= GET_MODE_SIZE (smode))
	continue;
      unsigned int nunits = cur / GET_MODE_SIZE (smode);
      if (mode_for_vector (smode, nunits).exists (&vmode)
	  && VECTOR_MODE_P (vmode)
	  && targetm.vectorize.get_mask_mode (nunits, cur).exists (&mask_mode)
	  && convert_optab_handler (op, vmode, mask_mode) != CODE_FOR_nothing)
	return true;
    }
  return false;
}

/* Return true if there is a compare_and_swap pattern.  */

bool
can_compare_and_swap_p (machine_mode mode, bool allow_libcall)
{
  enum insn_code icode;

  /* Check for __atomic_compare_and_swap.  */
  icode = direct_optab_handler (atomic_compare_and_swap_optab, mode);
  if (icode != CODE_FOR_nothing)
    return true;

  /* Check for __sync_compare_and_swap.  */
  icode = optab_handler (sync_compare_and_swap_optab, mode);
  if (icode != CODE_FOR_nothing)
    return true;
  if (allow_libcall && optab_libfunc (sync_compare_and_swap_optab, mode))
    return true;

  /* No inline compare and swap.  */
  return false;
}

/* Return true if an atomic exchange can be performed.  */

bool
can_atomic_exchange_p (machine_mode mode, bool allow_libcall)
{
  enum insn_code icode;

  /* Check for __atomic_exchange.  */
  icode = direct_optab_handler (atomic_exchange_optab, mode);
  if (icode != CODE_FOR_nothing)
    return true;

  /* Don't check __sync_test_and_set, as on some platforms that
     has reduced functionality.  Targets that really do support
     a proper exchange should simply be updated to the __atomics.  */

  return can_compare_and_swap_p (mode, allow_libcall);
}

/* Return true if an atomic load can be performed without falling back to
   a compare-and-swap.  */

bool
can_atomic_load_p (machine_mode mode)
{
  enum insn_code icode;

  /* Does the target supports the load directly?  */
  icode = direct_optab_handler (atomic_load_optab, mode);
  if (icode != CODE_FOR_nothing)
    return true;

  /* If the size of the object is greater than word size on this target,
     then we assume that a load will not be atomic.  Also see
     expand_atomic_load.  */
  return GET_MODE_PRECISION (mode) <= BITS_PER_WORD;
}

/* Determine whether "1 << x" is relatively cheap in word_mode.  */

bool
lshift_cheap_p (bool speed_p)
{
  /* FIXME: This should be made target dependent via this "this_target"
     mechanism, similar to e.g. can_copy_init_p in gcse.c.  */
  static bool init[2] = { false, false };
  static bool cheap[2] = { true, true };

  /* If the targer has no lshift in word_mode, the operation will most
     probably not be cheap.  ??? Does GCC even work for such targets?  */
  if (optab_handler (ashl_optab, word_mode) == CODE_FOR_nothing)
    return false;

  if (!init[speed_p])
    {
      rtx reg = gen_raw_REG (word_mode, 10000);
      int cost = set_src_cost (gen_rtx_ASHIFT (word_mode, const1_rtx, reg),
			       word_mode, speed_p);
      cheap[speed_p] = cost < COSTS_N_INSNS (3);
      init[speed_p] = true;
    }

  return cheap[speed_p];
}
