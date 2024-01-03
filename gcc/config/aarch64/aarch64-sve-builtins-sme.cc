/* ACLE support for AArch64 SME.
   Copyright (C) 2023-2024 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "rtl.h"
#include "tm_p.h"
#include "memmodel.h"
#include "insn-codes.h"
#include "optabs.h"
#include "recog.h"
#include "expr.h"
#include "basic-block.h"
#include "function.h"
#include "fold-const.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimplify.h"
#include "explow.h"
#include "emit-rtl.h"
#include "aarch64-sve-builtins.h"
#include "aarch64-sve-builtins-shapes.h"
#include "aarch64-sve-builtins-base.h"
#include "aarch64-sve-builtins-sme.h"
#include "aarch64-sve-builtins-functions.h"

using namespace aarch64_sve;

namespace {

class load_store_za_zt0_base : public function_base
{
public:
  tree
  memory_scalar_type (const function_instance &) const override
  {
    return void_type_node;
  }
};

class read_write_za_base : public function_base
{
public:
  constexpr read_write_za_base (int unspec) : m_unspec (unspec) {}

  rtx
  expand (function_expander &e) const override
  {
    auto za_mode = e.vector_mode (0);
    auto z_mode = e.tuple_mode (1);
    auto icode = (za_mode == VNx1TImode
		  ? code_for_aarch64_sme (m_unspec, za_mode, z_mode)
		  : code_for_aarch64_sme (m_unspec, z_mode, z_mode));
    return e.use_exact_insn (icode);
  }

  int m_unspec;
};

using load_za_base = add_call_properties<load_store_za_zt0_base,
					 CP_READ_MEMORY | CP_READ_ZA
					 | CP_WRITE_ZA>;

using store_za_base = add_call_properties<load_store_za_zt0_base,
					  CP_WRITE_MEMORY | CP_READ_ZA>;

/* E is a load or store intrinsic that accesses a ZA slice of mode MEM_MODE.
   The intrinsic has a vnum parameter at index ARGNO.  Return true if the
   vnum argument is a constant that is a valid ZA offset for the underlying
   instruction.  */

static bool
has_in_range_vnum_arg (function_expander &e, machine_mode mem_mode,
		       unsigned int argno)
{
  return (e.mode_suffix_id == MODE_vnum
	  && CONST_INT_P (e.args[argno])
	  && UINTVAL (e.args[argno]) < 16 / GET_MODE_UNIT_SIZE (mem_mode));
}

/* E is a ZA load or store intrinsic that uses instruction ICODE.  Add a
   32-bit operand that gives the total ZA slice.  (The instruction hard-codes
   the constant offset to 0, so there is no operand for that.)

   Argument ARGNO is the intrinsic's slice argument.  If the intrinsic is
   a _vnum intrinsic, argument VNUM_ARGNO is the intrinsic's vnum operand,
   which must be added to the slice argument.  */

static void
add_load_store_slice_operand (function_expander &e, insn_code icode,
			      unsigned int argno, unsigned int vnum_argno)
{
  rtx base = e.args[argno];
  if (e.mode_suffix_id == MODE_vnum)
    {
      rtx vnum = lowpart_subreg (SImode, e.args[vnum_argno], DImode);
      base = simplify_gen_binary (PLUS, SImode, base, vnum);
    }
  e.add_input_operand (icode, base);
}

/* Add a memory operand for ZA LD1 or ST1 intrinsic E.  BASE_ARGNO is
   the index of the base argument.  */

static void
add_load_store_operand (function_expander &e, unsigned int base_argno)
{
  auto mode = e.vector_mode (0);
  rtx base = e.get_contiguous_base (mode, base_argno, base_argno + 1,
				    AARCH64_FL_SM_ON);
  auto mem = gen_rtx_MEM (mode, force_reg (Pmode, base));
  set_mem_align (mem, BITS_PER_UNIT);
  e.add_fixed_operand (mem);
}

/* Expand ZA LDR or STR intrinsic E.  There are two underlying instructions:

   - BASE_CODE has a zero ZA slice offset
   - VNUM_CODE has a constant operand for the ZA slice offset.  */

static rtx
expand_ldr_str_za (function_expander &e, insn_code base_code,
		   insn_code vnum_code)
{
  if (has_in_range_vnum_arg (e, VNx16QImode, 2))
    {
      rtx mem_offset = aarch64_sme_vq_immediate (Pmode,
						 UINTVAL (e.args[2]) * 16,
						 AARCH64_ISA_MODE);
      e.add_input_operand (vnum_code, e.args[0]);
      e.add_input_operand (vnum_code, e.args[2]);
      e.add_input_operand (vnum_code, e.args[1]);
      e.add_input_operand (vnum_code, mem_offset);
      return e.generate_insn (vnum_code);
    }
  else
    {
      rtx base = e.get_contiguous_base (VNx16QImode, 1, 2, AARCH64_FL_SM_ON);
      add_load_store_slice_operand (e, base_code, 0, 2);
      e.add_input_operand (base_code, base);
      return e.generate_insn (base_code);
    }
}

/* Use instruction ICODE to expand ZT0 load or store E.  */

static rtx
expand_ldr_str_zt0 (function_expander &e, insn_code icode)
{
  rtx base = e.convert_to_pmode (e.args[1]);
  rtx mem = gen_rtx_MEM (V8DImode, force_reg (Pmode, base));
  e.add_fixed_operand (mem);
  return e.generate_insn (icode);
}

/* Expand ZA LD1 or ST1 intrinsic E.  UNSPEC is the load or store unspec.
   IS_LOAD is true if E is a load, false if it is a store.  */

static rtx
expand_ld1_st1 (function_expander &e, int unspec, bool is_load)
{
  bool is_vnum = has_in_range_vnum_arg (e, e.vector_mode (0), 4);
  auto icode = (is_vnum
		? code_for_aarch64_sme_plus (unspec, e.vector_mode (0))
		: code_for_aarch64_sme (unspec, e.vector_mode (0)));
  if (!is_load)
    add_load_store_operand (e, 3);
  e.add_input_operand (icode, e.args[0]);
  if (is_vnum)
    {
      e.add_input_operand (icode, e.args[1]);
      e.add_input_operand (icode, e.args[4]);
    }
  else
    add_load_store_slice_operand (e, icode, 1, 4);
  e.add_input_operand (icode, e.args[2]);
  if (is_load)
    add_load_store_operand (e, 3);
  return e.generate_insn (icode);
}

class arm_has_sme_impl : public function_base
{
  gimple *
  fold (gimple_folder &f) const override
  {
    if (TARGET_SME)
      return f.fold_to_cstu (1);
    return nullptr;
  }

  rtx
  expand (function_expander &e) const override
  {
    if (TARGET_SME)
      return const1_rtx;
    emit_insn (gen_aarch64_get_sme_state ());
    return expand_simple_binop (DImode, LSHIFTRT,
				gen_rtx_REG (DImode, R0_REGNUM),
				gen_int_mode (63, QImode),
				e.possible_target, true, OPTAB_LIB_WIDEN);
  }
};

class arm_in_streaming_mode_impl : public function_base
{
  gimple *
  fold (gimple_folder &f) const override
  {
    if (TARGET_STREAMING)
      return f.fold_to_cstu (1);
    if (TARGET_NON_STREAMING)
      return f.fold_to_cstu (0);
    return nullptr;
  }

  rtx
  expand (function_expander &e) const override
  {
    if (TARGET_STREAMING)
      return const1_rtx;

    if (TARGET_NON_STREAMING)
      return const0_rtx;

    rtx reg;
    if (TARGET_SME)
      {
	reg = gen_reg_rtx (DImode);
	emit_insn (gen_aarch64_read_svcr (reg));
      }
    else
      {
	emit_insn (gen_aarch64_get_sme_state ());
	reg = gen_rtx_REG (DImode, R0_REGNUM);
      }
    return expand_simple_binop (DImode, AND, reg, gen_int_mode (1, DImode),
				e.possible_target, true, OPTAB_LIB_WIDEN);
  }
};

/* Implements svcnts[bhwd].  */
class svcnts_bhwd_impl : public function_base
{
public:
  constexpr svcnts_bhwd_impl (machine_mode ref_mode) : m_ref_mode (ref_mode) {}

  unsigned int
  get_shift () const
  {
    return exact_log2 (GET_MODE_UNIT_SIZE (m_ref_mode));
  }

  gimple *
  fold (gimple_folder &f) const override
  {
    if (TARGET_STREAMING)
      return f.fold_to_cstu (GET_MODE_NUNITS (m_ref_mode));
    return nullptr;
  }

  rtx
  expand (function_expander &e) const override
  {
    rtx cntsb = aarch64_sme_vq_immediate (DImode, 16, AARCH64_ISA_MODE);
    auto shift = get_shift ();
    if (!shift)
      return cntsb;

    return expand_simple_binop (DImode, LSHIFTRT, cntsb,
				gen_int_mode (shift, QImode),
				e.possible_target, true, OPTAB_LIB_WIDEN);
  }

  /* The mode of the vector associated with the [bhwd] suffix.  */
  machine_mode m_ref_mode;
};

class svld1_za_impl : public load_za_base
{
public:
  constexpr svld1_za_impl (int unspec) : m_unspec (unspec) {}

  rtx
  expand (function_expander &e) const override
  {
    return expand_ld1_st1 (e, m_unspec, true);
  }

  int m_unspec;
};

class svldr_za_impl : public load_za_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    return expand_ldr_str_za (e, CODE_FOR_aarch64_sme_ldr0,
			      code_for_aarch64_sme_ldrn (Pmode));
  }
};

class svldr_zt_impl : public load_store_za_zt0_base
{
public:
  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_READ_MEMORY | CP_WRITE_ZT0;
  }

  rtx
  expand (function_expander &e) const override
  {
    return expand_ldr_str_zt0 (e, CODE_FOR_aarch64_sme_ldr_zt0);
  }
};

class svluti_lane_zt_impl : public read_zt0<function_base>
{
public:
  CONSTEXPR svluti_lane_zt_impl (unsigned int bits) : m_bits (bits) {}

  rtx
  expand (function_expander &e) const override
  {
    auto mode = e.tuple_mode (0);
    e.args.ordered_remove (0);
    return e.use_exact_insn (code_for_aarch64_sme_lut (m_bits, mode));
  }

  unsigned int m_bits;
};

class svread_za_impl : public function_base
{
public:
  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_READ_ZA;
  }

  rtx
  expand (function_expander &e) const override
  {
    machine_mode mode = e.vectors_per_tuple () == 4 ? VNx8DImode : VNx4DImode;
    rtx res = e.use_exact_insn (code_for_aarch64_sme_read (mode));
    return aarch64_sve_reinterpret (e.result_mode (), res);
  }
};

using svread_za_tile_impl = add_call_properties<read_write_za_base,
						CP_READ_ZA>;

class svst1_za_impl : public store_za_base
{
public:
  constexpr svst1_za_impl (int unspec) : m_unspec (unspec) {}

  rtx
  expand (function_expander &e) const override
  {
    return expand_ld1_st1 (e, m_unspec, false);
  }

  int m_unspec;
};

class svstr_za_impl : public store_za_base
{
public:
  rtx
  expand (function_expander &e) const override
  {
    return expand_ldr_str_za (e, CODE_FOR_aarch64_sme_str0,
			      code_for_aarch64_sme_strn (Pmode));
  }
};

class svstr_zt_impl : public load_store_za_zt0_base
{
public:
  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_WRITE_MEMORY | CP_READ_ZT0;
  }

  rtx
  expand (function_expander &e) const override
  {
    return expand_ldr_str_zt0 (e, CODE_FOR_aarch64_sme_str_zt0);
  }
};

class svsudot_za_impl : public read_write_za<function_base>
{
public:
  rtx
  expand (function_expander &e) const override
  {
    if (e.mode_suffix_id == MODE_single)
      {
	auto icode = code_for_aarch64_sme_single_sudot (e.vector_mode (0),
							e.tuple_mode (1));
	return e.use_exact_insn (icode);
      }
    std::swap (e.args[1], e.args[2]);
    return e.use_exact_insn (code_for_aarch64_sme (UNSPEC_SME_USDOT,
						   e.vector_mode (0),
						   e.tuple_mode (1)));
  }
};

class svundef_za_impl : public write_za<function_base>
{
public:
  rtx
  expand (function_expander &) const override
  {
    rtx target = gen_rtx_REG (VNx16QImode, ZA_REGNUM);
    emit_clobber (copy_rtx (target));
    return const0_rtx;
  }
};

class svwrite_za_impl : public function_base
{
public:
  unsigned int
  call_properties (const function_instance &) const override
  {
    return CP_WRITE_ZA;
  }

  rtx
  expand (function_expander &e) const override
  {
    machine_mode mode = e.vectors_per_tuple () == 4 ? VNx8DImode : VNx4DImode;
    e.args[1] = aarch64_sve_reinterpret (mode, e.args[1]);
    return e.use_exact_insn (code_for_aarch64_sme_write (mode));
  }
};

using svwrite_za_tile_impl = add_call_properties<read_write_za_base,
						 CP_READ_ZA | CP_WRITE_ZA>;

class svzero_mask_za_impl : public write_za<function_base>
{
public:
  rtx
  expand (function_expander &e) const override
  {
    return e.use_exact_insn (CODE_FOR_aarch64_sme_zero_za);
  }
};

class svzero_za_impl : public write_za<function_base>
{
public:
  rtx
  expand (function_expander &) const override
  {
    emit_insn (gen_aarch64_sme_zero_za (gen_int_mode (0xff, SImode)));
    return const0_rtx;
  }
};

class svzero_zt_impl : public write_zt0<function_base>
{
public:
  rtx
  expand (function_expander &) const override
  {
    emit_insn (gen_aarch64_sme_zero_zt0 ());
    return const0_rtx;
  }
};

} /* end anonymous namespace */

namespace aarch64_sve {

FUNCTION (arm_has_sme, arm_has_sme_impl, )
FUNCTION (arm_in_streaming_mode, arm_in_streaming_mode_impl, )
FUNCTION (svadd_za, sme_1mode_function, (UNSPEC_SME_ADD, UNSPEC_SME_ADD,
					 UNSPEC_SME_FADD))
FUNCTION (svadd_write_za, sme_1mode_function, (UNSPEC_SME_ADD_WRITE,
					       UNSPEC_SME_ADD_WRITE, -1))
FUNCTION (svaddha_za, sme_1mode_function, (UNSPEC_SME_ADDHA,
					   UNSPEC_SME_ADDHA, -1))
FUNCTION (svaddva_za, sme_1mode_function, (UNSPEC_SME_ADDVA,
					  UNSPEC_SME_ADDVA, -1))
FUNCTION (svbmopa_za, sme_2mode_function, (-1, UNSPEC_SME_BMOPA, -1))
FUNCTION (svbmops_za, sme_2mode_function, (-1, UNSPEC_SME_BMOPS, -1))
FUNCTION (svcntsb, svcnts_bhwd_impl, (VNx16QImode))
FUNCTION (svcntsd, svcnts_bhwd_impl, (VNx2DImode))
FUNCTION (svcntsh, svcnts_bhwd_impl, (VNx8HImode))
FUNCTION (svcntsw, svcnts_bhwd_impl, (VNx4SImode))
FUNCTION (svdot_za, sme_2mode_function, (UNSPEC_SME_SDOT, UNSPEC_SME_UDOT,
					 UNSPEC_SME_FDOT))
FUNCTION (svdot_lane_za, sme_2mode_lane_function, (UNSPEC_SME_SDOT,
						   UNSPEC_SME_UDOT,
						   UNSPEC_SME_FDOT))
FUNCTION (svld1_hor_za, svld1_za_impl, (UNSPEC_SME_LD1_HOR))
FUNCTION (svld1_ver_za, svld1_za_impl, (UNSPEC_SME_LD1_VER))
FUNCTION (svldr_za, svldr_za_impl, )
FUNCTION (svldr_zt, svldr_zt_impl, )
FUNCTION (svluti2_lane_zt, svluti_lane_zt_impl, (2))
FUNCTION (svluti4_lane_zt, svluti_lane_zt_impl, (4))
FUNCTION (svmla_za, sme_2mode_function, (UNSPEC_SME_SMLA, UNSPEC_SME_UMLA,
					 UNSPEC_SME_FMLA))
FUNCTION (svmla_lane_za, sme_2mode_lane_function, (UNSPEC_SME_SMLA,
						   UNSPEC_SME_UMLA,
						   UNSPEC_SME_FMLA))
FUNCTION (svmls_za, sme_2mode_function, (UNSPEC_SME_SMLS, UNSPEC_SME_UMLS,
					 UNSPEC_SME_FMLS))
FUNCTION (svmls_lane_za, sme_2mode_lane_function, (UNSPEC_SME_SMLS,
						   UNSPEC_SME_UMLS,
						   UNSPEC_SME_FMLS))
FUNCTION (svmopa_za, sme_2mode_function, (UNSPEC_SME_SMOPA, UNSPEC_SME_UMOPA,
					  UNSPEC_SME_FMOPA))
FUNCTION (svmops_za, sme_2mode_function, (UNSPEC_SME_SMOPS, UNSPEC_SME_UMOPS,
					  UNSPEC_SME_FMOPS))
FUNCTION (svread_za, svread_za_impl,)
FUNCTION (svread_hor_za, svread_za_tile_impl, (UNSPEC_SME_READ_HOR))
FUNCTION (svread_ver_za, svread_za_tile_impl, (UNSPEC_SME_READ_VER))
FUNCTION (svst1_hor_za, svst1_za_impl, (UNSPEC_SME_ST1_HOR))
FUNCTION (svst1_ver_za, svst1_za_impl, (UNSPEC_SME_ST1_VER))
FUNCTION (svstr_za, svstr_za_impl, )
FUNCTION (svstr_zt, svstr_zt_impl, )
FUNCTION (svsub_za, sme_1mode_function, (UNSPEC_SME_SUB, UNSPEC_SME_SUB,
					 UNSPEC_SME_FSUB))
FUNCTION (svsub_write_za, sme_1mode_function, (UNSPEC_SME_SUB_WRITE,
					       UNSPEC_SME_SUB_WRITE, -1))
FUNCTION (svsudot_za, svsudot_za_impl,)
FUNCTION (svsudot_lane_za, sme_2mode_lane_function, (UNSPEC_SME_SUDOT, -1, -1))
FUNCTION (svsuvdot_lane_za, sme_2mode_lane_function, (UNSPEC_SME_SUVDOT,
						      -1, -1))
FUNCTION (svsumopa_za, sme_2mode_function, (UNSPEC_SME_SUMOPA, -1, -1))
FUNCTION (svsumops_za, sme_2mode_function, (UNSPEC_SME_SUMOPS, -1, -1))
FUNCTION (svundef_za, svundef_za_impl, )
FUNCTION (svusdot_za, sme_2mode_function, (-1, UNSPEC_SME_USDOT, -1))
FUNCTION (svusdot_lane_za, sme_2mode_lane_function, (-1, UNSPEC_SME_USDOT, -1))
FUNCTION (svusvdot_lane_za, sme_2mode_lane_function, (-1, UNSPEC_SME_USVDOT,
						      -1))
FUNCTION (svusmopa_za, sme_2mode_function, (-1, UNSPEC_SME_USMOPA, -1))
FUNCTION (svusmops_za, sme_2mode_function, (-1, UNSPEC_SME_USMOPS, -1))
FUNCTION (svvdot_lane_za, sme_2mode_lane_function, (UNSPEC_SME_SVDOT,
						    UNSPEC_SME_UVDOT,
						    UNSPEC_SME_FVDOT))
FUNCTION (svwrite_za, svwrite_za_impl,)
FUNCTION (svwrite_hor_za, svwrite_za_tile_impl, (UNSPEC_SME_WRITE_HOR))
FUNCTION (svwrite_ver_za, svwrite_za_tile_impl, (UNSPEC_SME_WRITE_VER))
FUNCTION (svzero_mask_za, svzero_mask_za_impl, )
FUNCTION (svzero_za, svzero_za_impl, )
FUNCTION (svzero_zt, svzero_zt_impl, )

} /* end namespace aarch64_sve */
