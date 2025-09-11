/* This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#define INCLUDE_STRING
#define INCLUDE_MAP
#define INCLUDE_VECTOR
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "stringpool.h"
#include "function.h"
#include "memmodel.h"
#include "emit-rtl.h"
#include "tm_p.h"
#include "expr.h"
#include "selftest.h"
#include "selftest-rtl.h"
#include "insn-attr.h"
#include "target.h"
#include "optabs.h"

#if CHECKING_P
using namespace selftest;
class riscv_selftest_arch_abi_setter
{
private:
  std::string m_arch_backup;
  enum riscv_abi_type m_abi_backup;

public:
  riscv_selftest_arch_abi_setter (const char *arch, enum riscv_abi_type abi)
    : m_arch_backup (riscv_arch_str ()), m_abi_backup (riscv_abi)
  {
    riscv_parse_arch_string (arch, &global_options, UNKNOWN_LOCATION);
    riscv_abi = abi;
    riscv_reinit ();
  }
  ~riscv_selftest_arch_abi_setter ()
  {
    riscv_parse_arch_string (m_arch_backup.c_str (), &global_options,
			     UNKNOWN_LOCATION);
    riscv_abi = m_abi_backup;
    riscv_reinit ();
  }
};

static poly_int64
eval_value (rtx x, std::map<unsigned, rtx> &regno_to_rtx)
{
  if (!REG_P (x))
    {
      debug (x);
      gcc_unreachable ();
    }

  rtx expr = NULL_RTX;
  unsigned regno = REGNO (x);
  expr = regno_to_rtx[regno];

  poly_int64 op1_val = 0;
  poly_int64 op2_val = 0;
  if (UNARY_P (expr))
    {
      op1_val = eval_value (XEXP (expr, 0), regno_to_rtx);
    }
  if (BINARY_P (expr))
    {
      op1_val = eval_value (XEXP (expr, 0), regno_to_rtx);
      op2_val = eval_value (XEXP (expr, 1), regno_to_rtx);
    }

  switch (GET_CODE (expr))
    {
    case CONST_POLY_INT:
      return rtx_to_poly_int64 (expr);
    case CONST_INT:
      return INTVAL (expr);

    case MULT:
      if (op1_val.is_constant ())
	return op1_val.to_constant () * op2_val;
      else if (op2_val.is_constant ())
	return op1_val * op2_val.to_constant ();
      else
	gcc_unreachable ();
    case PLUS:
      return op1_val + op2_val;
    default:
      gcc_unreachable ();
    }
}

/* Calculate the value of x register in the sequence.  */
static poly_int64
calculate_x_in_sequence (rtx reg)
{
  std::map<unsigned, rtx> regno_to_rtx;
  rtx_insn *insn;
  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      rtx pat = PATTERN (insn);
      rtx dest = SET_DEST (pat);

      if (GET_CODE (pat) == CLOBBER)
	continue;

      if (SUBREG_P (dest))
	continue;

      gcc_assert (REG_P (dest));
      rtx note = find_reg_equal_equiv_note (insn);
      unsigned regno = REGNO (dest);
      if (note)
	regno_to_rtx[regno] = XEXP (note, 0);
      else
	regno_to_rtx[regno] = SET_SRC (pat);
    }

  return eval_value (reg, regno_to_rtx);
}

typedef enum
{
  POLY_TEST_DIMODE,
  POLY_TEST_PMODE
} poly_test_mode_t;

static void
simple_poly_selftest (const char *arch, enum riscv_abi_type abi,
		      const std::vector<machine_mode> &modes)
{
  riscv_selftest_arch_abi_setter rv (arch, abi);
  rtl_dump_test t (SELFTEST_LOCATION, locate_file ("riscv/empty-func.rtl"));
  set_new_first_and_last_insn (NULL, NULL);

  for (machine_mode mode : modes)
    emit_move_insn (gen_reg_rtx (mode),
		    gen_int_mode (BYTES_PER_RISCV_VECTOR, mode));
}

static void
run_poly_int_selftest (const char *arch, enum riscv_abi_type abi,
		       poly_test_mode_t test_mode,
		       const std::vector<poly_int64> &worklist)
{
  riscv_selftest_arch_abi_setter rv (arch, abi);
  rtl_dump_test t (SELFTEST_LOCATION, locate_file ("riscv/empty-func.rtl"));
  set_new_first_and_last_insn (NULL, NULL);
  machine_mode mode = VOIDmode;

  switch (test_mode)
    {
    case POLY_TEST_DIMODE:
      mode = DImode;
      break;
    case POLY_TEST_PMODE:
      mode = Pmode;
      break;
    default:
      gcc_unreachable ();
    }

  for (const poly_int64 &poly_val : worklist)
    {
      start_sequence ();
      rtx dest = gen_reg_rtx (mode);
      emit_move_insn (dest, gen_int_mode (poly_val, mode));
      ASSERT_TRUE (known_eq (calculate_x_in_sequence (dest), poly_val));
      end_sequence ();
    }
}

static void
run_poly_int_selftests (void)
{
  std::vector<poly_int64> worklist
    = {BYTES_PER_RISCV_VECTOR,	    BYTES_PER_RISCV_VECTOR * 8,
       BYTES_PER_RISCV_VECTOR * 32, -BYTES_PER_RISCV_VECTOR * 8,
       -BYTES_PER_RISCV_VECTOR * 32, BYTES_PER_RISCV_VECTOR * 7,
       BYTES_PER_RISCV_VECTOR * 31, -BYTES_PER_RISCV_VECTOR * 7,
       -BYTES_PER_RISCV_VECTOR * 31, BYTES_PER_RISCV_VECTOR * 9,
       BYTES_PER_RISCV_VECTOR * 33, -BYTES_PER_RISCV_VECTOR * 9,
       -BYTES_PER_RISCV_VECTOR * 33, poly_int64 (207, 0),
       poly_int64 (-207, 0),	    poly_int64 (0, 207),
       poly_int64 (0, -207),	    poly_int64 (5555, 0),
       poly_int64 (0, 5555),	    poly_int64 (4096, 4096),
       poly_int64 (17, 4088),	    poly_int64 (3889, 4104),
       poly_int64 (-4096, -4096),   poly_int64 (219, -4088),
       poly_int64 (-4309, -4104),   poly_int64 (-7337, 88),
       poly_int64 (9317, -88),	    poly_int64 (4, 4),
       poly_int64 (17, 4),	    poly_int64 (-7337, 4),
       poly_int64 (-4, -4),	    poly_int64 (-389, -4),
       poly_int64 (4789, -4),	    poly_int64 (-5977, 1508),
       poly_int64 (219, -1508),	    poly_int64 (2, 2),
       poly_int64 (33, 2),	    poly_int64 (-7337, 2),
       poly_int64 (-2, -2),	    poly_int64 (-389, -2),
       poly_int64 (4789, -2),	    poly_int64 (-3567, 954),
       poly_int64 (945, -954),	    poly_int64 (1, 1),
       poly_int64 (977, 1),	    poly_int64 (-339, 1),
       poly_int64 (-1, -1),	    poly_int64 (-12, -1),
       poly_int64 (44, -1),	    poly_int64 (9567, 77),
       poly_int64 (3467, -77)};

  simple_poly_selftest ("rv64imafdv", ABI_LP64D,
			{QImode, HImode, SImode, DImode});
  simple_poly_selftest ("rv32imafdv", ABI_ILP32D, {QImode, HImode, SImode});

  run_poly_int_selftest ("rv64imafdv", ABI_LP64D, POLY_TEST_PMODE, worklist);
  run_poly_int_selftest ("rv64imafd_zve32x1p0", ABI_LP64D, POLY_TEST_PMODE,
			 worklist);
  run_poly_int_selftest ("rv32imafdv", ABI_ILP32, POLY_TEST_PMODE, worklist);
  run_poly_int_selftest ("rv32imafdv", ABI_ILP32, POLY_TEST_DIMODE, worklist);
  run_poly_int_selftest ("rv32imafd_zve32x1p0", ABI_ILP32D, POLY_TEST_PMODE,
			 worklist);
  run_poly_int_selftest ("rv32imafd_zve32x1p0", ABI_ILP32D, POLY_TEST_DIMODE,
			 worklist);
  simple_poly_selftest ("rv64imafdv_zvl256b", ABI_LP64D,
			{QImode, HImode, SImode, DImode});
  simple_poly_selftest ("rv64imafdv_zvl512b", ABI_LP64D,
			{QImode, HImode, SImode, DImode});
  simple_poly_selftest ("rv64imafdv_zvl1024b", ABI_LP64D,
			{QImode, HImode, SImode, DImode});
  simple_poly_selftest ("rv64imafdv_zvl2048b", ABI_LP64D,
			{QImode, HImode, SImode, DImode});
  simple_poly_selftest ("rv64imafdv_zvl4096b", ABI_LP64D,
			{QImode, HImode, SImode, DImode});
}

static void
run_const_vector_selftests (void)
{
  /* We dont't need to do the redundant tests in different march && mabi.
     Just pick up the march && mabi which fully support all RVV modes.  */
  riscv_selftest_arch_abi_setter rv ("rv64imafdcv", ABI_LP64D);
  rtl_dump_test t (SELFTEST_LOCATION, locate_file ("riscv/empty-func.rtl"));
  set_new_first_and_last_insn (NULL, NULL);

  machine_mode mode;
  std::vector<HOST_WIDE_INT> worklist = {-111, -17, -16, 7, 15, 16, 111};

  FOR_EACH_MODE_IN_CLASS (mode, MODE_VECTOR_INT)
    {
      if (riscv_v_ext_vector_mode_p (mode))
	{
	  for (const HOST_WIDE_INT &val : worklist)
	    {
	      start_sequence ();
	      rtx dest = gen_reg_rtx (mode);
	      rtx dup = gen_const_vec_duplicate (mode, GEN_INT (val));
	      emit_move_insn (dest, dup);
	      rtx_insn *insn = get_last_insn ();
	      rtx src = SET_SRC (PATTERN (insn));
	      /* 1. Should be vmv.v.i for in rang of -16 ~ 15.
		 2. Should be vmv.v.x for exceed -16 ~ 15.  */
	      if (IN_RANGE (val, -16, 15))
		ASSERT_TRUE (
		  rtx_equal_p (XEXP (SET_SRC (PATTERN (insn)), 1), dup));
	      else
		ASSERT_TRUE (GET_CODE (src) == VEC_DUPLICATE);
	      end_sequence ();
	    }
	}
    }

  FOR_EACH_MODE_IN_CLASS (mode, MODE_VECTOR_FLOAT)
    {
      if (riscv_v_ext_vector_mode_p (mode))
	{
	  scalar_mode inner_mode = GET_MODE_INNER (mode);
	  REAL_VALUE_TYPE f = REAL_VALUE_ATOF ("0.2928932", inner_mode);
	  rtx ele = const_double_from_real_value (f, inner_mode);

	  start_sequence ();
	  rtx dest = gen_reg_rtx (mode);
	  rtx dup = gen_const_vec_duplicate (mode, ele);
	  emit_move_insn (dest, dup);
	  rtx_insn *insn = get_last_insn ();
	  rtx src = SET_SRC (PATTERN (insn));
	  /* Should always be vfmv.v.f.  */
	  ASSERT_TRUE (GET_CODE (src) == VEC_DUPLICATE);
	  end_sequence ();
	}
    }

  FOR_EACH_MODE_IN_CLASS (mode, MODE_VECTOR_BOOL)
    {
      /* Test vmset.m.  */
      if (riscv_v_ext_vector_mode_p (mode))
	{
	  start_sequence ();
	  rtx dest = gen_reg_rtx (mode);
	  emit_move_insn (dest, CONSTM1_RTX (mode));
	  rtx_insn *insn = get_last_insn ();
	  rtx src = XEXP (SET_SRC (PATTERN (insn)), 1);
	  ASSERT_TRUE (rtx_equal_p (src, CONSTM1_RTX (mode)));
	  end_sequence ();
	}
    }
}

static void
run_broadcast_selftests (void)
{
  /* We dont't need to do the redundant tests in different march && mabi.
     Just pick up the march && mabi which fully support all RVV modes.  */
  riscv_selftest_arch_abi_setter rv ("rv64imafdcv", ABI_LP64D);
  rtl_dump_test t (SELFTEST_LOCATION, locate_file ("riscv/empty-func.rtl"));
  set_new_first_and_last_insn (NULL, NULL);

  machine_mode mode;

#define BROADCAST_TEST(MODE_CLASS)                                             \
  FOR_EACH_MODE_IN_CLASS (mode, MODE_VECTOR_INT)                               \
    {                                                                          \
      if (riscv_v_ext_vector_mode_p (mode))                                    \
	{                                                                      \
	  rtx_insn *insn;                                                      \
	  rtx src;                                                             \
	  scalar_mode inner_mode = GET_MODE_INNER (mode);                      \
	  /* Test vlse.v with zero stride.  */                                 \
	  start_sequence ();                                                   \
	  rtx addr = gen_reg_rtx (Pmode);                                      \
	  rtx mem = gen_rtx_MEM (inner_mode, addr);                            \
	  expand_vector_broadcast (mode, mem);                                 \
	  insn = get_last_insn ();                                             \
	  src = SET_SRC (PATTERN (insn));                                      \
	  if (strided_load_broadcast_p ())                                     \
	    {                                                                  \
	      ASSERT_TRUE (MEM_P (XEXP (src, 0)));                             \
	      ASSERT_TRUE (                                                    \
		rtx_equal_p (src,                                              \
			     gen_rtx_VEC_DUPLICATE (mode, XEXP (src, 0))));    \
	    }                                                                  \
	  end_sequence ();                                                     \
	  /* Test vmv.v.x or vfmv.v.f.  */                                     \
	  start_sequence ();                                                   \
	  rtx reg = gen_reg_rtx (inner_mode);                                  \
	  expand_vector_broadcast (mode, reg);                                 \
	  insn = get_last_insn ();                                             \
	  src = SET_SRC (PATTERN (insn));                                      \
	  ASSERT_TRUE (REG_P (XEXP (src, 0)));                                 \
	  ASSERT_TRUE (                                                        \
	    rtx_equal_p (src, gen_rtx_VEC_DUPLICATE (mode, XEXP (src, 0))));   \
	  end_sequence ();                                                     \
	}                                                                      \
    }

  BROADCAST_TEST (MODE_VECTOR_INT)
  BROADCAST_TEST (MODE_VECTOR_FLOAT)
}

static void
test_vectorize_related_mode (machine_mode vec_mode, scalar_mode ele_mode,
			     machine_mode expected)
{
  opt_machine_mode result = riscv_vector::vectorize_related_mode (vec_mode,
								  ele_mode, 0);
  machine_mode result_mode = result.else_void ();
  ASSERT_TRUE (result_mode == expected);
}

static void
run_vectorize_related_mode_vla_selftests (void)
{
  riscv_selftest_arch_abi_setter rv ("rv64imafdcv", ABI_LP64D);
  enum rvv_max_lmul_enum backup_rvv_max_lmul = rvv_max_lmul;
  rvv_max_lmul = RVV_M1;

  test_vectorize_related_mode (RVVM1QImode, SImode, RVVM1SImode);
  test_vectorize_related_mode (RVVM2QImode, SImode, RVVM1SImode);
  test_vectorize_related_mode (RVVM4QImode, SImode, RVVM1SImode);
  test_vectorize_related_mode (RVVM8QImode, SImode, RVVM1SImode);
  test_vectorize_related_mode (RVVM8QImode, DImode, RVVM1DImode);
  test_vectorize_related_mode (RVVM8QImode, QImode, RVVM1QImode);
  test_vectorize_related_mode (RVVM8QImode, HImode, RVVM1HImode);

  rvv_max_lmul = RVV_M2;

  test_vectorize_related_mode (RVVM1QImode, SImode, RVVM2SImode);
  test_vectorize_related_mode (RVVM2QImode, SImode, RVVM2SImode);
  test_vectorize_related_mode (RVVM4QImode, SImode, RVVM2SImode);
  test_vectorize_related_mode (RVVM8QImode, SImode, RVVM2SImode);
  test_vectorize_related_mode (RVVM8QImode, DImode, RVVM2DImode);
  test_vectorize_related_mode (RVVM8QImode, QImode, RVVM2QImode);
  test_vectorize_related_mode (RVVM8QImode, HImode, RVVM2HImode);

  rvv_max_lmul = RVV_M4;

  test_vectorize_related_mode (RVVM1QImode, SImode, RVVM4SImode);
  test_vectorize_related_mode (RVVM2QImode, SImode, RVVM4SImode);
  test_vectorize_related_mode (RVVM4QImode, SImode, RVVM4SImode);
  test_vectorize_related_mode (RVVM8QImode, SImode, RVVM4SImode);
  test_vectorize_related_mode (RVVM8QImode, DImode, RVVM4DImode);
  test_vectorize_related_mode (RVVM8QImode, QImode, RVVM4QImode);
  test_vectorize_related_mode (RVVM8QImode, HImode, RVVM4HImode);

  rvv_max_lmul = RVV_M8;

  test_vectorize_related_mode (RVVM1QImode, SImode, RVVM4SImode);
  test_vectorize_related_mode (RVVM2QImode, SImode, RVVM8SImode);
  test_vectorize_related_mode (RVVM4QImode, SImode, RVVM8SImode);
  test_vectorize_related_mode (RVVM8QImode, SImode, RVVM8SImode);
  test_vectorize_related_mode (RVVM8QImode, DImode, RVVM8DImode);
  test_vectorize_related_mode (RVVM8QImode, QImode, RVVM8QImode);
  test_vectorize_related_mode (RVVM8QImode, HImode, RVVM8HImode);

  rvv_max_lmul = backup_rvv_max_lmul;
}

static void
run_vectorize_related_mode_vls_rv64gcv_selftests ()
{
  enum rvv_vector_bits_enum backup_rvv_vector_bits = rvv_vector_bits;
  rvv_vector_bits = RVV_VECTOR_BITS_SCALABLE;
  riscv_selftest_arch_abi_setter rv ("rv64imafdcv", ABI_LP64D);
  enum rvv_max_lmul_enum backup_rvv_max_lmul = rvv_max_lmul;
  rvv_max_lmul = RVV_M1;

  test_vectorize_related_mode (  V16QImode, QImode,   V16QImode);
  test_vectorize_related_mode (  V16QImode, HImode,    V8HImode);
  test_vectorize_related_mode (  V16QImode, SImode,    V4SImode);
  test_vectorize_related_mode (  V16QImode, DImode,    V2DImode);

  rvv_max_lmul = RVV_M2;

  test_vectorize_related_mode (  V32QImode, QImode,   V32QImode);
  test_vectorize_related_mode (  V32QImode, HImode,   V16HImode);
  test_vectorize_related_mode (  V32QImode, SImode,    V8SImode);
  test_vectorize_related_mode (  V32QImode, DImode,    V4DImode);

  rvv_max_lmul = RVV_M4;

  test_vectorize_related_mode (  V64QImode, QImode,   V64QImode);
  test_vectorize_related_mode (  V64QImode, HImode,   V32HImode);
  test_vectorize_related_mode (  V64QImode, SImode,   V16SImode);
  test_vectorize_related_mode (  V64QImode, DImode,    V8DImode);

  rvv_max_lmul = RVV_M8;

  test_vectorize_related_mode ( V128QImode, QImode,  V128QImode);
  test_vectorize_related_mode ( V128QImode, HImode,   V64HImode);
  test_vectorize_related_mode ( V128QImode, SImode,   V32SImode);
  test_vectorize_related_mode ( V128QImode, DImode,   V16DImode);

  rvv_vector_bits = backup_rvv_vector_bits;
  rvv_max_lmul = backup_rvv_max_lmul;
}

static void
run_vectorize_related_mode_vls_rv32gc_zve32x_zvl256b_selftests ()
{
  enum rvv_vector_bits_enum backup_rvv_vector_bits = rvv_vector_bits;
  rvv_vector_bits = RVV_VECTOR_BITS_SCALABLE;
  riscv_selftest_arch_abi_setter rv ("rv32gc_zve32x_zvl256b", ABI_ILP32D);
  enum rvv_max_lmul_enum backup_rvv_max_lmul = rvv_max_lmul;
  rvv_max_lmul = RVV_M1;

  test_vectorize_related_mode (  V32QImode, QImode,   V32QImode);
  test_vectorize_related_mode (  V32QImode, HImode,   V16HImode);
  test_vectorize_related_mode (  V32QImode, SImode,    V8SImode);
  test_vectorize_related_mode (  V32QImode, DImode,    VOIDmode);

  test_vectorize_related_mode (  V16QImode, QImode,   V16QImode);
  test_vectorize_related_mode (  V16QImode, HImode,   V16HImode);
  test_vectorize_related_mode (  V16QImode, SImode,    V8SImode);
  test_vectorize_related_mode (  V16QImode, DImode,    VOIDmode);

  rvv_max_lmul = RVV_M2;

  test_vectorize_related_mode (  V32QImode, QImode,   V32QImode);
  test_vectorize_related_mode (  V32QImode, HImode,   V32HImode);
  test_vectorize_related_mode (  V32QImode, SImode,   V16SImode);
  test_vectorize_related_mode (  V32QImode, DImode,    VOIDmode);

  rvv_max_lmul = RVV_M4;

  test_vectorize_related_mode ( V128QImode, QImode,  V128QImode);
  test_vectorize_related_mode ( V128QImode, HImode,   V64HImode);
  test_vectorize_related_mode ( V128QImode, SImode,   V32SImode);
  test_vectorize_related_mode ( V128QImode, DImode,    VOIDmode);

  rvv_max_lmul = RVV_M8;

  test_vectorize_related_mode ( V128QImode, QImode,  V128QImode);
  test_vectorize_related_mode ( V128QImode, HImode,  V128HImode);
  test_vectorize_related_mode ( V128QImode, SImode,   V64SImode);
  test_vectorize_related_mode ( V128QImode, DImode,    VOIDmode);

  rvv_vector_bits = backup_rvv_vector_bits;
  rvv_max_lmul = backup_rvv_max_lmul;

}

static void
run_vectorize_related_mode_vls_selftests (void)
{
  run_vectorize_related_mode_vls_rv64gcv_selftests ();
  run_vectorize_related_mode_vls_rv32gc_zve32x_zvl256b_selftests ();
}

static void
run_vectorize_related_mode_selftests (void)
{
  run_vectorize_related_mode_vla_selftests ();
  run_vectorize_related_mode_vls_selftests ();
}

namespace selftest {
/* Run all target-specific selftests.  */
void
riscv_run_selftests (void)
{
  if (!BYTES_PER_RISCV_VECTOR.is_constant ())
    /* We can know POLY value = [4, 4] when BYTES_PER_RISCV_VECTOR
       is !is_constant () since we can use csrr vlenb and scalar shift
       instruction to compute such POLY value and store it into a scalar
       register.  Wheras, we can't know [4, 4] on it is specified as
       FIXED-VLMAX since BYTES_PER_RISCV_VECTOR = 16 for -march=rv64gcv
       and csrr vlenb is 16 which is totally unrelated to any
       compile-time unknown POLY value.

       Since we never need to compute a compile-time unknown POLY value
       when -mrvv-vector-bits=zvl, disable poly
       selftests in such situation.  */
    run_poly_int_selftests ();
  run_const_vector_selftests ();
  run_broadcast_selftests ();
  run_vectorize_related_mode_selftests ();
}
} // namespace selftest
#endif /* #if CHECKING_P */
