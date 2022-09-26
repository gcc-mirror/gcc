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
}
namespace selftest {
/* Run all target-specific selftests.  */
void
riscv_run_selftests (void)
{
  run_poly_int_selftests ();
}
} // namespace selftest
#endif /* #if CHECKING_P */
