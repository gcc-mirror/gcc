/* Unit tests for RTL-handling.
   Copyright (C) 2015-2024 Free Software Foundation, Inc.

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

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "opts.h"
#include "hash-set.h"
#include "fixed-value.h"
#include "alias.h"
#include "flags.h"
#include "symtab.h"
#include "tree-core.h"
#include "stor-layout.h"
#include "tree.h"
#include "stringpool.h"
#include "stor-layout.h"
#include "rtl.h"
#include "pretty-print.h"
#include "cfgbuild.h"
#include "print-rtl.h"
#include "selftest.h"
#include "selftest-rtl.h"
#include "function.h"
#include "memmodel.h"
#include "emit-rtl.h"

#if CHECKING_P

namespace selftest {

/* Verify that PAT is printed as EXPECTED.  Helper function for
   selftests.  */

static void
verify_print_pattern (const char *expected, rtx pat)
{
  pretty_printer pp;
  print_pattern (&pp, pat, 1);
  ASSERT_STREQ (expected, pp_formatted_text (&pp));
}

/* Verify that X is dumped as EXPECTED_DUMP, using compact mode.
   Use LOC as the effective location when reporting errors.  */

void
assert_rtl_dump_eq (const location &loc, const char *expected_dump, rtx x,
		    rtx_reuse_manager *reuse_manager)
{
  named_temp_file tmp_out (".rtl");
  FILE *outfile = fopen (tmp_out.get_filename (), "w");
  rtx_writer w (outfile, 0, false, true, reuse_manager);
  w.print_rtl (x);
  fclose (outfile);

  char *dump = read_file (SELFTEST_LOCATION, tmp_out.get_filename ());
  ASSERT_STREQ_AT (loc, expected_dump, dump);
  free (dump);
}

/* Verify that regs are dumped as expected (in compact mode).  */

static void
test_dumping_regs ()
{
  /* Dumps of hard regs contain a target-specific name, so we don't test
     it here; this can be tested in target-specific selftests.  */

  /* Test dumping of virtual regs.  The various virtual regs are inited as
     Pmode, so this is target-specific.  The tests below assume DImode, so
     only run the tests for targets where Pmode is DImode.  */
  if (Pmode == DImode)
    {
      ASSERT_RTL_DUMP_EQ ("(reg:DI virtual-incoming-args)",
			  virtual_incoming_args_rtx);
      ASSERT_RTL_DUMP_EQ ("(reg:DI virtual-stack-vars)",
			  virtual_stack_vars_rtx);
      ASSERT_RTL_DUMP_EQ ("(reg:DI virtual-stack-dynamic)",
			  virtual_stack_dynamic_rtx);
      ASSERT_RTL_DUMP_EQ ("(reg:DI virtual-outgoing-args)",
			  virtual_outgoing_args_rtx);
      ASSERT_RTL_DUMP_EQ ("(reg:DI virtual-cfa)",
			  virtual_cfa_rtx);
      ASSERT_RTL_DUMP_EQ ("(reg:DI virtual-preferred-stack-boundary)",
			  virtual_preferred_stack_boundary_rtx);
    }

  /* Test dumping of non-virtual pseudos.  */
  ASSERT_RTL_DUMP_EQ ("(reg:SI <0>)",
    gen_raw_REG (SImode, LAST_VIRTUAL_REGISTER + 1));
  ASSERT_RTL_DUMP_EQ ("(reg:SI <1>)",
    gen_raw_REG (SImode, LAST_VIRTUAL_REGISTER + 2));
}

/* Verify that insns are dumped as expected (in compact mode).  */

static void
test_dumping_insns ()
{
  /* Barriers.  */
  rtx_barrier *barrier = as_a <rtx_barrier *> (rtx_alloc (BARRIER));
  SET_NEXT_INSN (barrier) = NULL;
  ASSERT_RTL_DUMP_EQ ("(cbarrier 0)\n", barrier);

  /* Labels.  */
  rtx_insn *label = gen_label_rtx ();
  CODE_LABEL_NUMBER (label) = 42;
  ASSERT_RTL_DUMP_EQ ("(clabel 0 42)\n", label);

  LABEL_NAME (label)= "some_label";
  ASSERT_RTL_DUMP_EQ ("(clabel 0 42 (\"some_label\"))\n", label);
}

/* Manually exercise the rtx_reuse_manager code.  */

static void
test_dumping_rtx_reuse ()
{
  rtx_reuse_manager r;

  rtx x = rtx_alloc (SCRATCH);
  rtx y = rtx_alloc (SCRATCH);
  rtx z = rtx_alloc (SCRATCH);

  /* x and y will be seen more than once.  */
  r.preprocess (x);
  r.preprocess (x);
  r.preprocess (y);
  r.preprocess (y);

  /* z will be only seen once.  */
  r.preprocess (z);

  /* Verify that x and y have been assigned reuse IDs.  */
  int reuse_id_for_x;
  ASSERT_TRUE (r.has_reuse_id (x, &reuse_id_for_x));
  ASSERT_EQ (0, reuse_id_for_x);

  int reuse_id_for_y;
  ASSERT_TRUE (r.has_reuse_id (y, &reuse_id_for_y));
  ASSERT_EQ (1, reuse_id_for_y);

  /* z is only seen once and thus shouldn't get a reuse ID.  */
  ASSERT_FALSE (r.has_reuse_id (z, NULL));

  /* The first dumps of x and y should be prefixed by reuse ID;
     all subsequent dumps of them should show up as "reuse_rtx".  */
  ASSERT_RTL_DUMP_EQ_WITH_REUSE ("(0|scratch)", x, &r);
  ASSERT_RTL_DUMP_EQ_WITH_REUSE ("(reuse_rtx 0)", x, &r);
  ASSERT_RTL_DUMP_EQ_WITH_REUSE ("(reuse_rtx 0)", x, &r);

  ASSERT_RTL_DUMP_EQ_WITH_REUSE ("(1|scratch)", y, &r);
  ASSERT_RTL_DUMP_EQ_WITH_REUSE ("(reuse_rtx 1)", y, &r);
  ASSERT_RTL_DUMP_EQ_WITH_REUSE ("(reuse_rtx 1)", y, &r);

  /* z only appears once and thus shouldn't be prefixed with a
     reuse ID.  */
  ASSERT_RTL_DUMP_EQ_WITH_REUSE ("(scratch)", z, &r);
}

/* Unit testing of "single_set".  */

static void
test_single_set ()
{
  /* A label is not a SET.  */
  ASSERT_EQ (NULL_RTX, single_set (gen_label_rtx ()));

  /* An unconditional jump insn is a single SET.  */
  rtx set_pc = gen_rtx_SET (pc_rtx,
			    gen_rtx_LABEL_REF (VOIDmode,
					       gen_label_rtx ()));
  rtx_insn *jump_insn = emit_jump_insn (set_pc);
  ASSERT_EQ (set_pc, single_set (jump_insn));

  /* etc */
}

/* Construct an unconditional jump to a label, and verify that
   various properties of it are sane.  */

static void
test_uncond_jump ()
{
  set_new_first_and_last_insn (NULL, NULL);
  rtx_insn *label = gen_label_rtx ();
  rtx jump_pat = gen_rtx_SET (pc_rtx,
			      gen_rtx_LABEL_REF (VOIDmode,
						 label));
  ASSERT_EQ (SET, jump_pat->code);
  ASSERT_EQ (LABEL_REF, SET_SRC (jump_pat)->code);
  ASSERT_EQ (label, label_ref_label (SET_SRC (jump_pat)));
  ASSERT_EQ (PC, SET_DEST (jump_pat)->code);

  verify_print_pattern ("pc=L0", jump_pat);

  ASSERT_RTL_DUMP_EQ ("(set (pc)\n"
		      "    (label_ref 0))",
		      jump_pat);

  rtx_insn *jump_insn = emit_jump_insn (jump_pat);
  ASSERT_FALSE (any_condjump_p (jump_insn));
  ASSERT_TRUE (any_uncondjump_p (jump_insn));
  ASSERT_TRUE (pc_set (jump_insn));
  ASSERT_TRUE (simplejump_p (jump_insn));
  ASSERT_TRUE (onlyjump_p (jump_insn));
  ASSERT_TRUE (control_flow_insn_p (jump_insn));

  ASSERT_RTL_DUMP_EQ ("(cjump_insn 1 (set (pc)\n"
		      "        (label_ref 0)))\n",
		      jump_insn);
}

template<unsigned int N>
struct const_poly_int_tests
{
  static void run ();
};

template<>
struct const_poly_int_tests<1>
{
  static void run () {}
};

/* Test various CONST_POLY_INT properties.  */

template<unsigned int N>
void
const_poly_int_tests<N>::run ()
{
  using poly_int64 = poly_int<N, HOST_WIDE_INT>;
  rtx x1 = gen_int_mode (poly_int64 (1, 1), QImode);
  rtx x255 = gen_int_mode (poly_int64 (1, 255), QImode);

  /* Test that constants are unique.  */
  ASSERT_EQ (x1, gen_int_mode (poly_int64 (1, 1), QImode));
  ASSERT_NE (x1, gen_int_mode (poly_int64 (1, 1), HImode));
  ASSERT_NE (x1, x255);

  /* Test const_poly_int_value.  */
  ASSERT_KNOWN_EQ (const_poly_int_value (x1), poly_int64 (1, 1));
  ASSERT_KNOWN_EQ (const_poly_int_value (x255), poly_int64 (1, -1));

  /* Test rtx_to_poly_int64.  */
  ASSERT_KNOWN_EQ (rtx_to_poly_int64 (x1), poly_int64 (1, 1));
  ASSERT_KNOWN_EQ (rtx_to_poly_int64 (x255), poly_int64 (1, -1));
  ASSERT_MAYBE_NE (rtx_to_poly_int64 (x255), poly_int64 (1, 255));

  /* Test plus_constant of a symbol.  */
  rtx symbol = gen_rtx_SYMBOL_REF (Pmode, "foo");
  rtx offset1 = gen_int_mode (poly_int64 (9, 11), Pmode);
  rtx sum1 = gen_rtx_CONST (Pmode, gen_rtx_PLUS (Pmode, symbol, offset1));
  ASSERT_RTX_EQ (plus_constant (Pmode, symbol, poly_int64 (9, 11)), sum1);

  /* Test plus_constant of a CONST.  */
  rtx offset2 = gen_int_mode (poly_int64 (12, 20), Pmode);
  rtx sum2 = gen_rtx_CONST (Pmode, gen_rtx_PLUS (Pmode, symbol, offset2));
  ASSERT_RTX_EQ (plus_constant (Pmode, sum1, poly_int64 (3, 9)), sum2);

  /* Test a cancelling plus_constant.  */
  ASSERT_EQ (plus_constant (Pmode, sum2, poly_int64 (-12, -20)), symbol);

  /* Test plus_constant on integer constants.  */
  ASSERT_EQ (plus_constant (QImode, const1_rtx, poly_int64 (4, -2)),
	     gen_int_mode (poly_int64 (5, -2), QImode));
  ASSERT_EQ (plus_constant (QImode, x1, poly_int64 (4, -2)),
	     gen_int_mode (poly_int64 (5, -1), QImode));
}

/* Check dumping of repeated RTL vectors.  */

static void
test_dumping_repeat ()
{
  rtx p = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (3));
  XVECEXP (p, 0, 0) = const0_rtx;
  XVECEXP (p, 0, 1) = const0_rtx;
  XVECEXP (p, 0, 2) = const0_rtx;
  ASSERT_RTL_DUMP_EQ ("(parallel [\n"
		      "        (const_int 0) repeated x3\n"
		      "    ])",
		      p);

  XVECEXP (p, 0, 1) = const1_rtx;
  ASSERT_RTL_DUMP_EQ ("(parallel [\n"
		      "        (const_int 0)\n"
		      "        (const_int 1)\n"
		      "        (const_int 0)\n"
		      "    ])",
		      p);
}

/* Run all of the selftests within this file.  */

void
rtl_tests_cc_tests ()
{
  test_dumping_regs ();
  test_dumping_insns ();
  test_dumping_rtx_reuse ();
  test_single_set ();
  test_uncond_jump ();
  const_poly_int_tests<NUM_POLY_INT_COEFFS>::run ();
  test_dumping_repeat ();

  /* Purge state.  */
  set_first_insn (NULL);
  set_last_insn (NULL);
}

} // namespace selftest
#endif /* #if CHECKING_P */
