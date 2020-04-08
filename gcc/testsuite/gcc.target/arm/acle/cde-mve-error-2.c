#include "arm_cde.h"

/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_1m_main_cde_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_main_cde_mve_fp } */

/* This file and cde-mve-error-tests.c are split since there are two kinds of
   errors happening here.  The errors in the other file cause the compiler to
   not reach the errors found here, hence they need to be in a different file
   so we can inspect these ones.  */

uint8x16_t test_bad_immediates (uint8x16_t n, uint8x16_t m, int someval)
{
  uint8x16_t accum = (uint8x16_t)(uint32x4_t){0, 0, 0, 0};

  /* We always different constants for the pairs (__arm_vcx2q and
     __arm_vcx2q_u8) and (__arm_vcx3q and __arm_vcx3q_u8) despite them mapping
     to the same builtin and us wanting to test the same thing in each block.

     This is because we have told the compiler that these functions are
     constant and pure (i.e. produce a value solely based on their arguments
     and have no side-effects).

     With that information the compiler eliminates duplicate calls to the
     functions, and we only get error messages for one of the pairs.
     Hence, in order to get error messages for both function calls, we use
     different constants.  */

  /* `coproc' not enabled.  */
  accum += __arm_vcx1q_u8 (1, 4095);           /* { dg-error {coprocessor 1 is not enabled with \+cdecp1} } */
  accum += __arm_vcx1qa (1, accum, 4095);      /* { dg-error {coprocessor 1 is not enabled with \+cdecp1} } */
  accum += __arm_vcx2q (1, n, 126);            /* { dg-error {coprocessor 1 is not enabled with \+cdecp1} } */
  accum += __arm_vcx2q_u8 (1, n, 127);         /* { dg-error {coprocessor 1 is not enabled with \+cdecp1} } */
  accum += __arm_vcx2qa (1, accum, n, 127);    /* { dg-error {coprocessor 1 is not enabled with \+cdecp1} } */
  accum += __arm_vcx3q_u8 (1, n, m, 14);       /* { dg-error {coprocessor 1 is not enabled with \+cdecp1} } */
  accum += __arm_vcx3q (1, n, m, 15);          /* { dg-error {coprocessor 1 is not enabled with \+cdecp1} } */
  accum += __arm_vcx3qa (1, accum, n, m, 15);  /* { dg-error {coprocessor 1 is not enabled with \+cdecp1} } */

  /* `coproc' out of range.  */
  accum += __arm_vcx1q_u8 (8, 4095);           /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with \+cdecp<N>} } */
  accum += __arm_vcx1qa (8, accum, 4095);      /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with \+cdecp<N>} } */
  accum += __arm_vcx2q (8, n, 126);            /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with \+cdecp<N>} } */
  accum += __arm_vcx2q_u8 (8, n, 127);         /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with \+cdecp<N>} } */
  accum += __arm_vcx2qa (8, accum, n, 127);    /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with \+cdecp<N>} } */
  accum += __arm_vcx3q_u8 (8, n, m, 14);       /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with \+cdecp<N>} } */
  accum += __arm_vcx3q (8, n, m, 15);          /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with \+cdecp<N>} } */
  accum += __arm_vcx3qa (8, accum, n, m, 15);  /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with \+cdecp<N>} } */

  /* `imm' out of range.  */
  accum += __arm_vcx1q_u8 (0, 4096);           /* { dg-error {argument 2 must be a constant immediate in range \[0-4095\]} } */
  accum += __arm_vcx1qa (0, accum, 4096);      /* { dg-error {argument 3 must be a constant immediate in range \[0-4095\]} } */
  accum += __arm_vcx2q (0, n, 128);            /* { dg-error {argument 3 must be a constant immediate in range \[0-127\]} } */
  accum += __arm_vcx2q_u8 (0, n, 129);         /* { dg-error {argument 3 must be a constant immediate in range \[0-127\]} } */
  accum += __arm_vcx2qa (0, accum, n, 128);    /* { dg-error {argument 4 must be a constant immediate in range \[0-127\]} } */
  accum += __arm_vcx3q_u8 (0, n, m, 16);       /* { dg-error {argument 4 must be a constant immediate in range \[0-15\]} } */
  accum += __arm_vcx3q (0, n, m, 17);          /* { dg-error {argument 4 must be a constant immediate in range \[0-15\]} } */
  accum += __arm_vcx3qa (0, accum, n, m, 16);  /* { dg-error {argument 5 must be a constant immediate in range \[0-15\]} } */

  /* `imm' is not an immediate.  */
  accum += __arm_vcx1q_u8 (0, someval);             /* { dg-error {argument 2 must be a constant immediate in range \[0-4095\]} } */
  accum += __arm_vcx1qa (0, accum, someval);        /* { dg-error {argument 3 must be a constant immediate in range \[0-4095\]} } */
  accum += __arm_vcx2q (0, n, someval);             /* { dg-error {argument 3 must be a constant immediate in range \[0-127\]} } */
  accum += __arm_vcx2q_u8 (6, n, someval);          /* { dg-error {argument 3 must be a constant immediate in range \[0-127\]} } */
  accum += __arm_vcx2qa (0, accum, n, someval);     /* { dg-error {argument 4 must be a constant immediate in range \[0-127\]} } */
  accum += __arm_vcx3q_u8 (0, n, m, someval);       /* { dg-error {argument 4 must be a constant immediate in range \[0-15\]} } */
  accum += __arm_vcx3q (6, n, m, someval);          /* { dg-error {argument 4 must be a constant immediate in range \[0-15\]} } */
  accum += __arm_vcx3qa (0, accum, n, m, someval);  /* { dg-error {argument 5 must be a constant immediate in range \[0-15\]} } */

  /* `coproc' is not an immediate.  */
  accum += __arm_vcx1q_u8 (someval, 4095);           /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with \+cdecp<N>} } */
  accum += __arm_vcx1qa (someval, accum, 4095);      /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with \+cdecp<N>} } */
  accum += __arm_vcx2q (someval, n, 126);            /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with \+cdecp<N>} } */
  accum += __arm_vcx2q_u8 (someval, n, 127);         /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with \+cdecp<N>} } */
  accum += __arm_vcx2qa (someval, accum, n, 127);    /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with \+cdecp<N>} } */
  accum += __arm_vcx3q_u8 (someval, n, m, 14);       /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with \+cdecp<N>} } */
  accum += __arm_vcx3q (someval, n, m, 15);          /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with \+cdecp<N>} } */
  accum += __arm_vcx3qa (someval, accum, n, m, 15);  /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with \+cdecp<N>} } */

  /* `imm' is of wrong type.  */
  accum += __arm_vcx1q_u8 (0, "");           /* { dg-error {argument 2 must be a constant immediate in range \[0-4095\]} } */
  accum += __arm_vcx1qa (0, accum, "");      /* { dg-error {argument 3 must be a constant immediate in range \[0-4095\]} } */
  accum += __arm_vcx2q (0, n, "");            /* { dg-error {argument 3 must be a constant immediate in range \[0-127\]} } */
  accum += __arm_vcx2q_u8 (0, n, "x");         /* { dg-error {argument 3 must be a constant immediate in range \[0-127\]} } */
  accum += __arm_vcx2qa (0, accum, n, "");    /* { dg-error {argument 4 must be a constant immediate in range \[0-127\]} } */
  accum += __arm_vcx3q_u8 (0, n, m, "");       /* { dg-error {argument 4 must be a constant immediate in range \[0-15\]} } */
  accum += __arm_vcx3q (0, n, m, "x");          /* { dg-error {argument 4 must be a constant immediate in range \[0-15\]} } */
  accum += __arm_vcx3qa (0, accum, n, m, "");  /* { dg-error {argument 5 must be a constant immediate in range \[0-15\]} } */

  /* `coproc' is of wrong type.  */
  accum += __arm_vcx1q_u8 ("", 4095);           /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with \+cdecp<N>} } */
  accum += __arm_vcx1qa ("", accum, 4095);      /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with \+cdecp<N>} } */
  accum += __arm_vcx2q ("", n, 126);            /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with \+cdecp<N>} } */
  accum += __arm_vcx2q_u8 ("", n, 127);         /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with \+cdecp<N>} } */
  accum += __arm_vcx2qa ("", accum, n, 127);    /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with \+cdecp<N>} } */
  accum += __arm_vcx3q_u8 ("", n, m, 14);       /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with \+cdecp<N>} } */
  accum += __arm_vcx3q ("", n, m, 15);          /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with \+cdecp<N>} } */
  accum += __arm_vcx3qa ("", accum, n, m, 15);  /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with \+cdecp<N>} } */

  /* { dg-warning {passing argument 2 of '__builtin_arm_vcx1qv16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 80 } */
  /* { dg-warning {passing argument 1 of '__builtin_arm_vcx1qv16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 90 } */

  return accum;
}
