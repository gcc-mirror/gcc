#include "arm_cde.h"

/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_1m_main_cde_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_main_cde_mve_fp } */

/* The error checking files are split since there are three kinds of
   errors happening here.  Different error types cause errors at different
   times, which means the compiler stops and doesn't produce messages about the
   later errors.  Hence they need to be in a different file so we can inspect
   these ones.  */

uint8x16_t test_bad_immediates (uint8x16_t n, uint8x16_t m, int someval,
				mve_pred16_t pred)
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

  accum += __arm_vcx1q_m (1, accum, 4094, pred);       /* { dg-error {coprocessor 1 is not enabled with \+cdecp1} } */
  accum += __arm_vcx1qa_m (1, accum, 4095, pred);      /* { dg-error {coprocessor 1 is not enabled with \+cdecp1} } */
  accum += __arm_vcx2q_m (1, accum, n, 126, pred);     /* { dg-error {coprocessor 1 is not enabled with \+cdecp1} } */
  accum += __arm_vcx2qa_m (1, accum, n, 127, pred);    /* { dg-error {coprocessor 1 is not enabled with \+cdecp1} } */
  accum += __arm_vcx3q_m (1, accum, n, m, 15, pred);   /* { dg-error {coprocessor 1 is not enabled with \+cdecp1} } */
  accum += __arm_vcx3qa_m (1, accum, n, m, 15, pred);  /* { dg-error {coprocessor 1 is not enabled with \+cdecp1} } */

  /* `coproc' out of range.  */
  accum += __arm_vcx1q_u8 (8, 4095);           /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_vcx1qa (8, accum, 4095);      /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_vcx2q (8, n, 126);            /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_vcx2q_u8 (8, n, 127);         /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_vcx2qa (8, accum, n, 127);    /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_vcx3q_u8 (8, n, m, 14);       /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_vcx3q (8, n, m, 15);          /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_vcx3qa (8, accum, n, m, 15);  /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */

  accum += __arm_vcx1q_m (8, accum, 4094, pred);       /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_vcx1qa_m (8, accum, 4095, pred);      /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_vcx2q_m (8, accum, n, 126, pred);     /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_vcx2qa_m (8, accum, n, 127, pred);    /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_vcx3q_m (8, accum, n, m, 15, pred);   /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_vcx3qa_m (8, accum, n, m, 15, pred);  /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */

  /* `imm' out of range.  */
  accum += __arm_vcx1q_u8 (0, 4096);           /* { dg-error {argument 2 to '__builtin_arm_vcx1qv16qi' must be a constant immediate in range \[0-4095\]} } */
  accum += __arm_vcx1qa (0, accum, 4096);      /* { dg-error {argument 3 to '__builtin_arm_vcx1qav16qi' must be a constant immediate in range \[0-4095\]} } */
  accum += __arm_vcx2q (0, n, 128);            /* { dg-error {argument 3 to '__builtin_arm_vcx2qv16qi' must be a constant immediate in range \[0-127\]} } */
  accum += __arm_vcx2q_u8 (0, n, 129);         /* { dg-error {argument 3 to '__builtin_arm_vcx2qv16qi' must be a constant immediate in range \[0-127\]} } */
  accum += __arm_vcx2qa (0, accum, n, 128);    /* { dg-error {argument 4 to '__builtin_arm_vcx2qav16qi' must be a constant immediate in range \[0-127\]} } */
  accum += __arm_vcx3q_u8 (0, n, m, 16);       /* { dg-error {argument 4 to '__builtin_arm_vcx3qv16qi' must be a constant immediate in range \[0-15\]} } */
  accum += __arm_vcx3q (0, n, m, 17);          /* { dg-error {argument 4 to '__builtin_arm_vcx3qv16qi' must be a constant immediate in range \[0-15\]} } */
  accum += __arm_vcx3qa (0, accum, n, m, 16);  /* { dg-error {argument 5 to '__builtin_arm_vcx3qav16qi' must be a constant immediate in range \[0-15\]} } */

  accum += __arm_vcx1q_m (0, accum, 4097, pred);      /* { dg-error {argument 3 to '__builtin_arm_vcx1q_p_v16qi' must be a constant immediate in range \[0-4095\]} } */
  accum += __arm_vcx1qa_m (0, accum, 4096, pred);     /* { dg-error {argument 3 to '__builtin_arm_vcx1qa_p_v16qi' must be a constant immediate in range \[0-4095\]} } */
  accum += __arm_vcx2q_m (0, accum, n, 128, pred);    /* { dg-error {argument 4 to '__builtin_arm_vcx2q_p_v16qi' must be a constant immediate in range \[0-127\]} } */
  accum += __arm_vcx2qa_m (0, accum, n, 128, pred);   /* { dg-error {argument 4 to '__builtin_arm_vcx2qa_p_v16qi' must be a constant immediate in range \[0-127\]} } */
  accum += __arm_vcx3q_m (0, accum, n, m, 17, pred);  /* { dg-error {argument 5 to '__builtin_arm_vcx3q_p_v16qi' must be a constant immediate in range \[0-15\]} } */
  accum += __arm_vcx3qa_m (0, accum, n, m, 16, pred); /* { dg-error {argument 5 to '__builtin_arm_vcx3qa_p_v16qi' must be a constant immediate in range \[0-15\]} } */

  /* `imm' is not an immediate.  */
  accum += __arm_vcx1q_u8 (6, someval);             /* { dg-error {argument 2 to '__builtin_arm_vcx1qv16qi' must be a constant immediate in range \[0-4095\]} } */
  accum += __arm_vcx1qa (0, accum, someval);        /* { dg-error {argument 3 to '__builtin_arm_vcx1qav16qi' must be a constant immediate in range \[0-4095\]} } */
  accum += __arm_vcx2q (0, n, someval);             /* { dg-error {argument 3 to '__builtin_arm_vcx2qv16qi' must be a constant immediate in range \[0-127\]} } */
  accum += __arm_vcx2q_u8 (6, n, someval);          /* { dg-error {argument 3 to '__builtin_arm_vcx2qv16qi' must be a constant immediate in range \[0-127\]} } */
  accum += __arm_vcx2qa (0, accum, n, someval);     /* { dg-error {argument 4 to '__builtin_arm_vcx2qav16qi' must be a constant immediate in range \[0-127\]} } */
  accum += __arm_vcx3q_u8 (0, n, m, someval);       /* { dg-error {argument 4 to '__builtin_arm_vcx3qv16qi' must be a constant immediate in range \[0-15\]} } */
  accum += __arm_vcx3q (6, n, m, someval);          /* { dg-error {argument 4 to '__builtin_arm_vcx3qv16qi' must be a constant immediate in range \[0-15\]} } */
  accum += __arm_vcx3qa (0, accum, n, m, someval);  /* { dg-error {argument 5 to '__builtin_arm_vcx3qav16qi' must be a constant immediate in range \[0-15\]} } */

  accum += __arm_vcx1q_m (6, accum, someval, pred);        /* { dg-error {argument 3 to '__builtin_arm_vcx1q_p_v16qi' must be a constant immediate in range \[0-4095\]} } */
  accum += __arm_vcx1qa_m (0, accum, someval, pred);       /* { dg-error {argument 3 to '__builtin_arm_vcx1qa_p_v16qi' must be a constant immediate in range \[0-4095\]} } */
  accum += __arm_vcx2q_m (0, accum, n, someval, pred);     /* { dg-error {argument 4 to '__builtin_arm_vcx2q_p_v16qi' must be a constant immediate in range \[0-127\]} } */
  accum += __arm_vcx2qa_m (0, accum, n, someval, pred);    /* { dg-error {argument 4 to '__builtin_arm_vcx2qa_p_v16qi' must be a constant immediate in range \[0-127\]} } */
  accum += __arm_vcx3q_m (6, accum, n, m, someval, pred);  /* { dg-error {argument 5 to '__builtin_arm_vcx3q_p_v16qi' must be a constant immediate in range \[0-15\]} } */
  accum += __arm_vcx3qa_m (0, accum, n, m, someval, pred); /* { dg-error {argument 5 to '__builtin_arm_vcx3qa_p_v16qi' must be a constant immediate in range \[0-15\]} } */

  /* `coproc' is not an immediate.  */
  accum += __arm_vcx1q_u8 (someval, 4095);           /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_vcx1qa (someval, accum, 4095);      /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_vcx2q (someval, n, 126);            /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_vcx2q_u8 (someval, n, 127);         /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_vcx2qa (someval, accum, n, 127);    /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_vcx3q_u8 (someval, n, m, 14);       /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_vcx3q (someval, n, m, 15);          /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_vcx3qa (someval, accum, n, m, 15);  /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */

  accum += __arm_vcx1q_m (someval, accum, 4096, pred);      /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_vcx1qa_m (someval, accum, 4095, pred);     /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_vcx2q_m (someval, accum, n, 126, pred);    /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_vcx2qa_m (someval, accum, n, 127, pred);   /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_vcx3q_m (someval, accum, n, m, 15, pred);  /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_vcx3qa_m (someval, accum, n, m, 15, pred); /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */

  /* `imm' is of wrong type.  */
  accum += __arm_vcx1q_u8 (0, "");                    /* { dg-error {argument 2 to '__builtin_arm_vcx1qv16qi' must be a constant immediate in range \[0-4095\]} } */
  /* { dg-warning {passing argument 2 of '__builtin_arm_vcx1qv16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 117 } */
  accum += __arm_vcx1qa (0, accum, "");               /* { dg-error {argument 3 to '__builtin_arm_vcx1qav16qi' must be a constant immediate in range \[0-4095\]} } */
  /* { dg-warning {passing argument 3 of '__builtin_arm_vcx1qav16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 119 } */
  accum += __arm_vcx2q (0, n, "");                    /* { dg-error {argument 3 to '__builtin_arm_vcx2qv16qi' must be a constant immediate in range \[0-127\]} } */
  /* { dg-warning {passing argument 3 of '__builtin_arm_vcx2qv16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 121 } */
  accum += __arm_vcx2q_u8 (0, n, "x");                /* { dg-error {argument 3 to '__builtin_arm_vcx2qv16qi' must be a constant immediate in range \[0-127\]} } */
  /* { dg-warning {passing argument 3 of '__builtin_arm_vcx2qv16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 123 } */
  accum += __arm_vcx2qa (0, accum, n, "");            /* { dg-error {argument 4 to '__builtin_arm_vcx2qav16qi' must be a constant immediate in range \[0-127\]} } */
  /* { dg-warning {passing argument 4 of '__builtin_arm_vcx2qav16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 125 } */
  accum += __arm_vcx3q_u8 (0, n, m, "");              /* { dg-error {argument 4 to '__builtin_arm_vcx3qv16qi' must be a constant immediate in range \[0-15\]} } */
  /* { dg-warning {passing argument 4 of '__builtin_arm_vcx3qv16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 127 } */
  accum += __arm_vcx3q (0, n, m, "x");                /* { dg-error {argument 4 to '__builtin_arm_vcx3qv16qi' must be a constant immediate in range \[0-15\]} } */
  /* { dg-warning {passing argument 4 of '__builtin_arm_vcx3qv16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 129 } */
  accum += __arm_vcx3qa (0, accum, n, m, "");         /* { dg-error {argument 5 to '__builtin_arm_vcx3qav16qi' must be a constant immediate in range \[0-15\]} } */
  /* { dg-warning {passing argument 5 of '__builtin_arm_vcx3qav16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 131 } */

  accum += __arm_vcx1q_m (0, accum, "", pred);        /* { dg-error {argument 3 to '__builtin_arm_vcx1q_p_v16qi' must be a constant immediate in range \[0-4095\]} } */
  /* { dg-warning {passing argument 3 of '__builtin_arm_vcx1q_p_v16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 134 } */
  accum += __arm_vcx1qa_m (0, accum, "", pred);       /* { dg-error {argument 3 to '__builtin_arm_vcx1qa_p_v16qi' must be a constant immediate in range \[0-4095\]} } */
  /* { dg-warning {passing argument 3 of '__builtin_arm_vcx1qa_p_v16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 136 } */
  accum += __arm_vcx2q_m (0, accum, n, "", pred);     /* { dg-error {argument 4 to '__builtin_arm_vcx2q_p_v16qi' must be a constant immediate in range \[0-127\]} } */
  /* { dg-warning {passing argument 4 of '__builtin_arm_vcx2q_p_v16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 138 } */
  accum += __arm_vcx2qa_m (0, accum, n, "", pred);    /* { dg-error {argument 4 to '__builtin_arm_vcx2qa_p_v16qi' must be a constant immediate in range \[0-127\]} } */
  /* { dg-warning {passing argument 4 of '__builtin_arm_vcx2qa_p_v16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 140 } */
  accum += __arm_vcx3q_m (0, accum, n, m, "x", pred); /* { dg-error {argument 5 to '__builtin_arm_vcx3q_p_v16qi' must be a constant immediate in range \[0-15\]} } */
  /* { dg-warning {passing argument 5 of '__builtin_arm_vcx3q_p_v16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 142 } */
  accum += __arm_vcx3qa_m (0, accum, n, m, "", pred); /* { dg-error {argument 5 to '__builtin_arm_vcx3qa_p_v16qi' must be a constant immediate in range \[0-15\]} } */
  /* { dg-warning {passing argument 5 of '__builtin_arm_vcx3qa_p_v16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 144 } */

  /* `coproc' is of wrong type.  */
  accum += __arm_vcx1qa ("", accum, 4095);      /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  /* { dg-warning {passing argument 1 of '__builtin_arm_vcx1qav16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 148 } */
  accum += __arm_vcx2q ("", n, 126);            /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  /* { dg-warning {passing argument 1 of '__builtin_arm_vcx2qv16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 150 } */
  accum += __arm_vcx2qa ("", accum, n, 127);    /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  /* { dg-warning {passing argument 1 of '__builtin_arm_vcx2qav16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 152 } */
  accum += __arm_vcx3q ("", n, m, 15);          /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  /* { dg-warning {passing argument 1 of '__builtin_arm_vcx3qv16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 154 } */
  accum += __arm_vcx3qa ("", accum, n, m, 15);  /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  /* { dg-warning {passing argument 1 of '__builtin_arm_vcx3qav16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 156 } */

  accum += __arm_vcx1q_m ("", accum, 4094, pred);      /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  /* { dg-warning {passing argument 1 of '__builtin_arm_vcx1q_p_v16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 159 } */
  accum += __arm_vcx1qa_m ("", accum, 4095, pred);     /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  /* { dg-warning {passing argument 1 of '__builtin_arm_vcx1qa_p_v16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 161 } */
  accum += __arm_vcx2q_m ("", accum, n, 126, pred);    /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  /* { dg-warning {passing argument 1 of '__builtin_arm_vcx2q_p_v16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 163 } */
  accum += __arm_vcx2qa_m ("", accum, n, 127, pred);   /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  /* { dg-warning {passing argument 1 of '__builtin_arm_vcx2qa_p_v16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 165 } */
  accum += __arm_vcx3q_m ("", accum, n, m, 15, pred);  /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  /* { dg-warning {passing argument 1 of '__builtin_arm_vcx3q_p_v16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 167 } */
  accum += __arm_vcx3qa_m ("", accum, n, m, 15, pred); /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  /* { dg-warning {passing argument 1 of '__builtin_arm_vcx3qa_p_v16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 169 } */

  /* `pred" is of wrong type.  */
  accum += __arm_vcx1q_m (0, accum, 4094, "");
  /* { dg-warning {passing argument 4 of '__builtin_arm_vcx1q_p_v16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 173 } */
  accum += __arm_vcx1qa_m (0, accum, 4095, "");
  /* { dg-warning {passing argument 4 of '__builtin_arm_vcx1qa_p_v16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 175 } */
  accum += __arm_vcx2q_m (0, accum, n, 126, "");
  /* { dg-warning {passing argument 5 of '__builtin_arm_vcx2q_p_v16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 177 } */
  accum += __arm_vcx2qa_m (0, accum, n, 127, "");
  /* { dg-warning {passing argument 5 of '__builtin_arm_vcx2qa_p_v16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 179 } */
  accum += __arm_vcx3q_m (0, accum, n, m, 15, "");
  /* { dg-warning {passing argument 6 of '__builtin_arm_vcx3q_p_v16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 181 } */
  accum += __arm_vcx3qa_m (0, accum, n, m, 15, "");
  /* { dg-warning {passing argument 6 of '__builtin_arm_vcx3qa_p_v16qi' makes integer from pointer without a cast \[-Wint-conversion\]} "" { target *-*-* } 183 } */

  return accum;
}
