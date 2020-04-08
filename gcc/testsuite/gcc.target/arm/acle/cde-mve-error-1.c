#include "arm_cde.h"

/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_1m_main_cde_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_main_cde_mve_fp } */

/* Ensure the error messages make sense when passing too many/too few arguments
   to the intrinsic user-facing functions.  */
uint8x16_t test_invalid_arguments (uint8x16_t n, uint8x16_t m)
{
  uint8x16_t accum = __arm_vcx1q_u8 (0, 33, 1);   /* { dg-error {macro "__arm_vcx1q_u8" passed 3 arguments, but takes just 2} } */
  accum += __arm_vcx1qa (0, accum, 33, 1);        /* { dg-error {macro "__arm_vcx1qa" passed 4 arguments, but takes just 3} } */
  accum += __arm_vcx2q_u8 (0, n, 33, 1);          /* { dg-error {macro "__arm_vcx2q_u8" passed 4 arguments, but takes just 3} } */
  accum += __arm_vcx2q (0, n, 33, 1);             /* { dg-error {macro "__arm_vcx2q" passed 4 arguments, but takes just 3} } */
  accum += __arm_vcx2qa (0, accum, n, 33, 1);     /* { dg-error {macro "__arm_vcx2qa" passed 5 arguments, but takes just 4} } */
  accum += __arm_vcx3q_u8 (0, n, m, 33, 1);       /* { dg-error {macro "__arm_vcx3q_u8" passed 5 arguments, but takes just 4} } */
  accum += __arm_vcx3q (0, n, m, 33, 1);          /* { dg-error {macro "__arm_vcx3q" passed 5 arguments, but takes just 4} } */
  accum += __arm_vcx3qa (0, accum, n, m, 33, 1);  /* { dg-error {macro "__arm_vcx3qa" passed 6 arguments, but takes just 5} } */
  accum += __arm_vcx1q_u8 (0);                    /* { dg-error {macro "__arm_vcx1q_u8" requires 2 arguments, but only 1 given} } */
  accum += __arm_vcx1qa (0, accum);               /* { dg-error {macro "__arm_vcx1qa" requires 3 arguments, but only 2 given} } */
  accum += __arm_vcx2q_u8 (0, n);                 /* { dg-error {macro "__arm_vcx2q_u8" requires 3 arguments, but only 2 given} } */
  accum += __arm_vcx2q (0, n);                    /* { dg-error {macro "__arm_vcx2q" requires 3 arguments, but only 2 given} } */
  accum += __arm_vcx2qa (0, accum, n);            /* { dg-error {macro "__arm_vcx2qa" requires 4 arguments, but only 3 given} } */
  accum += __arm_vcx3q_u8 (0, n, m);              /* { dg-error {macro "__arm_vcx3q_u8" requires 4 arguments, but only 3 given} } */
  accum += __arm_vcx3q (0, n, m);                 /* { dg-error {macro "__arm_vcx3q" requires 4 arguments, but only 3 given} } */
  accum += __arm_vcx3qa (0, accum, n, m);         /* { dg-error {macro "__arm_vcx3qa" requires 5 arguments, but only 4 given} } */

  accum += __arm_vcx1q_m (0, accum, 33, 1, 4);         /* { dg-error {macro "__arm_vcx1q_m" passed 5 arguments, but takes just 4} } */
  accum += __arm_vcx1qa_m (0, accum, 33, 1, 4);        /* { dg-error {macro "__arm_vcx1qa_m" passed 5 arguments, but takes just 4} } */
  accum += __arm_vcx2q_m (0, accum, n, 33, 1, 4);      /* { dg-error {macro "__arm_vcx2q_m" passed 6 arguments, but takes just 5} } */
  accum += __arm_vcx2qa_m (0, accum, n, 33, 1, 4);     /* { dg-error {macro "__arm_vcx2qa_m" passed 6 arguments, but takes just 5} } */
  accum += __arm_vcx3q_m (0, accum, n, m, 33, 1, 4);   /* { dg-error {macro "__arm_vcx3q_m" passed 7 arguments, but takes just 6} } */
  accum += __arm_vcx3qa_m (0, accum, n, m, 33, 1, 4);  /* { dg-error {macro "__arm_vcx3qa_m" passed 7 arguments, but takes just 6} } */
  accum += __arm_vcx1q_m (0, accum, 4);                /* { dg-error {macro "__arm_vcx1q_m" requires 4 arguments, but only 3 given} } */
  accum += __arm_vcx1qa_m (0, accum, 4);               /* { dg-error {macro "__arm_vcx1qa_m" requires 4 arguments, but only 3 given} } */
  accum += __arm_vcx2q_m (0, accum, n, 4);             /* { dg-error {macro "__arm_vcx2q_m" requires 5 arguments, but only 4 given} } */
  accum += __arm_vcx2qa_m (0, accum, n, 4);            /* { dg-error {macro "__arm_vcx2qa_m" requires 5 arguments, but only 4 given} } */
  accum += __arm_vcx3q_m (0, accum, n, m, 4);          /* { dg-error {macro "__arm_vcx3q_m" requires 6 arguments, but only 5 given} } */
  accum += __arm_vcx3qa_m (0, accum, n, m, 4);         /* { dg-error {macro "__arm_vcx3qa_m" requires 6 arguments, but only 5 given} } */

  /* The preprocessor complains that the macro was given an invalid number of
     arguments, and because of that ends up not expanding the macro but
     rather just leaving the macro name in the source code.  That macro name
     results in these errors.  */
  /* { dg-error {'__arm_vcx1q_u8' undeclared \(first use in this function\)}  "" { target { *-*-* } } 11 } */
  /* { dg-error {'__arm_vcx1qa' undeclared \(first use in this function\)}  "" { target { *-*-* } } 12 } */
  /* { dg-error {'__arm_vcx2q_u8' undeclared \(first use in this function\)}  "" { target { *-*-* } } 13 } */
  /* { dg-error {'__arm_vcx2q' undeclared \(first use in this function\)}  "" { target { *-*-* } } 14 } */
  /* { dg-error {'__arm_vcx2qa' undeclared \(first use in this function\)}  "" { target { *-*-* } } 15 } */
  /* { dg-error {'__arm_vcx3q_u8' undeclared \(first use in this function\)}  "" { target { *-*-* } } 16 } */
  /* { dg-error {'__arm_vcx3q' undeclared \(first use in this function\)}  "" { target { *-*-* } } 17 } */
  /* { dg-error {'__arm_vcx3qa' undeclared \(first use in this function\)}  "" { target { *-*-* } } 18 } */

  /* { dg-error {'__arm_vcx1q_m' undeclared \(first use in this function\)} "" { target { *-*-* } } 28 } */
  /* { dg-error {'__arm_vcx1qa_m' undeclared \(first use in this function\)} "" { target { *-*-* } } 29 } */
  /* { dg-error {'__arm_vcx2q_m' undeclared \(first use in this function\)} "" { target { *-*-* } } 30 } */
  /* { dg-error {'__arm_vcx2qa_m' undeclared \(first use in this function\)} "" { target { *-*-* } } 31 } */
  /* { dg-error {'__arm_vcx3q_m' undeclared \(first use in this function\)} "" { target { *-*-* } } 32 } */
  /* { dg-error {'__arm_vcx3qa_m' undeclared \(first use in this function\)} "" { target { *-*-* } } 33 } */

  return accum;
}
