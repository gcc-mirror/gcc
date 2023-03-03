/* Test the Custom Datapath Extension ACLE intrinsic.  */

/* This file is to check we catch incorrect uses of the ACLE.  */

/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8m_main_cde_ok } */
/* { dg-add-options arm_v8m_main_cde } */
/* { dg-additional-options "-save-temps" } */

#include "arm_cde.h"

/*
   These are the scalar intrinsics.
uint32_t __arm_cx1(int coproc, uint32_t imm);
uint32_t __arm_cx1a(int coproc, uint32_t acc, uint32_t imm);
uint32_t __arm_cx2(int coproc, uint32_t n, uint32_t imm);
uint32_t __arm_cx2a(int coproc, uint32_t acc, uint32_t n, uint32_t imm);
uint32_t __arm_cx3(int coproc, uint32_t n, uint32_t m, uint32_t imm);
uint32_t __arm_cx3a(int coproc, uint32_t acc, uint32_t n, uint32_t m, uint32_t imm);

uint64_t __arm_cx1d(int coproc, uint32_t imm);
uint64_t __arm_cx1da(int coproc, uint64_t acc, uint32_t imm);
uint64_t __arm_cx2d(int coproc, uint32_t n, uint32_t imm);
uint64_t __arm_cx2da(int coproc, uint64_t acc, uint32_t n, uint32_t imm);
uint64_t __arm_cx3d(int coproc, uint32_t n, uint32_t m, uint32_t imm);
uint64_t __arm_cx3da(int coproc, uint64_t acc, uint32_t n, uint32_t m, uint32_t imm);
*/

/* Incorrect types as the constants.  */
uint64_t test_cde (uint32_t n, uint32_t m)
{
  uint64_t accum = 0;

  /* `coproc` not enabled.  */
  accum += __arm_cx1   (7,                        0); /* { dg-error {coprocessor 7 is not enabled with \+cdecp7} } */
  accum += __arm_cx1a  (7, (uint32_t)accum,       0); /* { dg-error {coprocessor 7 is not enabled with \+cdecp7} } */
  accum += __arm_cx2   (7, n,                     0); /* { dg-error {coprocessor 7 is not enabled with \+cdecp7} } */
  accum += __arm_cx2a  (7, (uint32_t)accum, n,    0); /* { dg-error {coprocessor 7 is not enabled with \+cdecp7} } */
  accum += __arm_cx3   (7, n, m,                  0); /* { dg-error {coprocessor 7 is not enabled with \+cdecp7} } */
  accum += __arm_cx3a  (7, (uint32_t)accum, n, m, 0); /* { dg-error {coprocessor 7 is not enabled with \+cdecp7} } */

  accum += __arm_cx1d  (7,                        0); /* { dg-error {coprocessor 7 is not enabled with \+cdecp7} } */
  accum += __arm_cx1da (7, accum,                 0); /* { dg-error {coprocessor 7 is not enabled with \+cdecp7} } */
  accum += __arm_cx2d  (7, n,                     0); /* { dg-error {coprocessor 7 is not enabled with \+cdecp7} } */
  accum += __arm_cx2da (7, accum, n,              0); /* { dg-error {coprocessor 7 is not enabled with \+cdecp7} } */
  accum += __arm_cx3d  (7, n, m,                  0); /* { dg-error {coprocessor 7 is not enabled with \+cdecp7} } */
  accum += __arm_cx3da (7, accum, n, m,           0); /* { dg-error {coprocessor 7 is not enabled with \+cdecp7} } */

  /* `coproc` out of range.  */
  accum += __arm_cx1   (8,                        0); /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_cx1a  (8, (uint32_t)accum,       0); /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_cx2   (8, n,                     0); /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_cx2a  (8, (uint32_t)accum, n,    0); /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_cx3   (8, n, m,                  0); /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_cx3a  (8, (uint32_t)accum, n, m, 0); /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */

  accum += __arm_cx1d  (8,                        0); /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_cx1da (8, accum,                 0); /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_cx2d  (8, n,                     0); /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_cx2da (8, accum, n,              0); /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_cx3d  (8, n, m,                  0); /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_cx3da (8, accum, n, m,           0); /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */

  /* `imm` out of range.  */
  accum += __arm_cx1   (0,                        8192); /* { dg-error {argument 2 to '__builtin_arm_cx1si' must be a constant immediate in range \[0-8191\]} } */
  accum += __arm_cx1a  (0, (uint32_t)accum,       8192); /* { dg-error {argument 3 to '__builtin_arm_cx1asi' must be a constant immediate in range \[0-8191\]} } */
  accum += __arm_cx2   (0, n,                     512);  /* { dg-error {argument 3 to '__builtin_arm_cx2si' must be a constant immediate in range \[0-511\]} } */
  accum += __arm_cx2a  (0, (uint32_t)accum, n,    512);  /* { dg-error {argument 4 to '__builtin_arm_cx2asi' must be a constant immediate in range \[0-511\]} } */
  accum += __arm_cx3   (0, n, m,                  64);   /* { dg-error {argument 4 to '__builtin_arm_cx3si' must be a constant immediate in range \[0-63\]} } */
  accum += __arm_cx3a  (0, (uint32_t)accum, n, m, 64);   /* { dg-error {argument 5 to '__builtin_arm_cx3asi' must be a constant immediate in range \[0-63\]} } */

  accum += __arm_cx1d  (0,                        8192); /* { dg-error {argument 2 to '__builtin_arm_cx1di' must be a constant immediate in range \[0-8191\]} } */
  accum += __arm_cx1da (0, accum,                 8192); /* { dg-error {argument 3 to '__builtin_arm_cx1adi' must be a constant immediate in range \[0-8191\]} } */
  accum += __arm_cx2d  (0, n,                     512);  /* { dg-error {argument 3 to '__builtin_arm_cx2di' must be a constant immediate in range \[0-511\]} } */
  accum += __arm_cx2da (0, accum, n,              512);  /* { dg-error {argument 4 to '__builtin_arm_cx2adi' must be a constant immediate in range \[0-511\]} } */
  accum += __arm_cx3d  (0, n, m,                  64);   /* { dg-error {argument 4 to '__builtin_arm_cx3di' must be a constant immediate in range \[0-63\]} } */
  accum += __arm_cx3da (0, accum, n, m,           64);   /* { dg-error {argument 5 to '__builtin_arm_cx3adi' must be a constant immediate in range \[0-63\]} } */

  /* `imm` must be an immediate.  */
  accum += __arm_cx1   (0,                        n);    /* { dg-error {argument 2 to '__builtin_arm_cx1si' must be a constant immediate in range \[0-8191\]} } */
  accum += __arm_cx1a  (0, (uint32_t)accum,       n);    /* { dg-error {argument 3 to '__builtin_arm_cx1asi' must be a constant immediate in range \[0-8191\]} } */
  accum += __arm_cx2   (0, n,                     n);    /* { dg-error {argument 3 to '__builtin_arm_cx2si' must be a constant immediate in range \[0-511\]} } */
  accum += __arm_cx2a  (0, (uint32_t)accum, n,    n);    /* { dg-error {argument 4 to '__builtin_arm_cx2asi' must be a constant immediate in range \[0-511\]} } */
  accum += __arm_cx3   (0, n, m,                  n);    /* { dg-error {argument 4 to '__builtin_arm_cx3si' must be a constant immediate in range \[0-63\]} } */
  accum += __arm_cx3a  (0, (uint32_t)accum, n, m, n);    /* { dg-error {argument 5 to '__builtin_arm_cx3asi' must be a constant immediate in range \[0-63\]} } */

  accum += __arm_cx1d  (0,                        n);    /* { dg-error {argument 2 to '__builtin_arm_cx1di' must be a constant immediate in range \[0-8191\]} } */
  accum += __arm_cx1da (0, accum,                 n);    /* { dg-error {argument 3 to '__builtin_arm_cx1adi' must be a constant immediate in range \[0-8191\]} } */
  accum += __arm_cx2d  (0, n,                     n);    /* { dg-error {argument 3 to '__builtin_arm_cx2di' must be a constant immediate in range \[0-511\]} } */
  accum += __arm_cx2da (0, accum, n,              n);    /* { dg-error {argument 4 to '__builtin_arm_cx2adi' must be a constant immediate in range \[0-511\]} } */
  accum += __arm_cx3d  (0, n, m,                  n);    /* { dg-error {argument 4 to '__builtin_arm_cx3di' must be a constant immediate in range \[0-63\]} } */
  accum += __arm_cx3da (0, accum, n, m,           n);    /* { dg-error {argument 5 to '__builtin_arm_cx3adi' must be a constant immediate in range \[0-63\]} } */

  /* `coproc` is not an immediate.  */
  accum += __arm_cx1   ((int)m,                        0); /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_cx1a  ((int)m, (uint32_t)accum,       0); /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_cx2   ((int)m, n,                     0); /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_cx2a  ((int)m, (uint32_t)accum, n,    0); /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_cx3   ((int)m, n, m,                  0); /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_cx3a  ((int)m, (uint32_t)accum, n, m, 0); /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */

  accum += __arm_cx1d  ((int)m,                        0); /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_cx1da ((int)m, accum,                 0); /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_cx2d  ((int)m, n,                     0); /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_cx2da ((int)m, accum, n,              0); /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_cx3d  ((int)m, n, m,                  0); /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */
  accum += __arm_cx3da ((int)m, accum, n, m,           0); /* { dg-error {coproc must be a constant immediate in range \[0-7\] enabled with .\+cdecp<N>.} } */

  return accum;
}

