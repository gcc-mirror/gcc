#include "arm_cde.h"

/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_1m_main_cde_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_main_cde_mve_fp } */

uint32_t test (int8x16_t m, int8x16_t n, mve_pred16_t pred)
{
  /* Bad types for polymophic arguments.  */
  uint32_t accum = 0, n_int = 0, m_int = 0;
  accum += __arm_vcx1qa (0, accum, 4095);
  accum += __arm_vcx2q (0, n_int, 126);
  accum += __arm_vcx2q_u8 (0, n_int, 127);
  accum += __arm_vcx2qa (0, accum, n_int, 127);
  accum += __arm_vcx3q_u8 (0, n_int, m_int, 14);
  accum += __arm_vcx3q (0, n_int, m_int, 15);
  accum += __arm_vcx3qa (0, accum, n_int, m_int, 15);

  /* We get a at least two errors for each function since the errors are
     checked for validity in two different ways and both are caught.
     The resolver manually checks that each type is 128 bits wide, and only
     casts the value if that's true.
     After the resolver rejects the function call then the C frontend tries to
     fit the original call to the builtin.  This produces a second set of
     error messages from the C frontend argument checking.  */

  /* { dg-error {argument 2 to function '__builtin_arm_vcx1qav16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 11 } */
  /* { dg-error {incompatible type for argument 2 of '__builtin_arm_vcx1qav16qi'} "" { target *-*-* } 11 } */

  /* { dg-error {argument 2 to function '__builtin_arm_vcx2qv16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 12 } */
  /* { dg-error {incompatible type for argument 2 of '__builtin_arm_vcx2qv16qi'} "" { target *-*-* } 12 } */

  /* { dg-error {argument 2 to function '__builtin_arm_vcx2qv16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 13 } */
  /* { dg-error {incompatible type for argument 2 of '__builtin_arm_vcx2qv16qi'} "" { target *-*-* } 13 } */

  /* { dg-error {argument 2 to function '__builtin_arm_vcx2qav16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 14 } */
  /* { dg-error {incompatible type for argument 2 of '__builtin_arm_vcx2qav16qi'} "" { target *-*-* } 14 } */
  /* { dg-error {argument 3 to function '__builtin_arm_vcx2qav16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 14 } */
  /* { dg-error {incompatible type for argument 3 of '__builtin_arm_vcx2qav16qi'} "" { target *-*-* } 14 } */

  /* { dg-error {argument 2 to function '__builtin_arm_vcx3qv16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 15 } */
  /* { dg-error {incompatible type for argument 2 of '__builtin_arm_vcx3qv16qi'} "" { target *-*-* } 15 } */
  /* { dg-error {argument 3 to function '__builtin_arm_vcx3qv16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 15 } */
  /* { dg-error {incompatible type for argument 3 of '__builtin_arm_vcx3qv16qi'} "" { target *-*-* } 15 } */

  /* { dg-error {argument 2 to function '__builtin_arm_vcx3qv16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 16 } */
  /* { dg-error {incompatible type for argument 2 of '__builtin_arm_vcx3qv16qi'} "" { target *-*-* } 16 } */
  /* { dg-error {argument 3 to function '__builtin_arm_vcx3qv16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 16 } */
  /* { dg-error {incompatible type for argument 3 of '__builtin_arm_vcx3qv16qi'} "" { target *-*-* } 16 } */

  /* { dg-error {argument 2 to function '__builtin_arm_vcx3qav16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 17 } */
  /* { dg-error {incompatible type for argument 2 of '__builtin_arm_vcx3qav16qi'} "" { target *-*-* } 17 } */
  /* { dg-error {argument 3 to function '__builtin_arm_vcx3qav16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 17 } */
  /* { dg-error {incompatible type for argument 3 of '__builtin_arm_vcx3qav16qi'} "" { target *-*-* } 17 } */
  /* { dg-error {argument 4 to function '__builtin_arm_vcx3qav16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 17 } */
  /* { dg-error {incompatible type for argument 4 of '__builtin_arm_vcx3qav16qi'} "" { target *-*-* } 17 } */

  accum += __arm_vcx1qa_m (0, accum, 4095, pred);
  accum += __arm_vcx2q_m (0, accum, n_int, 126, pred);
  accum += __arm_vcx2qa_m (0, accum, n_int, 127, pred);
  accum += __arm_vcx3q_m (0, accum, n_int, m_int, 15, pred);
  accum += __arm_vcx3qa_m (0, accum, n_int, m_int, 15, pred);

  /* { dg-error {argument 2 to function '__builtin_arm_vcx1qa_p_v16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 58 } */
  /* { dg-error {incompatible type for argument 2 of '__builtin_arm_vcx1qa_p_v16qi'} "" { target *-*-* } 58 } */

  /* { dg-error {argument 2 to function '__builtin_arm_vcx2q_p_v16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 59 } */
  /* { dg-error {incompatible type for argument 2 of '__builtin_arm_vcx2q_p_v16qi'} "" { target *-*-* } 59 } */
  /* { dg-error {argument 3 to function '__builtin_arm_vcx2q_p_v16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 59 } */
  /* { dg-error {incompatible type for argument 3 of '__builtin_arm_vcx2q_p_v16qi'} "" { target *-*-* } 59 } */

  /* { dg-error {argument 2 to function '__builtin_arm_vcx2qa_p_v16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 60 } */
  /* { dg-error {incompatible type for argument 2 of '__builtin_arm_vcx2qa_p_v16qi'} "" { target *-*-* } 60 } */
  /* { dg-error {argument 3 to function '__builtin_arm_vcx2qa_p_v16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 60 } */
  /* { dg-error {incompatible type for argument 3 of '__builtin_arm_vcx2qa_p_v16qi'} "" { target *-*-* } 60 } */

  /* { dg-error {argument 2 to function '__builtin_arm_vcx3q_p_v16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 61 } */
  /* { dg-error {incompatible type for argument 2 of '__builtin_arm_vcx3q_p_v16qi'} "" { target *-*-* } 61 } */
  /* { dg-error {argument 3 to function '__builtin_arm_vcx3q_p_v16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 61 } */
  /* { dg-error {incompatible type for argument 3 of '__builtin_arm_vcx3q_p_v16qi'} "" { target *-*-* } 61 } */
  /* { dg-error {argument 4 to function '__builtin_arm_vcx3q_p_v16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 61 } */
  /* { dg-error {incompatible type for argument 4 of '__builtin_arm_vcx3q_p_v16qi'} "" { target *-*-* } 61 } */

  /* { dg-error {argument 2 to function '__builtin_arm_vcx3qa_p_v16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 62 } */
  /* { dg-error {incompatible type for argument 2 of '__builtin_arm_vcx3qa_p_v16qi'} "" { target *-*-* } 62 } */
  /* { dg-error {argument 3 to function '__builtin_arm_vcx3qa_p_v16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 62 } */
  /* { dg-error {incompatible type for argument 3 of '__builtin_arm_vcx3qa_p_v16qi'} "" { target *-*-* } 62 } */
  /* { dg-error {argument 4 to function '__builtin_arm_vcx3qa_p_v16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 62 } */
  /* { dg-error {incompatible type for argument 4 of '__builtin_arm_vcx3qa_p_v16qi'} "" { target *-*-* } 62 } */

  return accum;
}

/* Testing that undeclared variables work as expected.
   (This to verify we fixed a problem hit during development).  */
int8x16_t test3 (int8x16_t m, int8x16_t n)
{
  int8x16_t accum = (int8x16_t)(uint64x2_t) { 0, 0 };
  accum += __arm_vcx1qa (0, accum_int, 4095);
  accum += __arm_vcx2q (0, n_int, 126);
  accum += __arm_vcx2q_u8 (0, n_int, 127);
  accum += __arm_vcx2qa (0, accum, n_int, 127);
  accum += __arm_vcx3q_u8 (0, n_int, m, 14);
  accum += __arm_vcx3q_u8 (0, n, m_int, 14);
  accum += __arm_vcx3q (0, n_int, m, 15);
  accum += __arm_vcx3q (0, n, m_int, 15);
  accum += __arm_vcx3qa (0, accum, n_int, m, 15);
  accum += __arm_vcx3qa (0, accum, n_int, m_int, 15);

  /* { dg-error {'accum_int' undeclared \(first use in this function\)} "" { target *-*-* } 99 } */
  /* { dg-error {'n_int' undeclared \(first use in this function\)} "" { target *-*-* } 100 } */
  /* { dg-error {'m_int' undeclared \(first use in this function\)} "" { target *-*-* } 104 } */
  return accum;
}
