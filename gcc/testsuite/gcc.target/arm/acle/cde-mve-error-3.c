#include "arm_cde.h"

/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_1m_main_cde_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_main_cde_mve_fp } */

uint32_t test (int8x16_t m, int8x16_t n)
{
  /* Bad types for polymophic arguments.  */
  uint32_t accum = 0, n_int = 0;
  accum += __arm_vcx1qa (0, accum, 4095);
  accum += __arm_vcx2q (0, n_int, 126);
  accum += __arm_vcx2q_u8 (0, n_int, 127);
  accum += __arm_vcx2qa (0, accum, n, 127);
  accum += __arm_vcx3q_u8 (0, n_int, m, 14);
  accum += __arm_vcx3q (0, n_int, m, 15);
  accum += __arm_vcx3qa (0, accum, n, m, 15);

  /* { dg-error {argument 1 to function '__builtin_arm_vcx1qav16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 11 } */
  /* { dg-error {incompatible type for argument 2 of '__builtin_arm_vcx1qav16qi'} "" { target *-*-* } 11 } */
  /* { dg-error {argument 1 to function '__builtin_arm_vcx2qv16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 12 } */
  /* { dg-error {incompatible type for argument 2 of '__builtin_arm_vcx2qv16qi'} "" { target *-*-* } 12 } */
  /* { dg-error {argument 1 to function '__builtin_arm_vcx2qv16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 13 } */
  /* { dg-error {incompatible type for argument 2 of '__builtin_arm_vcx2qv16qi'} "" { target *-*-* } 13 } */
  /* { dg-error {argument 1 to function '__builtin_arm_vcx2qav16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 14 } */
  /* { dg-error {incompatible type for argument 2 of '__builtin_arm_vcx2qav16qi'} "" { target *-*-* } 14 } */
  /* { dg-error {argument 1 to function '__builtin_arm_vcx3qv16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 15 } */
  /* { dg-error {incompatible type for argument 2 of '__builtin_arm_vcx3qv16qi'} "" { target *-*-* } 15 } */
  /* { dg-error {argument 1 to function '__builtin_arm_vcx3qv16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 16 } */
  /* { dg-error {incompatible type for argument 2 of '__builtin_arm_vcx3qv16qi'} "" { target *-*-* } 16 } */
  /* { dg-error {argument 1 to function '__builtin_arm_vcx3qav16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 17 } */
  /* { dg-error {incompatible type for argument 2 of '__builtin_arm_vcx3qav16qi'} "" { target *-*-* } 17 } */
  return accum;
}

int8x16_t test2 (int8x16_t m, int8x16_t n)
{
  uint32_t n_int = 0, m_int = 0;
  int8x16_t accum = (int8x16_t)(uint64x2_t) { 0, 0 };
  accum += __arm_vcx2qa (0, accum, n_int, 127);
  accum += __arm_vcx3q_u8 (0, n, m_int, 14);
  accum += __arm_vcx3q (0, n, m_int, 15);
  accum += __arm_vcx3qa (0, accum, n_int, m, 15);
  accum += __arm_vcx3qa (0, accum, n_int, m, 15);
  accum += __arm_vcx3qa (0, accum, n, m_int, 15);
  accum += __arm_vcx3qa (0, accum, n, m_int, 15);

  /* { dg-error {argument 2 to function '__builtin_arm_vcx2qav16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 40 } */
  /* { dg-error {incompatible type for argument 3 of '__builtin_arm_vcx2qav16qi'} "" { target *-*-* } 40 } */
  /* { dg-error {argument 2 to function '__builtin_arm_vcx3qv16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 41 } */
  /* { dg-error {incompatible type for argument 3 of '__builtin_arm_vcx3qv16qi'} "" { target *-*-* } 41 } */
  /* { dg-error {argument 2 to function '__builtin_arm_vcx3qv16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 42 } */
  /* { dg-error {incompatible type for argument 3 of '__builtin_arm_vcx3qv16qi'} "" { target *-*-* } 42 } */
  /* { dg-error {argument 2 to function '__builtin_arm_vcx3qav16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 43 } */
  /* { dg-error {incompatible type for argument 3 of '__builtin_arm_vcx3qav16qi'} "" { target *-*-* } 43 } */
  /* { dg-error {argument 2 to function '__builtin_arm_vcx3qav16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 44 } */
  /* { dg-error {incompatible type for argument 3 of '__builtin_arm_vcx3qav16qi'} "" { target *-*-* } 44 } */
  /* { dg-error {argument 3 to function '__builtin_arm_vcx3qav16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 45 } */
  /* { dg-error {incompatible type for argument 4 of '__builtin_arm_vcx3qav16qi'} "" { target *-*-* } 45 } */
  /* { dg-error {argument 3 to function '__builtin_arm_vcx3qav16qi' is of type 'uint32_t' {aka '(?:long )?unsigned int'} which is not known to be 128 bits wide} "" { target *-*-* } 46 } */
  /* { dg-error {incompatible type for argument 4 of '__builtin_arm_vcx3qav16qi'} "" { target *-*-* } 46 } */
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

  /* { dg-error {'accum_int' undeclared \(first use in this function\)} "" { target *-*-* } 70 } */
  /* { dg-error {'n_int' undeclared \(first use in this function\)} "" { target *-*-* } 71 } */
  /* { dg-error {'m_int' undeclared \(first use in this function\)} "" { target *-*-* } 75 } */
  return accum;
}
