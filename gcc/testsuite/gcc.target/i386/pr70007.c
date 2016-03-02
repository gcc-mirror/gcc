/* PR rtl-optimization/70007 */
/* { dg-do run { target bmi2 } } */
/* { dg-options "-O -fgcse -mbmi2 -Wno-psabi" } */
/* { dg-require-effective-target int128 } */

#include "bmi2-check.h"

typedef unsigned short v32u16 __attribute__ ((vector_size (32)));
typedef unsigned long long v32u64 __attribute__ ((vector_size (32)));
typedef unsigned __int128 u128;
typedef unsigned __int128 v32u128 __attribute__ ((vector_size (32)));

u128
foo (v32u16 v32u16_0, v32u64 v32u64_0, v32u64 v32u64_1)
{
  do {
    v32u16_0[13] |= v32u64_1[3] = (v32u64_1[3] >> 19) | (v32u64_1[3] << 45);
    v32u64_1 %= ~v32u64_1;
    v32u64_0 *= (v32u64) v32u16_0;
  } while (v32u64_0[0]);
  return v32u64_1[3];
}

static void
bmi2_test ()
{
  u128 x = foo((v32u16){(unsigned short) ~0xba31, 0x47c6}, (v32u64){64}, (v32u64){0, 0x8b217e2514d23242, 0xac569b6dff9f82, 0x9d4cffe03c139c});
  if (x != 0x3c74da5ca328d09)
    __builtin_abort();
}
