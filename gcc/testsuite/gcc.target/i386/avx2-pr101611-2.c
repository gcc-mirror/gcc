/* PR target/101611 */
/* { dg-do run } */
/* { dg-options "-O2 -mavx2 -mno-avx512f" } */
/* { dg-require-effective-target avx2 } */

#include "avx2-check.h"

typedef long long V __attribute__((vector_size(32)));
typedef long long W __attribute__((vector_size(16)));

__attribute__((noipa)) V
foo (V a, V b)
{
  return a >> b;
}

__attribute__((noipa)) W
bar (W a, W b)
{
  return a >> b;
}

static void
avx2_test (void)
{
  V a = { 0x7f123456789abcdeLL, -0x30edcba987654322LL,
	  -0x30edcba987654322LL, 0x7f123456789abcdeLL };
  V b = { 17, 11, 23, 0 };
  V c = foo (a, b);
  if (c[0] != 0x3f891a2b3c4dLL
      || c[1] != -0x61db97530eca9LL
      || c[2] != -0x61db97530fLL
      || c[3] != 0x7f123456789abcdeLL)
    abort ();
  W d = { 0x7f123456789abcdeLL, -0x30edcba987654322LL };
  W e = { 45, 27 };
  W f = bar (d, e);
  if (f[0] != 0x3f891LL
      || f[1] != -0x61db97531LL)
    abort ();
}
