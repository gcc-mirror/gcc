/* { dg-do run } */
/* { dg-options "-O1 -fdump-tree-vect-details -fdump-tree-slp-details -fdump-tree-optimized" } */

extern void abort (void);
static void do_test (void);

#define DO_TEST do_test
#define AVX512BF16
#include "avx512-check.h"

__bf16 b_32[2], r_abs_32[2], r_neg_32[2];
__bf16 b_64[4], r_abs_64[4], r_neg_64[4];

void
__attribute__((optimize ("O2"), noinline, noipa, noclone, no_icf))
abs_32 (void)
{
  for (int i = 0; i < 2; i++)
    r_abs_32[i] = __builtin_fabsf16 (b_32[i]);
}

void
__attribute__((optimize ("O2"), noinline, noipa, noclone, no_icf))
neg_32 (void)
{
  for (int i = 0; i < 2; i++)
    r_neg_32[i] = -b_32[i];
}

void
__attribute__((optimize ("O2"), noinline, noipa, noclone, no_icf))
abs_64 (void)
{
  for (int i = 0; i < 4; i++)
    r_abs_64[i] = __builtin_fabsf16 (b_64[i]);
}

void
__attribute__((optimize ("O2"), noinline, noipa, noclone, no_icf))
neg_64 (void)
{
  for (int i = 0; i < 4; i++)
    r_neg_64[i] = -b_64[i];
}

void
check_absneg_results (__bf16 *b, __bf16 *r_abs, __bf16 *r_neg, int len)
{
  for (int i = 0; i < len; i++)
    {
      __bf16 expected_abs = __builtin_fabsf16 (b[i]);
      __bf16 expected_neg = -b[i];
      if (r_abs[i] != expected_abs || r_neg[i] != expected_neg)
        abort ();
    }
}

static void
__attribute__ ((noinline, noclone))
do_test (void)
{
  float float_b[16] = {-1.2f, 3.4f, -5.6f, 7.8f};

  for (int i = 0; i < 2; i++)
    b_32[i] = (__bf16) float_b[i];

  for (int i = 0; i < 4; i++)
    b_64[i] = (__bf16) float_b[i];

  abs_32 ();
  neg_32 ();
  check_absneg_results (b_32, r_abs_32, r_neg_32, 2);

  abs_64 ();
  neg_64 ();
  check_absneg_results (b_64, r_abs_64, r_neg_64, 4);
}

/* { dg-final { scan-tree-dump-times "vectorized using 4 byte vectors" 2 "slp1" } } */
/* { dg-final { scan-tree-dump-times "loop vectorized using 8 byte vectors" 2 "vect" { target { ! ia32 } } } } */
/* { dg-final { scan-tree-dump-times {(?n)ABS_EXPR <vect} 2 "optimized" { target { ! ia32 } } } } */
