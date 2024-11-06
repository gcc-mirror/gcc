/* { dg-do run { target avx512f } } */
/* { dg-options "-O1 -mavx512bf16 -fdump-tree-vect-details -fdump-tree-optimized" } */

extern void abort (void);
static void do_test (void);

#define DO_TEST do_test
#define AVX512BF16
#include "avx512-check.h"

__bf16 b_512[32], r_abs_512[32], r_neg_512[32];

void
__attribute__((optimize ("O2"), noinline, noipa, noclone, no_icf,
target("prefer-vector-width=512")))
abs_512 (void)
{
  for (int i = 0; i < 32; i++)
    r_abs_512[i] = __builtin_fabsf16(b_512[i]);
}

void
__attribute__((optimize ("O2"), noinline, noipa, noclone, no_icf,
target("prefer-vector-width=512")))
neg_512 (void)
{
  for (int i = 0; i < 32; i++)
    r_neg_512[i] = -b_512[i];
}

void
check_absneg_results (__bf16 *b, __bf16 *r_abs, __bf16 *r_neg, int len)
{
  for (int i = 0; i < len; i++)
    {
      __bf16 expected_abs = __builtin_fabsf16(b[i]);
      __bf16 expected_neg = -b[i];
      if (r_abs[i] != expected_abs || r_neg[i] != expected_neg)
        abort ();
    }
}

static void
__attribute__ ((noinline, noclone))
do_test (void)
{
  /* Initialize test values */
  float float_b[32] = {-1.2f, 3.4f, -5.6f, 7.8f,
                      -9.0f, 1.0f, -2.0f, 3.0f,
                      -4.0f, -5.0f, 6.0f, 7.0f,
                      -8.0f, -9.0f, 10.0f, 11.0f,
                      -1.2f, 3.4f, -5.6f, 7.8f,
                      -9.0f, 1.0f, -2.0f, 3.0f,
                      -4.0f, -5.0f, 6.0f, 7.0f,
                      -8.0f, -9.0f, 10.0f, 11.0f};

  for (int i = 0; i < 32; i++)
    b_512[i] = (__bf16)float_b[i];

  abs_512 ();
  neg_512 ();
  check_absneg_results (b_512, r_abs_512, r_neg_512, 32);
}

/* { dg-final { scan-tree-dump-times "loop vectorized using 64 byte vectors" 2 "vect" } } */
/* { dg-final { scan-tree-dump-times {(?n)ABS_EXPR <vect} 1 "optimized" { target { ! ia32 } } } } */