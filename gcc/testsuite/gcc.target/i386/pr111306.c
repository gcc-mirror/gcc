/* { dg-do run } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl" } */
/* { dg-require-effective-target avx512fp16 } */

#define AVX512FP16
#include "avx512f-helper.h"

__attribute__((optimize("O2"),noipa))
void func1(_Float16 *a, _Float16 *b, int n, _Float16 *c) {
  __m512h rA = _mm512_loadu_ph(a);
  for (int i = 0; i < n; i += 32) {
    __m512h rB = _mm512_loadu_ph(b + i);
    _mm512_storeu_ph(c + i, _mm512_fcmul_pch(rB, rA));
  }
}

void
test_512 (void)
{
  int n = 32;
  _Float16 a[n], b[n], c[n];
  _Float16 exp[n];
  for (int i = 1; i <= n; i++) {
    a[i - 1] = i & 1 ? -i : i;
    b[i - 1] = i;
  }

  func1(a, b, n, c);
  for (int i = 0; i < n / 32; i += 2) {
    if (c[i] != a[i] * b[i] + a[i+1] * b[i+1]
	|| c[i+1] != a[i] * b[i+1] - a[i+1]*b[i])
      __builtin_abort ();
    }
}


