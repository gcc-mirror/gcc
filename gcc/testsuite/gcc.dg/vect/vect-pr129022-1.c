/* { dg-additional-options "--param vect-partial-vector-usage=1" } */
/* { dg-additional-options "-mavx512f -mavx512bw -mavx512vl" { target { avx512f_runtime && { avx512bw && avx512vl } } } } */

#include "tree-vect.h"

int A[64], C[64];

__attribute__((noipa)) unsigned long long
f(void)
{
  unsigned long long acc = 0;
  for (int i = 0; i < 58; i++)
    {
      unsigned short x = A[i] + 1;
      int c = x ? x + 1 : 0;
      C[i] = c;
      acc += (unsigned long long)c * 3 + i;
    }
  return acc;
}

int main()
{
  check_vect ();
  unsigned long long r = f();
  if (r != 58 * 6 + 58 * 57 / 2)
    __builtin_abort ();
  return 0;
}
