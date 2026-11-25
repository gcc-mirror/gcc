/* { dg-additional-options "-mavx512f" { target avx512f_runtime } } */

#include "tree-vect.h"

#define N 16
int X[N], Z[N], Y[N];

__attribute__((noipa)) unsigned long long
f(void)
{
  unsigned long long acc = 0;
  for (int i = 0; i < N; i++)
    if (Y[i])
      acc += (unsigned long long)X[i] * Z[i];
  return acc;
}

int main()
{
  check_vect ();

  unsigned long long e = 0;
#pragma GCC novector
  for (int i = 0; i < N; i++)
    {
      X[i] = 1; Z[i] = i; Y[i] = i & 1;
      if (Y[i])
	e += (unsigned long long)X[i] * Z[i];
    }
  unsigned long long r = f();
  if (r != e)
    __builtin_abort ();
  return 0;
}
