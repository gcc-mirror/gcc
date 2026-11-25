/* { dg-require-effective-target int32plus } */
/* { dg-additional-options "-ftrapv" } */

#include "tree-vect.h"

#define N 4
int X[N], Z[N];

__attribute__((noipa)) long long
f(void)
{
  long long acc = 0;
  for (int i = 0; i < N; i++)
    acc += (long long)X[i] * Z[i];
  return acc;
}

int main()
{
  check_vect ();

  X[0] = (-2147483647 - 1); Z[0] = (-2147483647 - 1);
  X[1] = 2147483647;        Z[1] = (-2147483647 - 1);
  X[2] = (-2147483647 - 1); Z[2] = (-2147483647 - 1);
  X[3] = 2147483647;        Z[3] = (-2147483647 - 1);

  long long e = 0;
#pragma GCC novector
  for (int i = 0; i < N; i++)
    e += (long long)X[i] * Z[i];

  long long r = f();
  if (r != e)
    __builtin_abort ();
  return 0;
}
