/* Make sure a -0 stays -0 when we perform a conditional reduction.  */
/* { dg-require-effective-target vect_double } */
/* { dg-add-options ieee } */
/* { dg-additional-options "-std=gnu99 -fno-fast-math" } */

#include "tree-vect.h"

#include <math.h>

#define N (VECTOR_BITS * 17)

double __attribute__ ((noinline, noclone))
reduc_plus_double (double *restrict a, double init, int *cond, int n)
{
  double res = init;
  for (int i = 0; i < n; i++)
    if (cond[i])
      res += a[i];
  return res;
}

double __attribute__ ((noinline, noclone, optimize ("0")))
reduc_plus_double_ref (double *restrict a, double init, int *cond, int n)
{
  double res = init;
  for (int i = 0; i < n; i++)
    if (cond[i])
      res += a[i];
  return res;
}

double __attribute__ ((noinline, noclone))
reduc_minus_double (double *restrict a, double init, int *cond, int n)
{
  double res = init;
  for (int i = 0; i < n; i++)
    if (cond[i])
      res -= a[i];
  return res;
}

double __attribute__ ((noinline, noclone, optimize ("0")))
reduc_minus_double_ref (double *restrict a, double init, int *cond, int n)
{
  double res = init;
  for (int i = 0; i < n; i++)
    if (cond[i])
      res -= a[i];
  return res;
}

int __attribute__ ((optimize (1)))
main ()
{
  int n = 19;
  double a[N];
  int cond1[N], cond2[N];

  for (int i = 0; i < N; i++)
    {
      a[i] = (i * 0.1) * (i & 1 ? 1 : -1);
      cond1[i] = 0;
      cond2[i] = i & 4 ? 1 : 0;
      asm volatile ("" ::: "memory");
    }

  double res1 = reduc_plus_double (a, -0.0, cond1, n);
  double ref1 = reduc_plus_double_ref (a, -0.0, cond1, n);
  double res2 = reduc_minus_double (a, -0.0, cond1, n);
  double ref2 = reduc_minus_double_ref (a, -0.0, cond1, n);
  double res3 = reduc_plus_double (a, -0.0, cond1, n);
  double ref3 = reduc_plus_double_ref (a, -0.0, cond1, n);
  double res4 = reduc_minus_double (a, -0.0, cond1, n);
  double ref4 = reduc_minus_double_ref (a, -0.0, cond1, n);

  if (res1 != ref1 || signbit (res1) != signbit (ref1))
    __builtin_abort ();
  if (res2 != ref2 || signbit (res2) != signbit (ref2))
    __builtin_abort ();
  if (res3 != ref3 || signbit (res3) != signbit (ref3))
    __builtin_abort ();
  if (res4 != ref4 || signbit (res4) != signbit (ref4))
    __builtin_abort ();

  res1 = reduc_plus_double (a, 0.0, cond1, n);
  ref1 = reduc_plus_double_ref (a, 0.0, cond1, n);
  res2 = reduc_minus_double (a, 0.0, cond1, n);
  ref2 = reduc_minus_double_ref (a, 0.0, cond1, n);
  res3 = reduc_plus_double (a, 0.0, cond1, n);
  ref3 = reduc_plus_double_ref (a, 0.0, cond1, n);
  res4 = reduc_minus_double (a, 0.0, cond1, n);
  ref4 = reduc_minus_double_ref (a, 0.0, cond1, n);

  if (res1 != ref1 || signbit (res1) != signbit (ref1))
    __builtin_abort ();
  if (res2 != ref2 || signbit (res2) != signbit (ref2))
    __builtin_abort ();
  if (res3 != ref3 || signbit (res3) != signbit (ref3))
    __builtin_abort ();
  if (res4 != ref4 || signbit (res4) != signbit (ref4))
    __builtin_abort ();

  res1 = reduc_plus_double (a, -0.0, cond2, n);
  ref1 = reduc_plus_double_ref (a, -0.0, cond2, n);
  res2 = reduc_minus_double (a, -0.0, cond2, n);
  ref2 = reduc_minus_double_ref (a, -0.0, cond2, n);
  res3 = reduc_plus_double (a, -0.0, cond2, n);
  ref3 = reduc_plus_double_ref (a, -0.0, cond2, n);
  res4 = reduc_minus_double (a, -0.0, cond2, n);
  ref4 = reduc_minus_double_ref (a, -0.0, cond2, n);

  if (res1 != ref1 || signbit (res1) != signbit (ref1))
    __builtin_abort ();
  if (res2 != ref2 || signbit (res2) != signbit (ref2))
    __builtin_abort ();
  if (res3 != ref3 || signbit (res3) != signbit (ref3))
    __builtin_abort ();
  if (res4 != ref4 || signbit (res4) != signbit (ref4))
    __builtin_abort ();

  res1 = reduc_plus_double (a, 0.0, cond2, n);
  ref1 = reduc_plus_double_ref (a, 0.0, cond2, n);
  res2 = reduc_minus_double (a, 0.0, cond2, n);
  ref2 = reduc_minus_double_ref (a, 0.0, cond2, n);
  res3 = reduc_plus_double (a, 0.0, cond2, n);
  ref3 = reduc_plus_double_ref (a, 0.0, cond2, n);
  res4 = reduc_minus_double (a, 0.0, cond2, n);
  ref4 = reduc_minus_double_ref (a, 0.0, cond2, n);

  if (res1 != ref1 || signbit (res1) != signbit (ref1))
    __builtin_abort ();
  if (res2 != ref2 || signbit (res2) != signbit (ref2))
    __builtin_abort ();
  if (res3 != ref3 || signbit (res3) != signbit (ref3))
    __builtin_abort ();
  if (res4 != ref4 || signbit (res4) != signbit (ref4))
    __builtin_abort ();

  return 0;
}
