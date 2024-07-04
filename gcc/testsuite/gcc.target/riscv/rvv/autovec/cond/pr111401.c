/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mrvv-vector-bits=scalable -fdump-tree-vect-details" } */

double
__attribute__ ((noipa))
foo2 (double *__restrict a, double init, int *__restrict cond, int n)
{
  for (int i = 0; i < n; i++)
    if (cond[i])
      init += a[i];
  return init;
}

double
__attribute__ ((noipa))
foo3 (double *__restrict a, double init, int *__restrict cond, int n)
{
  for (int i = 0; i < n; i++)
    if (cond[i])
      init -= a[i];
  return init;
}

double
__attribute__ ((noipa))
foo4 (double *__restrict a, double init, int *__restrict cond, int n)
{
  for (int i = 0; i < n; i++)
    if (cond[i])
      init *= a[i];
  return init;
}

int
__attribute__ ((noipa))
foo5 (int *__restrict a, int init, int *__restrict cond, int n)
{
  for (int i = 0; i < n; i++)
    if (cond[i])
      init &= a[i];
  return init;
}

int
__attribute__ ((noipa))
foo6 (int *__restrict a, int init, int *__restrict cond, int n)
{
  for (int i = 0; i < n; i++)
    if (cond[i])
      init |= a[i];
  return init;
}

int
__attribute__ ((noipa))
foo7 (int *__restrict a, int init, int *__restrict cond, int n)
{
  for (int i = 0; i < n; i++)
    if (cond[i])
      init ^= a[i];
  return init;
}

#define SZ 125

int
main ()
{
  double res1 = 0, res2 = 0, res3 = 0;
  double a1[SZ], a2[SZ], a3[SZ];
  int c1[SZ], c2[SZ], c3[SZ];

  int a4[SZ], a5[SZ], a6[SZ];
  int res4 = 0, res5 = 0, res6 = 0;
  int c4[SZ], c5[SZ], c6[SZ];

  for (int i = 0; i < SZ; i++)
    {
      a1[i] = i * 3 + (i & 4) - (i & 7);
      a2[i] = i * 3 + (i & 4) - (i & 7);
      a3[i] = i * 0.05 + (i & 4) - (i & 7);
      a4[i] = i * 3 + (i & 4) - (i & 7);
      a5[i] = i * 3 + (i & 4) - (i & 7);
      a6[i] = i * 3 + (i & 4) - (i & 7);
      c1[i] = i & 1;
      c2[i] = i & 2;
      c3[i] = i & 3;
      c4[i] = i & 4;
      c5[i] = i & 5;
      c6[i] = i & 6;
      __asm__ volatile ("" : : : "memory");
    }

  double init1 = 2.7, init2 = 8.2, init3 = 0.1;
  double ref1 = init1, ref2 = init2, ref3 = init3;

  int init4 = 87, init5 = 11, init6 = -123894344;
  int ref4 = init4, ref5 = init5, ref6 = init6;

#pragma GCC novector
  for (int i = 0; i < SZ; i++)
    {
      if (c1[i])
        ref1 += a1[i];
      if (c2[i])
        ref2 -= a2[i];
      if (c3[i])
        ref3 *= a3[i];
      if (c4[i])
        ref4 &= a4[i];
      if (c5[i])
        ref5 |= a5[i];
      if (c6[i])
        ref6 ^= a6[i];
    }

  res1 = foo2 (a1, init1, c1, SZ);
  res2 = foo3 (a2, init2, c2, SZ);
  res3 = foo4 (a3, init3, c3, SZ);
  res4 = foo5 (a4, init4, c4, SZ);
  res5 = foo6 (a5, init5, c5, SZ);
  res6 = foo7 (a6, init6, c6, SZ);

  if (res1 != ref1)
    __builtin_abort ();
  if (res2 != ref2)
    __builtin_abort ();
  if (res3 != ref3)
    __builtin_abort ();
  if (res4 != ref4)
    __builtin_abort ();
  if (res5 != ref5)
    __builtin_abort ();
  if (res6 != ref6)
    __builtin_abort ();
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 5 "vect" } } */
/* { dg-final { scan-tree-dump-not "VCOND_MASK" "vect" } } */
