/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "" { powerpc*-*-darwin* } } */
/* { dg-options "-O2 -mdejagnu-cpu=power6 -mavoid-indexed-addresses" } */
/* { dg-final { scan-assembler-times "lwbrx" 12 } } */
/* { dg-final { scan-assembler-times "stwbrx" 6 } } */

/* PR 51399: bswap gets an error if -mavoid-indexed-addresses was used in
   creating the two lwbrx instructions.  */

long long
load64_reverse_1 (long long *p)
{
  return __builtin_bswap64 (*p);
}

long long
load64_reverse_2 (long long *p)
{
  return __builtin_bswap64 (p[1]);
}

long long
load64_reverse_3 (long long *p, int i)
{
  return __builtin_bswap64 (p[i]);
}

long long
load64_reverse_4 (long long dummy __attribute__ ((unused)), long long *p)
{
  return __builtin_bswap64 (*p);
}

long long
load64_reverse_5 (long long dummy __attribute__ ((unused)), long long *p)
{
  return __builtin_bswap64 (p[1]);
}

long long
load64_reverse_6 (long long dummy __attribute__ ((unused)), long long *p, int i)
{
  return __builtin_bswap64 (p[i]);
}

void
store64_reverse_1 (long long *p, long long x)
{
  *p = __builtin_bswap64 (x);
}

void
store64_reverse_2 (long long *p, long long x)
{
  p[1] = __builtin_bswap64 (x);
}

void
store64_reverse_3 (long long *p, long long x, int i)
{
  p[i] = __builtin_bswap64 (x);
}

long long
reg_reverse_1 (long long x)
{
  return __builtin_bswap64 (x);
}

long long
reg_reverse_2 (long long dummy __attribute__ ((unused)), long long x)
{
  return __builtin_bswap64 (x);
}
