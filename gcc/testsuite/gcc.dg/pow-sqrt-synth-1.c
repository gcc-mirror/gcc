/* { dg-do compile { target sqrt_insn } } */
/* { dg-options "-fdump-tree-powcabs -Ofast --param max-pow-sqrt-depth=8" } */
/* { dg-additional-options "-mfloat-abi=softfp -mfpu=neon-vfpv4" { target arm*-*-* } } */

double
foo (double a)
{
  return __builtin_pow (a, -5.875);
}

double
foof (double a)
{
  return __builtin_pow (a, 0.75f);
}

double
bar (double a)
{
  return __builtin_pow (a, 1.0 + 0.00390625);
}

double
baz (double a)
{
  return __builtin_pow (a, -1.25) + __builtin_pow (a, 5.75) - __builtin_pow (a, 3.375);
}

#define N 256
void
vecfoo (double *a)
{
  for (int i = 0; i < N; i++)
    a[i] = __builtin_pow (a[i], 1.25);
}

/* { dg-final { scan-tree-dump-times "synthesizing" 7 "powcabs" } } */
