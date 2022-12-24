/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mstv -mno-stackrealign" } */
typedef unsigned long long v2di __attribute__((vector_size(16)));

unsigned long long foo(v2di a, v2di b)
{
  a[0] += b[0];
  return a[0] + a[1];
}

/* { dg-final { scan-assembler-not "\taddq\t" } } */
/* { dg-final { scan-assembler-times "v?paddq" 2 } } */
/* { dg-final { scan-assembler "v?psrldq" } } */
