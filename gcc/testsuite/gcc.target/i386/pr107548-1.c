/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mstv -mno-stackrealign" } */
typedef unsigned int v4si __attribute__((vector_size(16)));

unsigned int foo1 (v4si a, v4si b)
{
  a[0] += b[0];
  return a[0] + a[1];
}

unsigned int foo2 (v4si a, v4si b)
{
  a[0] += b[0];
  return a[0] + a[2];
}

unsigned int foo3 (v4si a, v4si b)
{
  a[0] += b[0];
  return a[0] + a[3];
}

/* { dg-final { scan-assembler-times "\tv?movd\t" 3 } } */
/* { dg-final { scan-assembler-times "v?paddd" 6 } } */
/* { dg-final { scan-assembler-not "addl" } } */
