/* { dg-do compile } */
/* { dg-options "-O2 -msse4.2 -mno-avx2" } */

typedef int v4si __attribute__((vector_size(16)));
typedef float v4sf __attribute__((vector_size(16)));

v4si foo (v4si x)
{
  return (v4si){ x[0], 1, x[2], 3 };
}

/* { dg-final { scan-assembler "pblendw" } } */

v4si bar (v4sf x)
{
  return (v4si){ 1, x[1], x[2], 3 };
}

/* { dg-final { scan-assembler "cvttps2dq" } } */
/* { dg-final { scan-assembler "pblendw" } } */

v4si baz (v4si x)
{
  return (v4si) { x[1], x[2], x[3], 0 };
}

/* { dg-final { scan-assembler "psrldq" } } */
