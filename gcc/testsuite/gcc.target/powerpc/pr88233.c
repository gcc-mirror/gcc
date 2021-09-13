/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -mdejagnu-cpu=power8" } */

typedef struct { double a[2]; } A;
A
foo (const A *a)
{
  return *a;
}

/* { dg-final { scan-assembler-not {\mmtvsr} } } */
/* { dg-final { scan-assembler-times {\mlxvd2x\M} 1 { target { be } } } } */
/* { dg-final { scan-assembler-times {\mstxvd2x\M} 1 { target { be } } } } */
/* { dg-final { scan-assembler-times {\mlfd\M} 2 { target { le } } } } */
