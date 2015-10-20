/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -march=pentium" } */

float
__attribute__((target("arch=lakemont")))
foo (void)
{
  return 0.0f;
}

/* { dg-final { scan-assembler-not "fldz" } } */
