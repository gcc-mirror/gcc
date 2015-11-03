/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -march=lakemont" } */

float
__attribute__((target("arch=pentium")))
foo (void)
{
  return 0.0f;
}

/* { dg-final { scan-assembler "fldz" } } */
