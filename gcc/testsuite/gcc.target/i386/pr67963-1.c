/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -march=lakemont" } */

float foo (void)
{
  return 0.0f;
}

/* { dg-final { scan-assembler-not "fldz" } } */
