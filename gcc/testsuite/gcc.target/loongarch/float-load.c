/* { dg-do compile } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "fld\\.s" } } */

extern int a;
float
test (void)
{
  return (float)a;
}

