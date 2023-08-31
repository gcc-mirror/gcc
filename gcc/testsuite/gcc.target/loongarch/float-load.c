/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "fld\\.s" } } */

extern int a;
float
test (void)
{
  return (float)a;
}

