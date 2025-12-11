/* { dg-do compile } */
/* { dg-options "-O2 -mno-lsx" } */
/* { dg-final { scan-assembler-times "movgr2fr" 2 } } */

double
get_double_zero ()
{
  return 0;
}

float
get_float_zero ()
{
  return 0;
}
