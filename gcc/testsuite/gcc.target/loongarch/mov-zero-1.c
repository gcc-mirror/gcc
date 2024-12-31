/* { dg-do compile } */
/* { dg-options "-O2 -mlsx" } */
/* { dg-final { scan-assembler-times "vxor\\.v" 2 } } */

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
