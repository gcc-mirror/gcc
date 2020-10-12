/* { dg-do assemble } */
/* { dg-options "-O2 -save-temps" } */

double
foo (double x)
{
  return 1.0 / x;
}

float
foof (float x)
{
  return 1.0f / x;
}

/* { dg-final { scan-assembler-times "rcp.rn.f64" 1 } } */
/* { dg-final { scan-assembler-times "rcp.rn.f32" 1 } } */

