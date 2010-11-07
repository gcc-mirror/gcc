/* { dg-options "-mgp64 -mhard-float isa>=4 -O3 -ffast-math" } */
/* { dg-final { scan-assembler-times "\tnmadd\\.s\t" 1 } } */
/* { dg-final { scan-assembler-times "\tnmadd\\.d\t" 1 } } */
/* { dg-final { scan-assembler-not "\tnmsub\\." } } */

NOMIPS16 float
nmadd_s_2 (float b, float c, float d)
{
  return __builtin_fmaf (-b, c, -d);
}

NOMIPS16 double
nmadd_d_2 (double b, double c, double d)
{
  return __builtin_fma (-b, c, -d);
}
