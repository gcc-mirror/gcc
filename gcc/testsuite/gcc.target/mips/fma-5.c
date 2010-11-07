/* { dg-options "-mgp64 -mhard-float isa>=4 -O3 -fno-fast-math -ffp-contract=off -ffinite-math-only" } */
/* { dg-final { scan-assembler-not "\tnmadd\\." } } */
/* { dg-final { scan-assembler-not "\tnmsub\\." } } */

/* These patterns can only use NMSUB if -fno-signed-zeros is in effect.  */

NOMIPS16 float
not_nmsub_s_2 (float b, float c, float d)
{
  return __builtin_fmaf (-b, c, d);
}

NOMIPS16 double
not_nmsub_d_2 (double b, double c, double d)
{
  return __builtin_fma (-b, c, d);
}
