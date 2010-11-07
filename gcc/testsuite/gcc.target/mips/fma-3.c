/* { dg-options "-mgp64 -mhard-float isa>=4 -O3 -fno-fast-math -ffp-contract=off -ffinite-math-only" } */
/* { dg-final { scan-assembler-times "\tnmsub\\.s\t" 1 } } */
/* { dg-final { scan-assembler-times "\tnmsub\\.d\t" 1 } } */
/* { dg-final { scan-assembler-not "\tnmadd\\." } } */

NOMIPS16 float
nmsub_s (float b, float c, float d)
{
  return -__builtin_fmaf (b, c, -d);
}

NOMIPS16 double
nmsub_d (double b, double c, double d)
{
  return -__builtin_fma (b, c, -d);
}
