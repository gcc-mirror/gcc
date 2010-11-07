/* { dg-options "-mgp64 -mhard-float isa>=4 -O3 -fno-fast-math -ffp-contract=off" } */
/* { dg-final { scan-assembler-times "\tmadd\\.s\t" 3 } } */
/* { dg-final { scan-assembler-times "\tmsub\\.s\t" 3 } } */
/* { dg-final { scan-assembler-times "\tmadd\\.d\t" 3 } } */
/* { dg-final { scan-assembler-times "\tmsub\\.d\t" 3 } } */
/* { dg-final { scan-assembler-not "\tnmadd\\." } } */
/* { dg-final { scan-assembler-not "\tnmsub\\." } } */

/* We should not use NMADD or NMSUB without -ffinite-math-only because
   those instructions may perform arithmetic negation.  */

NOMIPS16 float
madd_s (float b, float c, float d)
{
  return __builtin_fmaf (b, c, d);
}

NOMIPS16 float
msub_s (float b, float c, float d)
{
  return __builtin_fmaf (b, c, -d);
}

NOMIPS16 float
not_nmadd_s (float b, float c, float d)
{
  return -__builtin_fmaf (b, c, d);
}

NOMIPS16 float
not_nmsub_s (float b, float c, float d)
{
  return -__builtin_fmaf (b, c, -d);
}

NOMIPS16 float
not_nmadd_s_2 (float b, float c, float d)
{
  return __builtin_fmaf (-b, c, -d);
}

NOMIPS16 float
not_nmsub_s_2 (float b, float c, float d)
{
  return __builtin_fmaf (-b, c, d);
}

NOMIPS16 double
madd_d (double b, double c, double d)
{
  return __builtin_fma (b, c, d);
}

NOMIPS16 double
msub_d (double b, double c, double d)
{
  return __builtin_fma (b, c, -d);
}

NOMIPS16 double
not_nmadd_d (double b, double c, double d)
{
  return -__builtin_fma (b, c, d);
}

NOMIPS16 double
not_nmsub_d (double b, double c, double d)
{
  return -__builtin_fma (b, c, -d);
}

NOMIPS16 double
not_nmadd_d_2 (double b, double c, double d)
{
  return __builtin_fma (-b, c, -d);
}

NOMIPS16 double
not_nmsub_d_2 (double b, double c, double d)
{
  return __builtin_fma (-b, c, d);
}
