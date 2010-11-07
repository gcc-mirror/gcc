/* { dg-options "-mgp64 -mhard-float isa=loongson -O3 -fno-fast-math -ffp-contract=off" } */
/* { dg-final { scan-assembler-not "\tmadd\\." } } */
/* { dg-final { scan-assembler-not "\tmsub\\." } } */
/* { dg-final { scan-assembler-not "\tnmadd\\." } } */
/* { dg-final { scan-assembler-not "\tnmsub\\." } } */

/* No function should use fused operations, however high the -O level.  */

NOMIPS16 float
not_madd_s (float b, float c, float d)
{
  return b * c + d;
}

NOMIPS16 float
not_msub_s (float b, float c, float d)
{
  return b * c + -d;
}

NOMIPS16 float
not_nmadd_s (float b, float c, float d)
{
  return -(b * c + d);
}

NOMIPS16 float
not_nmsub_s (float b, float c, float d)
{
  return -(b * c + -d);
}

NOMIPS16 float
not_nmadd_s_2 (float b, float c, float d)
{
  return -b * c - d;
}

NOMIPS16 float
not_nmsub_s_2 (float b, float c, float d)
{
  return -b * c + d;
}

NOMIPS16 double
not_madd_d (double b, double c, double d)
{
  return b * c + d;
}

NOMIPS16 double
not_msub_d (double b, double c, double d)
{
  return b * c + -d;
}

NOMIPS16 double
not_nmadd_d (double b, double c, double d)
{
  return -(b * c + d);
}

NOMIPS16 double
not_nmsub_d (double b, double c, double d)
{
  return -(b * c + -d);
}

NOMIPS16 double
not_nmadd_d_2 (double b, double c, double d)
{
  return -b * c - d;
}

NOMIPS16 double
not_nmsub_d_2 (double b, double c, double d)
{
  return -b * c + d;
}
