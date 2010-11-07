/* { dg-options "-mgp64 -mhard-float isa>=4 -O2 -ffast-math" } */
/* { dg-final { scan-assembler-times "\tmadd\\.s\t" 1 } } */
/* { dg-final { scan-assembler-times "\tmsub\\.s\t" 1 } } */
/* { dg-final { scan-assembler-times "\tnmadd\\.s\t" 2 } } */
/* { dg-final { scan-assembler-times "\tnmsub\\.s\t" 2 } } */
/* { dg-final { scan-assembler-times "\tmadd\\.d\t" 1 } } */
/* { dg-final { scan-assembler-times "\tmsub\\.d\t" 1 } } */
/* { dg-final { scan-assembler-times "\tnmadd\\.d\t" 2 } } */
/* { dg-final { scan-assembler-times "\tnmsub\\.d\t" 2 } } */

NOMIPS16 float
madd_s (float b, float c, float d)
{
  return b * c + d;
}

NOMIPS16 float
msub_s (float b, float c, float d)
{
  return b * c + -d;
}

NOMIPS16 float
nmadd_s (float b, float c, float d)
{
  return -(b * c + d);
}

NOMIPS16 float
nmsub_s (float b, float c, float d)
{
  return -(b * c + -d);
}

NOMIPS16 float
nmadd_s_2 (float b, float c, float d)
{
  return -b * c - d;
}

NOMIPS16 float
nmsub_s_2 (float b, float c, float d)
{
  return -b * c + d;
}

NOMIPS16 double
madd_d (double b, double c, double d)
{
  return b * c + d;
}

NOMIPS16 double
msub_d (double b, double c, double d)
{
  return b * c + -d;
}

NOMIPS16 double
nmadd_d (double b, double c, double d)
{
  return -(b * c + d);
}

NOMIPS16 double
nmsub_d (double b, double c, double d)
{
  return -(b * c + -d);
}

NOMIPS16 double
nmadd_d_2 (double b, double c, double d)
{
  return -b * c - d;
}

NOMIPS16 double
nmsub_d_2 (double b, double c, double d)
{
  return -b * c + d;
}
