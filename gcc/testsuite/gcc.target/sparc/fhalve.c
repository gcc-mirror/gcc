/* { dg-do compile { target { vis3 } } } */
/* { dg-options "-mcpu=niagara3 -mvis" } */

float test_fhadds (float x, float y)
{
  return __builtin_vis_fhadds (x, y);
}

double test_fhaddd (double x, double y)
{
  return __builtin_vis_fhaddd (x, y);
}

float test_fhsubs (float x, float y)
{
  return __builtin_vis_fhsubs (x, y);
}

double test_fhsubd (double x, double y)
{
  return __builtin_vis_fhsubd (x, y);
}

float test_fnhadds (float x, float y)
{
  return __builtin_vis_fnhadds (x, y);
}

double test_fnhaddd (double x, double y)
{
  return __builtin_vis_fnhaddd (x, y);
}

/* { dg-final { scan-assembler "fhadds\t%" } } */
/* { dg-final { scan-assembler "fhaddd\t%" } } */
/* { dg-final { scan-assembler "fhsubs\t%" } } */
/* { dg-final { scan-assembler "fhsubd\t%" } } */
/* { dg-final { scan-assembler "fnhadds\t%" } } */
/* { dg-final { scan-assembler "fnhaddd\t%" } } */
