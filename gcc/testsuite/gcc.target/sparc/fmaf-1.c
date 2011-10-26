/* { dg-do compile } */
/* { dg-options "-O2 -mfmaf" } */

float fmadds (float a, float b, float c)
{
  return a * b + c;
}

float fmsubs (float a, float b, float c)
{
  return a * b - c;
}

float fnmadds (float a, float b, float c)
{
  return -(a * b + c);
}

float fnmsubs (float a, float b, float c)
{
  return -(a * b - c);
}

double fmaddd (double a, double b, double c)
{
  return a * b + c;
}

double fmsubd (double a, double b, double c)
{
  return a * b - c;
}

double fnmaddd (double a, double b, double c)
{
  return -(a * b + c);
}

double fnmsubd (double a, double b, double c)
{
  return -(a * b - c);
}

/* { dg-final { scan-assembler "fmadds\t%" } } */
/* { dg-final { scan-assembler "fmsubs\t%" } } */
/* { dg-final { scan-assembler "fnmadds\t%" } } */
/* { dg-final { scan-assembler "fnmsubs\t%" } } */
/* { dg-final { scan-assembler "fmaddd\t%" } } */
/* { dg-final { scan-assembler "fmsubd\t%" } } */
/* { dg-final { scan-assembler "fnmaddd\t%" } } */
/* { dg-final { scan-assembler "fnmsubd\t%" } } */
