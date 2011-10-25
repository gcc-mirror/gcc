/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O1 -mvis3" } */

double d;
float f;

int test_convert_from_float(void)
{
  return f;
}

int test_convert_from_double(void)
{
  return d;
}

float test_convert_to_float(int x)
{
  return x;
}

double test_convert_to_double(int x)
{
  return x;
}

/* { dg-final { scan-assembler-times "movstouw\t%" 2 } } */
/* { dg-final { scan-assembler-times "movwtos\t%" 2 } } */
