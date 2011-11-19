/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O1 -mvis3" } */

double d;
float f;

long test_convert_from_float(void)
{
  return f;
}

long test_convert_from_double(void)
{
  return d;
}

float test_convert_to_float(long x)
{
  return x;
}

double test_convert_to_double(long x)
{
  return x;
}

/* { dg-final { scan-assembler-times "movdtox\t%" 2 } } */
/* { dg-final { scan-assembler-times "movxtod\t%" 2 } } */
