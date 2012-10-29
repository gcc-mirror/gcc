/* { dg-do compile } */
/* { dg-options "-O2" } */

long long r;

void test_signed (int a, int b)
{
  /* { dg-final { scan-assembler "smnegl\tx\[0-9\]*, w\[0-9\]*, w\[0-9\]*\n" } } */
  r = ((long long) a) * (-((long long) b));
}

void test_unsigned (unsigned int a, unsigned int b)
{
  /* { dg-final { scan-assembler "umnegl\tx\[0-9\]*, w\[0-9\]*, w\[0-9\]*\n" } } */
  r = ((long long) a) * (-((long long) b));
}
