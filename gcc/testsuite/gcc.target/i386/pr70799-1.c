/* PR target/pr70799 */
/* { dg-do compile { target { ia32 } } } */
/* { dg-options "-O2 -march=slm" } */
/* { dg-final { scan-assembler "pxor" } } */
/* { dg-final { scan-assembler "pcmpeqd" } } */
/* { dg-final { scan-assembler "movdqa\[ \\t\]+.LC0" } } */

long long a, b, c;

void test1 (void)
{
  long long t;
  if (a)
    t = 0LL;
  else
    t = b;
  a = c & t;
  b = c | t;
}

void test2 (void)
{
  long long t;
  if (a)
    t = -1LL;
  else
    t = b;
  a = c & t;
  b = c | t;
}

void test3 (void)
{
  long long t;
  if (a)
    t = 0xf0f0f0f0f0f0f0f0LL;
  else
    t = b;
  a = c & t;
  b = c | t;
}
