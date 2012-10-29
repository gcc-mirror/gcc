/* { dg-do compile } */
/* { dg-options "-O2" } */

int r;

void test (int a, int b)
{
  /* { dg-final { scan-assembler "mneg\tw\[0-9\]*, w\[0-9\]*, w\[0-9\]*\n" } } */
  r = - (a * b);
}
