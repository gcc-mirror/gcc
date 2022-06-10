/* { dg-options "-mdejagnu-cpu=power4 -O2 -fcompare-debug -fharden-compares -frename-registers" } */

double m;
int n;

unsigned int
foo (unsigned int x, int y)
{
  long long int a = y, b = !a;
  int c = 0;

  if (b != x)
    while ((int) m == a)
      {
        c = a;
        a = 0;
      }

  n = b = y;

  return x + c;
}
