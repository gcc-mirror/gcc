/* { dg-do compile } */
/* { dg-additional-options "-march=armv8-a+sve -O1" } */

long int m, n;

int
qux (int z)
{
  return 4 >> z ? z : 0;
}

int
bar (long int y)
{
  return y ? 3 : 2;
}

__attribute__ ((simd)) int
foo (int x)
{
  long int a = x & m;
  int b = bar (x) / n;

  return qux (b) == a;
}

