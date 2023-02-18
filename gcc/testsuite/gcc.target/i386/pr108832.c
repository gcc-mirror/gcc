/* PR target/108832 */
/* { dg-do compile } */
/* { dg-options "-O2 -funroll-loops" } */

unsigned int m;
short int n;

long int
bar (unsigned int x)
{
  return x ? x : 1;
}

__attribute__ ((simd)) void
foo (void)
{
  int a = m / bar (3);
  n = 1 % bar (a << 1);
}
