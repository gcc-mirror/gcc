/* PR target/97322 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

void
foo (unsigned long long x, unsigned long long *y)
{
  y[0] = x / 10;
  y[1] = x % 10;
}

void
bar (unsigned int x, unsigned int *y)
{
  y[0] = x / 10;
  y[1] = x % 10;
}
