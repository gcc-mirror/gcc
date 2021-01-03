/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch -mvx-long-double-fma" } */

long double a, c, d;
int b;
void
e (void)
{
  while (d)
    {
      a = 0 * c + 0;
      d = b;
    }
}
