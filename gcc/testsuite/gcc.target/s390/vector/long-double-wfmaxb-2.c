/* { dg-do compile } */
/* { dg-options "-O3 -march=z14 -mzarch -mvx-long-double-fma" } */

int a, c, d, f, k, l, m;
long double b, e, g, h, i;
double j;

void
n (void)
{
  while (m)
    {
      a = b * d;
      b = c;
      c = d * e + 2;
      e = f + g + 4;
      f = h + 6;
      g = h * 0 + i + 7;
      h = i + 9;
      i = j * k + 0 + 10;
      j = l;
      m = a * b;
    }
}
