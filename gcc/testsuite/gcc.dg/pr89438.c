/* PR target/89438 */
/* { dg-do compile } */
/* { dg-options "-O2 -g -w" } */

struct S { double b, c; struct T { double d, e; } f[16]; } g;
int h, i, j;
double k;

double
foo (void)
{
  int m;
  if (j)
    return k;
  long a, p = a - 80;
  double b, n;
  n = b * h + g.f[p].e;
  m = n;
  double o = 1 ? m : 1.0;
  k = i ? -o : o;
  return k;
}
