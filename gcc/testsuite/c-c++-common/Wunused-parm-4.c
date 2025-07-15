/* PR c/44677 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wunused-but-set-parameter=2" } */

void baz (int);

void
foo (int a,		/* { dg-warning "parameter 'a' set but not used" } */
     int b,		/* { dg-warning "parameter 'b' set but not used" } */
     int c,		/* { dg-warning "parameter 'c' set but not used" } */
     int d,		/* { dg-warning "parameter 'd' set but not used" } */
     int e,		/* { dg-warning "parameter 'e' set but not used" } */
     int f,
     int g,
     int h,
     int i,
     int j,
     int k,
     int l,
     int m)		/* { dg-warning "parameter 'm' set but not used" } */
{
  a = 1;
  ++b;
  c++;
  --d;
  e--;
  f += 2;
  g |= 2;
  h -= 2;
  i &= 2;
  j ^= 2;
  k *= 2;
  l %= 2;
  for (int n = 4; n < 10; n++, m++)
    baz (n);
}

int
bar (int a, int b, int c, int d, int e, int f, int g, int h, int i, int j,
     int k, int l, int m, int n)
{
  b = ++a;
  d = --c;
  f = e--;
  h = g++;
  j = i += 42;
  l = k *= 4;
  n = m |= 2;
  return b + d + f + h + j + l + n;
}
