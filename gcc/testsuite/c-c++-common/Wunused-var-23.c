/* PR c/44677 */
/* { dg-do compile } */
/* { dg-options "-O2 -Wunused-but-set-variable=1" } */

void baz (int);

void
foo (void)
{
  int a = 0;		/* { dg-warning "variable 'a' set but not used" } */
  a = 1;
  int b = 0;
  ++b;
  int c = 0;
  c++;
  int d = 0;
  --d;
  int e = 0;
  e--;
  int f = 0;
  f += 2;
  int g = 0;
  g |= 2;
  int h = 0;
  h -= 2;
  int i = 0;
  i &= 2;
  int j = 0;
  j ^= 2;
  int k = 0;
  k *= 2;
  int l = 0;
  l %= 2;
  int m = 0;
  for (int n = 4; n < 10; n++, m++)
    baz (n);
}

int
bar (void)
{
  int a = 0;
  int b = ++a;
  int c = 0;
  int d = --c;
  int e = 0;
  int f = e--;
  int g = 0;
  int h = g++;
  int i = 0;
  int j;
  j = i += 42;
  int k = 0;
  int l;
  l = k *= 4;
  int m = 0;
  int n;
  n = m |= 2;
  return b + d + f + h + j + l + n;
}
