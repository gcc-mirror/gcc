/* PR tree-optimization/58775 */

void bar (void);

void
foo (char *x)
{
  char a;
  _Bool b, c, d, e, f, g, h, i, j, k, l, m;

  a = *x;
  b = a == 100;
  c = a == 105;
  d = b | c;
  e = a != 111;
  f = !d;
  g = e & f;
  h = a != 117;
  i = g & h;
  j = a != 120;
  k = i & j;
  l = a != 88;
  m = k & l;
  if (m == 0)
    bar ();
}
