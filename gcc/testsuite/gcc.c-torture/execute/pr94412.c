/* PR middle-end/94412 */

typedef unsigned V __attribute__ ((__vector_size__ (sizeof (unsigned) * 2)));

void
foo (V *v, V *w)
{
  *w = -*v / 11;
}

void
bar (V *v, V *w)
{
  *w = -18 / -*v;
}

int
main ()
{
  V a = (V) { 1, 0 };
  V b = (V) { 3, __INT_MAX__ };
  V c, d;
  foo (&a, &c);
  bar (&b, &d);
  if (c[0] != -1U / 11 || c[1] != 0 || d[0] != 0 || d[1] != -18U / -__INT_MAX__)
    __builtin_abort ();
  return 0;
}
