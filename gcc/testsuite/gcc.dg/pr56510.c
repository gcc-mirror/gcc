/* PR debug/56510 */
/* { dg-do compile } */
/* { dg-options "-O2 -g" } */

struct S { unsigned long s1; void **s2[0]; };
void **a, **b, **c, **d, **e, **f;

static void **
baz (long x, long y)
{
  void **s = f;
  *f = (void **) (y << 8 | (x & 0xff));
  f += y + 1;
  return s;
}

void bar (void);
void
foo (void)
{
  void **g = b[4];
  a = b[2];
  b = b[1];
  g[2] = e;
  void **h
    = ((void **************************)
       a)[1][1][1][1][1][1][1][1][1][1][1][1][1][1][1][1][1][1][1][1][1][1][1][1][66];
  void **i = ((struct S *) h)->s2[4];
  d = baz (4, 3);
  d[1] = b;
  d[2] = a;
  d[3] = bar;
  b = d;
  g[1] = i[2];
  a = g;
  ((void (*) (void)) (i[1])) ();
}
