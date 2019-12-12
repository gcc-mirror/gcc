/* { dg-do run } */
/* { dg-skip-if "asm operand has impossible constraints" { hppa*-*-* } } */
/* { dg-additional-options "-fstack-protector" { target fstack_protector } } */
/* { dg-additional-options "-fPIC" { target fpic } } */

struct S { int *l, *u; };
int a[3];

__attribute__((noipa)) struct S
foo (void)
{
  int *p = a, *q = a + 1;
  struct S s;
  asm volatile ("" : "+g" (p), "+g" (q) : : "memory");
  s.l = p;
  s.u = q;
  a[0]++;
  return s;
}

__attribute__((noipa)) void
bar (struct S *x)
{
  asm volatile ("" : : "g" (x) : "memory");
  if (x->l != a || x->u != a + 1)
    __builtin_abort ();
  a[1]++;
}

__attribute__((noipa)) int
baz (int *x, int *y)
{
  int r = -1;
  asm volatile ("" : "+g" (r) : "g" (x), "g" (y) : "memory");
  a[2]++;
  return r;
}

__attribute__((noipa)) void
quux (void)
{
  asm volatile ("" : : : "memory");
}

__attribute__((noipa)) void
qux (void)
{
  struct S v = foo ();
  struct S w;
  struct S x = foo ();
  int y = 0;

  w.l = x.l;
  w.u = x.u;
  if (baz (x.l, v.l) > 0)
    {
      w.l = v.l;
      y = 1;
      quux ();
    }
  if (baz (x.u, v.u) < 0)
    {
      w.u = v.u;
      y = 1;
    }
  if (y)
    bar (&w);
}

int
main ()
{
  qux ();
  if (a[0] != 2 || a[1] != 1 || a[2] != 2)
    __builtin_abort ();
  return 0;
}
