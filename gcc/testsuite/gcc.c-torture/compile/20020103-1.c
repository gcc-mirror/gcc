/* This testcase failed on Alpha at -O2 when simplifying conditional
   expressions.  */

int foo (void);

struct A
{
  int a, b, c, d;
};

void bar (struct A *x)
{
  int e, f;

  e = foo ();
  e = e / x->b;
  if (e < 1)
    e = 1;
  f = (x->a + x->c) / e;
  if (f < x->d)
    x->d -= (1 << 16) / 8;
}
