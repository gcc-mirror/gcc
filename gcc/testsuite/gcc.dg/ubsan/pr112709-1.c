/* PR sanitizer/112709 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined -O2" } */

struct S { char c[1024]; };
int foo (int);

__attribute__((returns_twice, noipa)) struct S
bar (int x)
{
  (void) x;
  struct S s = {};
  s.c[42] = 42;
  return s;
}

void
baz (struct S *p)
{
  foo (1);
  *p = bar (0);
}

void
qux (int x, struct S *p)
{
  if (x == 25)
    x = foo (2);
  else if (x == 42)
    x = foo (foo (3));
  *p = bar (x);
}

void
corge (int x, struct S *p)
{
  void *q[] = { &&l1, &&l2, &&l3, &&l3 };
  if (x == 25)
    {
    l1:
      x = foo (2);
    }
  else if (x == 42)
    {
    l2:
      x = foo (foo (3));
    }
l3:
  *p = bar (x);
  if (x < 4)
    goto *q[x & 3];
}

void
freddy (int x, struct S *p)
{
  *p = bar (x);
  ++p;
  if (x == 25)
    x = foo (2);
  else if (x == 42)
    x = foo (foo (3));
  *p = bar (x);
}
