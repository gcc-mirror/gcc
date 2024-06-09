/* PR sanitizer/112709 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=address -O2" } */

struct S { char c[1024]; } *p;
int foo (int);

__attribute__((returns_twice, noipa)) int
bar (struct S x)
{
  (void) x.c[0];
  return 0;
}

void
baz (int *y)
{
  foo (1);
  *y = bar (*p);
}

void
qux (int x, int *y)
{
  if (x == 25)
    x = foo (2);
  else if (x == 42)
    x = foo (foo (3));
  *y = bar (*p);
}

void
corge (int x, int *y)
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
  *y = bar (*p);
  if (x < 4)
    goto *q[x & 3];
}

void
freddy (int x, int *y, struct S *p)
{
  bar (*p);
  ++p;
  if (x == 25)
    x = foo (2);
  else if (x == 42)
    x = foo (foo (3));
  *y = bar (*p);
}
