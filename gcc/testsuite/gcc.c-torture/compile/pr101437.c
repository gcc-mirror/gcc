/* PR middle-end/101437 */

struct S { int : 1; };

void
foo (volatile struct S *p)
{
  struct S s = {};
  *p = s;
}

void
bar (volatile struct S *p)
{
  *p;
}

void
baz (volatile struct S *p)
{
  struct S s;
  s = *p;
}

void
qux (volatile struct S *p, volatile struct S *q)
{
  *p = *q;
}
