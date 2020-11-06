/* { dg-do compile } */

struct S { int a, b; } *e;
int d;

void
foo (struct S *x)
{
  for (e = x; d; d++, e++)
    e->a = e->b = (int) (__UINTPTR_TYPE__) e;
}
