/* { dg-do compile } */
/* { dg-options "-O2 -Wall" } */

struct S { int *p; int *q; };

void foo (struct S *);

int bar (int b)
{
  struct S s;
  int *p;
  float f;
  foo (&s);
  if (b)
    p = s.q;
  else
    p = (int *)&f;
  return *p;
}
