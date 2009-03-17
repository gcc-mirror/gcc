/* PR debug/39412 */
/* { dg-do compile } */

struct S { int i; };

inline void
bar (const void *x, unsigned long y)
{
  const union { struct S a[y]; } *u = x;
}

void
foo (const void *x, unsigned long y)
{
  bar (x, y);
}
