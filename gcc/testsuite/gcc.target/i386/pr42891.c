/* { dg-do compile } */
/* { dg-options "-O2" } */

union B { int i; float f; };

extern void bar (void);

void
foo (union B x, union B y)
{
  if (!(y.f > x.i))
    bar ();
}
