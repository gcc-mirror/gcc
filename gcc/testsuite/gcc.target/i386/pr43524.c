/* { dg-do compile } */
/* { dg-options "-mstack-arg-probe" } */

extern void bar (void);

void foo (int i)
{
  bar ();
}
