/* { dg-do compile { target { ! { x86_64-*-mingw* } } } } */
/* { dg-options "-O2" } */
/* { dg-additional-options "-mpreferred-stack-boundary=12" { target { { i?86-*-* x86_64-*-* } && lp64 } } } */

extern void bar (int*);

char *p;

int
main ()
{
  int q;
  p = __builtin_stack_save ();
  bar (&q);
  __builtin_stack_restore (p);
  bar (&q);
  return 0;
}
