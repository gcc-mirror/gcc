/* PR target/67770 */
/* { dg-do run { target ia32 } } */
/* { dg-require-effective-target trampolines } */
/* { dg-options "-O2" } */

#ifndef NO_TRAMPOLINES
__attribute__ ((noinline)) void
foo (int i, void (* __attribute__ ((regparm (3))) bar) (int))
{
  bar (i);
}
#endif

int
main ()
{
#ifndef NO_TRAMPOLINES
  int p = 0;

  __attribute__ ((regparm (3), noinline)) void
  bar (int i)
  {
    if (__builtin_expect (i, 0))
      ++p;
  }

  foo (0, bar);
  bar (0);

  if (p != 0)
    __builtin_abort ();

  foo (1, bar);
  bar (1);

  if (p != 2)
    __builtin_abort ();
#endif
  return 0;
}
