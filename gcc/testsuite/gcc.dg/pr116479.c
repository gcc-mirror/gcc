/* PR 116479 */
/* { dg-do run { target { bitint } } } */
/* { dg-additional-options "-O -funroll-loops -finline-stringops -fmodulo-sched --param=max-iterations-computation-cost=637924687 -std=c23" } */

#if __BITINT_MAXWIDTH__ >= 13577
_BitInt (13577) b;

void
foo (char *ret)
{
  __builtin_memset (&b, 4, 697);
  *ret = 0;
}
#endif

int
main ()
{
#if __BITINT_MAXWIDTH__ >= 13577
  char x;
  foo (&x);
  for (unsigned i = 0; i < sizeof (x); i++)
    if (x != 0)
      __builtin_abort ();
#endif
}
