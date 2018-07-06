/* PR debug/82718 */
/* { dg-do assemble } */
/* { dg-options "-O2 -gdwarf-5" } */

extern int bar (void);

int
foo (int x)
{
  if (bar ())
    __builtin_abort ();
}
