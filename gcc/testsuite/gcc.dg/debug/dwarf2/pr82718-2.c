/* PR debug/82718 */
/* { dg-do assemble } */
/* { dg-options "-O2 -gdwarf-5" } */
/* { dg-skip-if "AIX DWARF5" { powerpc-ibm-aix* } } */

extern int bar (void);

int
foo (int x)
{
  if (bar ())
    __builtin_abort ();
}
