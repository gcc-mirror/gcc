/* { dg-do compile } */
/* { dg-options "-Wno-pedantic" } */

void bar (void);

void
foo (int x)
{
  __builtin_unwind_init ();
  __builtin_eh_return (x, bar);
}
