/* PR tree-optimization/44539 */
/* { dg-do compile } */
/* { dg-options "-ftracer -freorder-blocks -O2" } */

void bar (int file);
extern int baz (void);

void noret1 ()
{
  bar (0);
  __builtin_exit (0);
}

void noret2 ()
{
  __builtin_exit (0);
}

void bar (int i)
{
  if (baz ())
    noret1 (i);
}

void foo (int i)
{
  if (~i) bar (i);
  i ? noret1 () : noret2 ();
}
