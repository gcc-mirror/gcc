/* { dg-do compile } */
/* { dg-options "-O -fgimple" } */

int foo (void);

void __GIMPLE (startwith("fre1"))
d ()
{
  int _1;

bb_2:
  _1 = foo ();
  return;
}
