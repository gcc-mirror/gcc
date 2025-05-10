/* { dg-do compile } */
/* { dg-options "-fgimple -O2" } */

void __GIMPLE (ssa,startwith("switchlower1"))
foo (int b)
{
  __BB(2):
  switch (b) {default: L9; case 0: L5; case 5: L5; case 101: L5; }

  __BB(3):
L9:
  switch (b) {default: L7; case 5: L6; case 101: L6; }

  __BB(4):
L6:
  __builtin_unreachable ();

  __BB(5):
L7:
  __builtin_trap ();

  __BB(6):
L5:
  return;

}
