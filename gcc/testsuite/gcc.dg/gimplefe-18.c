/* { dg-do compile } */
/* { dg-options "-fgimple" } */

int
__GIMPLE (ssa) *
foo ()
{
  int _1;
  int j;
  int *b;

__BB(2):
  _1 = 1;
  goto __BB3;

__BB(3):
  if (_1)
    goto __BB5;
  else
    goto __BB4;

__BB(4):
  b_2 = (int *)0;
  goto __BB5;

__BB(5):
  b_4 = __PHI (__BB3: &j, __BB4: b_2);
  return b_4;
}
