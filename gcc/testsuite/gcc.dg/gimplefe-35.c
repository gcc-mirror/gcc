/* { dg-do compile } */
/* { dg-options "-fgimple" } */

struct X { int i; } x;
void __GIMPLE foo (void)
{
bb_2:
  x = _Literal (struct X) {};
  return;
}

