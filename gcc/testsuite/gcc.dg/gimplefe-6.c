/* { dg-do compile } */
/* { dg-options "-fgimple" } */

void __GIMPLE () foo ()
{
  int a;
  int b;
  int c;
  int d;

bb_2:
  a = ~b;
  b = a << c;
  c = a & b;
  d = b | c;

bb_3:
  return;
}
