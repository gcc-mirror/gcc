/* { dg-do compile } */
/* { dg-options "-fgimple" } */

int __GIMPLE () foo ()
{
  int a;
  int b;

bb_2:
  b = a_1(D) + 1;
bb_3:
  return b;
}
