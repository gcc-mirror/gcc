/* { dg-do compile } */
/* { dg-options "-fgimple" } */

void __GIMPLE () foo (int a)
{
bb_2:
  a = *b; /* { dg-error "undeclared" } */

bb_3:
  return;
}
