/* { dg-do compile } */
/* { dg-options "-fgimple -fdump-tree-ssa" } */

int 
__GIMPLE () *
foo ()
{
  int _1;
  int j;
  int *b;
  _1 = 1;
bb1:
  if (_1)
    goto bb3;
  else
    goto bb2;

bb2:
  b_2 = (int *)0;

bb3:
  b_4 = __PHI (bb1: b_3(D), bb2: b_2);
  return b_4;
}

/* { dg-final { scan-tree-dump-not "_1_" "ssa" } } */
