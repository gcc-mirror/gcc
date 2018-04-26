/* { dg-do compile } */
/* { dg-options "-O2 -fgimple -fdump-tree-ch2-details" } */

int __GIMPLE (startwith("ch"))
test2 (int n)
{
bb_3:
  if (n_1(D) > 0)
    goto bb_3;
  else
    goto bb_4;

bb_4:
  return;

}

/* { dg-final { scan-tree-dump "is do-while loop" "ch2" } } */
/* { dg-final { scan-tree-dump-not "is not do-while loop" "ch2" } } */
