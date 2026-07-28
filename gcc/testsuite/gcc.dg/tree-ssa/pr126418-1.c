/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop1" } */

int f_cmp_gt_commuted(int x, int y)
{
  int cmp = x > y;
  return cmp + (-cmp ^ x);
}

/* { dg-final { scan-tree-dump-times "\\(unsigned int\\) " 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "\\(int\\) " 1 "forwprop1" } } */
