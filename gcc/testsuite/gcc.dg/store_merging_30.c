/* PR tree-optimization/94573 */
/* { dg-do compile } */
/* { dg-require-effective-target store_merge } */
/* { dg-options "-O2 -fno-tree-vectorize -fdump-tree-store-merging-details" } */
/* { dg-final { scan-tree-dump "New sequence of 4 stores to replace old one of 8 stores" "store-merging" { target lp64 } } } */

int var[43][12];

void
foo (int x)
{
  var[x][0] = 0;
  var[x][1] = 0;
  var[x][2] = 0;
  var[x][3] = 0;
  var[x][4] = 0;
  var[x][5] = 0;
  var[x][6] = 0;
  var[x][7] = 0;
}
