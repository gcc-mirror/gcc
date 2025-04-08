/* { dg-options "-O2 -fno-strict-aliasing -fdump-tree-dse-details -fno-tree-fre -fdump-tree-optimized" } */

/* PR tree-optimization/87901 */


int i;
int foo ()
{
  i = 0;
  *((short *)&i + 1) = 1;
  return i;
}

/* we should get:
  MEM <char[2]> [(int *)&i] = {};
  MEM[(short int *)&i + 2B] = 1;
  in DSE1.

  Note later on the stores will be merged.  */
/* { dg-final { scan-tree-dump "return 65536;" "optimized" { target le } } } */
/* { dg-final { scan-tree-dump "return 1;" "optimized" { target be } } } */
/* { dg-final { scan-tree-dump "\\\[2\\\]" "dse1" } } */

/* { dg-final { scan-tree-dump-times "Trimming statement " 1 "dse1" } } */


