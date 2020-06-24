/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-slp2-details -msse2" } */

int x[4];
void foo(int i)
{
  int j = (i+1) & 31;
  x[0] = (x[0] << j) + j;
  x[1] = (x[1] << j) + j;
  x[2] = (x[2] << j) + j;
  x[3] = (x[3] << j) + j;
}

/* We should not use vector operations for i + 1 and (i + 1) & 31 but
   instead use { j, j, j, j }.  */ 
/* { dg-final { scan-tree-dump-times "Building parent vector operands from scalars" 2 "slp2" } } */
/* { dg-final { scan-tree-dump-not " = \{i_" "slp2" } } */
/* { dg-final { scan-tree-dump-times " = \{j_" 1 "slp2" } } */
