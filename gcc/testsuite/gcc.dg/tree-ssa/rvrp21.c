/* { dg-do compile } */
/* { dg-options "-O1 -fno-tree-vrp -fdisable-tree-evrp -ftree-rvrp -fdump-tree-rvrp" } */

signed char arr[240];
void foo (void)
{

  unsigned short i, length = 200;

  for (i = 1; (int)i < (length - 1); i++)
    arr[i] = -1;
}

/* i_12 = i_6 + 1... i_6 should be [1, 198] on the back edge,  */
/* i_12 should be [2, 199].				   */
/* { dg-final { scan-tree-dump "\\\[2, 199\\\]" "rvrp" } } */
