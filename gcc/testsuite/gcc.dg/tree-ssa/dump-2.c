/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

int f(void)
{
  return 0;
}

/* { dg-final { scan-tree-dump "<bb \[0-9\]> \\\[local count: " "optimized" } } */
