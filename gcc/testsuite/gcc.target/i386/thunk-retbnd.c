/* { dg-do compile } */
/* { dg-require-effective-target mpx } */
/* { dg-options "-O2 -fcheck-pointer-bounds -mmpx -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "return &glob," 2 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */

int glob;

int *
test1 (void)
{
  return &glob;
}

int *
test2 (void)
{
  return test1 ();
}
