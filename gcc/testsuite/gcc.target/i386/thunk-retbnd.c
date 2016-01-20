/* { dg-do compile { target { ! x32 } } } */
/* { dg-options "-O2 -fcheck-pointer-bounds -mmpx -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "return &glob," 2 "optimized" } } */

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
