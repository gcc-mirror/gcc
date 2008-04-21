/* { dg-do compile } */
/* { dg-options "-O -fprofile-generate -fprofile-dir=. -fdump-tree-tree_profile" } */
/* { dg-final { scan-tree-dump " ./profile-dir-3.gcda" "tree_profile" } } */

int
main(void)
{
  return 0;
}

/* { dg-final { cleanup-coverage-files } } */
/* { dg-final { cleanup-tree-dump "tree_profile" } } */
