/* { dg-do compile } */
/* { dg-options "-O -fprofile-generate -fdump-tree-tree_profile" } */
/* { dg-require-host-local "" } */
/* { dg-final { scan-tree-dump "/profile-dir-2.gcda" "tree_profile" } } */

int
main(void)
{
  return 0;
}

/* { dg-final { cleanup-coverage-files } } */
/* { dg-final { cleanup-tree-dump "tree_profile" } } */
