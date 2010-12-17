/* { dg-do compile } */
/* { dg-options "-O -fprofile-generate -fdump-ipa-tree_profile_ipa" } */
/* { dg-final { scan-ipa-dump "/profile-dir-2.gcda" "tree_profile_ipa" } } */

int
main(void)
{
  return 0;
}

/* { dg-final { cleanup-ipa-dump "tree_profile_ipa" } } */
