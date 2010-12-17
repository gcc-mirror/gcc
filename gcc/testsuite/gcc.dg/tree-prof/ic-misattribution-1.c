/* { dg-options "-O2 -fdump-ipa-tree_profile_ipa" } */
/* { dg-additional-sources "ic-misattribution-1a.c" } */

extern void other_caller (void);

void
callee (void)
{
  return;
}

void
caller(void (*func) (void))
{
  func ();
}

/* { dg-final-use { scan-ipa-dump "hist->count 1 hist->all 1" "tree_profile_ipa" } } */
/* { dg-final-use { cleanup-ipa-dump "tree_profile_ipa" } } */
