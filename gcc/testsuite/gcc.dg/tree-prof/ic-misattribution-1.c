/* { dg-options "-O2 -fdump-ipa-profile" } */
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

/* { dg-final-use { scan-ipa-dump "hist->count 1 hist->all 1" "profile" } } */
/* { dg-final-use { cleanup-ipa-dump "profile" } } */
