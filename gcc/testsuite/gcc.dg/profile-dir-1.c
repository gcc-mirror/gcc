/* { dg-do compile } */
/* { dg-options "-O -fprofile-generate=. -fdump-ipa-profile" } */
/* { dg-final { scan-ipa-dump " ./profile-dir-1.gcda" "profile" } } */

int
main(void)
{
  return 0;
}

/* { dg-final { cleanup-ipa-dump "profile" } } */
