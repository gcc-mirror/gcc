/* { dg-do compile } */
/* { dg-require-profiling "-fprofile-generate" } */
/* { dg-options "-O -fprofile-generate -fdump-ipa-cgraph" } */
/* { dg-final { scan-ipa-dump "/profile-dir-2.gcda" "cgraph" } } */

int
main(void)
{
  return 0;
}

/* { dg-final { cleanup-ipa-dump "cgraph" } } */
