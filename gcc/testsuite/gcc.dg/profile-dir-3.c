/* { dg-do compile } */
/* { dg-require-profiling "-fprofile-generate" } */
/* { dg-options "-O -fprofile-generate -fprofile-dir=. -fdump-ipa-cgraph" } */
/* { dg-final { scan-ipa-dump " ./profile-dir-3.gcda" "cgraph" } } */

int
main(void)
{
  return 0;
}

