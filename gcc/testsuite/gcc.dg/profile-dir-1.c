/* { dg-do compile } */
/* { dg-require-profiling "-fprofile-generate" } */
/* { dg-options "-O -fprofile-generate=. -fdump-ipa-cgraph" } */
/* { dg-final { scan-ipa-dump "Using data file \.\/.*#profile-dir-1.gcda" "cgraph" } } */

int
main(void)
{
  return 0;
}

