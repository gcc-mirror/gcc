/* { dg-options "-O2 -fdump-ipa-profile -fprofile-update=atomic" } */
/* { dg-require-effective-target profile_update_atomic } */

__attribute__ ((noinline))
int foo()
{
  return 0;
}

__attribute__ ((noinline))
int bar()
{
  return 1;
}

int main ()
{
  return foo ();
}
/* { dg-final-use-not-autofdo { scan-ipa-dump-times "Read tp_first_run: 0" 1 "profile"} } */
/* { dg-final-use-not-autofdo { scan-ipa-dump-times "Read tp_first_run: 1" 1 "profile"} } */
/* { dg-final-use-not-autofdo { scan-ipa-dump-times "Read tp_first_run: 2" 1 "profile"} } */
