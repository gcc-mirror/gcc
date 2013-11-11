/* { dg-options "-O2 -fdump-ipa-profile" } */

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
/* { dg-final-use { scan-ipa-dump-times "Read tp_first_run: 0" 1 "profile"} } */
/* { dg-final-use { scan-ipa-dump-times "Read tp_first_run: 1" 1 "profile"} } */
/* { dg-final-use { scan-ipa-dump-times "Read tp_first_run: 2" 1 "profile"} } */
/* { dg-final-use { cleanup-ipa-dump "profile" } } */
