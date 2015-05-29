/* Regression test for PR middle-end/23584 */
/* Verify that dereferencing an absolute address inside of a function
   makes that function impure.  */

/* { dg-do compile } */
/* { dg-options "-O1 -fdump-ipa-pure-const" } */

int test1 (void)
{
  return * (volatile int *) 0x1234;
}

int test2 (void)
{
  int local = * (volatile int *) 0x1234;
  return local;
}

/* { dg-final { scan-ipa-dump-not "found to be pure: test1" "pure-const" } } */
/* { dg-final { scan-ipa-dump-not "found to be pure: test2" "pure-const" } } */
