/* Test that most-minor versions greater than 9 work.  */
/* { dg-options "-mmacosx-version-min=10.4.10" } */
/* { dg-do compile { target *-*-darwin* } } */

int main(void)
{
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ != 1040
  fail me;
#endif
  return 0;
}
