/* Test that most minor versions less than 10 work.  */
/* { dg-options "-mmacosx-version-min=10.5.8" } */
/* { dg-do compile { target *-*-darwin* } } */

int
main ()
{
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ != 1058
  fail me;
#endif
  return 0;
}
