/* Basic test for -mmacosx-version-min switch on Darwin.  */
/* { dg-options "-mmacosx-version-min=10.1 -mmacosx-version-min=10.5" } */
/* { dg-do compile { target *-*-darwin* } } */

int
main ()
{
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ != 1050
  fail me;
#endif
  return 0;
}
