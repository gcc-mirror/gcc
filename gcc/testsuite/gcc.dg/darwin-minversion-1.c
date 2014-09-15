/* Basic test for -mmacosx-version-min switch on Darwin.  */
/* { dg-options "-mmacosx-version-min=10.1" } */
/* { dg-do run { target *-*-darwin* } } */

int
main ()
{
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ != 1010
  fail me;
#endif
  return 0;
}
