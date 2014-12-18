/* Test that major versions greater than 9 work and have the additional 0.  */
/* { dg-options "-mmacosx-version-min=10.10.0" } */
/* { dg-do compile { target *-*-darwin* } } */

int
main ()
{
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ != 101000
  fail me;
#endif
  return 0;
}
