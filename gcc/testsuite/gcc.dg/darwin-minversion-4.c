/* Test that minor versions greater than 9 produce a six-character macro.  */
/* { dg-options "-mmacosx-version-min=10.10.1" } */
/* { dg-do compile { target *-*-darwin* } } */

int
main ()
{
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ != 101001
  fail me;
#endif
  return 0;
}
