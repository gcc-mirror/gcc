/* Test for -mmacosx-version-min default on powerpc-darwin.  */
/* { dg-do compile { target powerpc-*-darwin* } } */

int main(void)
{
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ < 1030
  fail me;
#endif
  return 0;
}
