/* PR target/63810: Test that an OS X minimum version with minor number
   greater than 9 and no tiny number produces a six-character macro
   ending in "00".  */
/* Added by Lawrence Velázquez <vq@larryv.me>.  */

/* { dg-options "-mmacosx-version-min=10.11" } */
/* { dg-do compile { target *-*-darwin* } } */

int
main ()
{
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ != 101100
  fail me;
#endif
  return 0;
}
