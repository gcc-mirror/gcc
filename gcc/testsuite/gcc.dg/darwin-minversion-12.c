/* PR target/63810: Test that an OS X minimum version with outrageous
   zero-padding and a minor number less than 10 still produces
   a four-character macro.  */
/* Added by Lawrence Velázquez <vq@larryv.me>.  */

/* { dg-options "-mmacosx-version-min=010.008.000031" } */
/* { dg-do compile { target *-*-darwin* } } */

int
main ()
{
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ != 1089
  fail me;
#endif
  return 0;
}
