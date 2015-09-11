/* PR target/63810: Test that an OS X minimum version with outrageous
   zero-padding and a minor number greater than 9 still produces
   a six-character macro.  */
/* Added by Lawrence Velázquez <vq@larryv.me>.  */

/* { dg-options "-mmacosx-version-min=00010.010.0000098" } */
/* { dg-do compile { target *-*-darwin* } } */

int
main ()
{
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ != 101098
  fail me;
#endif
  return 0;
}
