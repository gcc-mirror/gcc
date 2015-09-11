/* PR target/63810: Test that an OS X minimum version with a zero-padded
   minor number less than 10 produces a four-character macro.  */
/* Added by Lawrence Velázquez <vq@larryv.me>.  */

/* { dg-options "-mmacosx-version-min=10.08.4" } */
/* { dg-do compile { target *-*-darwin* } } */

int
main ()
{
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ != 1084
  fail me;
#endif
  return 0;
}
