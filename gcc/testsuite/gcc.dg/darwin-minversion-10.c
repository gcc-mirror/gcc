/* PR target/63810: Test that an OS X minimum version with zero-padded
   minor and tiny numbers less than 10 produces the correct
   four-character macro.  */
/* Added by Lawrence Velázquez <vq@larryv.me>.  */

/* { dg-options "-mmacosx-version-min=10.07.02" } */
/* { dg-do compile { target *-*-darwin* } } */

int
main ()
{
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ != 1072
  fail me;
#endif
  return 0;
}
