/* PR target/63810: Test that tiny numbers less than 10 are preserved in
   four-character macros.  */
/* Added by Lawrence Velázquez <vq@larryv.me>.  */

/* { dg-options "-mmacosx-version-min=10.9.1" } */
/* { dg-do compile { target *-*-darwin* } } */

int
main ()
{
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ != 1091
  fail me;
#endif
  return 0;
}
