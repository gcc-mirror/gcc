/* PR target/63810: Test that an OS X minimum version with minor number
   less than 10 and tiny number greater than 9 produces a four-character
   macro with the tiny number clamped to 9.  */
/* Added by Lawrence Velázquez <vq@larryv.me>.  */

/* { dg-options "-mmacosx-version-min=10.9.10" } */
/* { dg-do compile { target *-*-darwin* } } */

int
main ()
{
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ != 1099
  fail me;
#endif
  return 0;
}
