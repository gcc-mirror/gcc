/* { dg-do compile { target arm*-*-symbianelf* } } */
/* { dg-options "-O2" } */

/* Symbian OS requires that builtins not be expanded by default.  Make
   sure that a reference to "strlen" is emitted.  */
/* { dg-final { scan-assembler "strlen" } } */

int f() {
  return strlen("abc");
}
