/* { dg-lto-do link } */
/* { dg-skip-if "" { ! { i?86-*-* x86_64-*-* } } } */
/* { dg-lto-options {{-flto -r -nostdlib}} } */
/* { dg-extra-ld-options "-flinker-output=nolto-rel" } */

int f(void) {
  register int ri asm("edi");
  return ri;
}
