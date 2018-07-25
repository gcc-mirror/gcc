/* { dg-lto-do link } */
/* { dg-lto-options {{-flto -r -nostdlib}} } */
/* { dg-extra-ld-options "-flinker-output=nolto-rel" } */
extern int stat(void) __asm__("" "stat64");
extern inline int stat(void) { }
static void foo(void) { stat(); }
