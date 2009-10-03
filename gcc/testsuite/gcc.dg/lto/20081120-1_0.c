/* { dg-lto-do link } */
/* { dg-lto-options {{-flto -shared}} } */
extern int stat(void) __asm__("" "stat64");
extern inline int stat(void) { }
static void foo(void) { stat(); }
