extern int stat(void) __asm__("" "stat64");
extern inline int stat(void) { }
static void foo(void) { stat(); }
