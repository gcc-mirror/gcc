/* Test that putting an initialized variable in a register works. */

#ifdef __i386__
#define REGISTER asm ("eax")
#elif defined (__arm__)
#define REGISTER asm ("r0")
#else
/* Put examples for other architectures here. */
#define REGISTER
#endif

void f() {
  register int i REGISTER = 3;
}
