/* { dg-do compile { target i?86-*-mingw32* i?86-*-cygwin* } } */

__attribute__((dllexport)) void foo (void) __asm__("_bar");
__attribute__((dllexport)) void foo (void) {}

/* { dg-final { scan-assembler " -export:\[\\\\\"\]*bar" } } */
