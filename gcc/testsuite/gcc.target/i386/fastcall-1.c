/* { dg-do compile { target i?86-*-mingw32* i?86-*-cygwin* } } */
/* { dg-options "-std=gnu89" } */

void
__attribute__ ((fastcall))
f1() { }

void
_fastcall
f2() { }

void
__fastcall
f3() { }

void
__attribute__ ((fastcall))
f4(int x, int y, int z) { }

/* Scan for global label with correct prefix and suffix.  */
/* { dg-final { scan-assembler "\.globl\[ \t\]@f4@12" } } */
