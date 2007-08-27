/* { dg-do compile { target i?86-*-mingw32* i?86-*-cygwin* } } */

void
__attribute__ ((fastcall))
f1() { }

void
_fastcall
f2() { }

void
__fastcall
f3() { }

int
__attribute__ ((fastcall))
f4(int x, int y, int z) { }
