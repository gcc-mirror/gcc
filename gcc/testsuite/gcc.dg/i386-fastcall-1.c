/* { dg-do compile { target i386-pc-mingw32* i386-pc-cygwin* } } */

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
