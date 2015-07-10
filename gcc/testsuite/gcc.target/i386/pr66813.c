/* { dg-do compile { target { ia32 } } } */
/* { dg-options "-march=pentium" } */

void f_s(void) { short x; asm("" : "=@ccc"(x)); }
void f_i(void) { int x; asm("" : "=@ccc"(x)); }
