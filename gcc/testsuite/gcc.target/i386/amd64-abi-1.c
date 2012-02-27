/* { dg-do compile { target { ! { ia32 } } } } */
/* { dg-options "-mno-sse" } */
/* { dg-additional-options "-mabi=sysv" { target *-*-mingw* } } */

double foo(void) { return 0; }	/* { dg-error "SSE disabled" } */
void bar(double x) { }
