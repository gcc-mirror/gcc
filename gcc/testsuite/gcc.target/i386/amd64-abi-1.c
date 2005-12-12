/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && lp64 } } } */
/* { dg-options "-mno-sse" } */

double foo(void) { return 0; }	/* { dg-error "SSE disabled" } */
void bar(double x) { }
