/* { dg-do compile { target x86_64-*-* } } */
/* { dg-options "-mno-sse" } */
/* { dg-require-effective-target lp64 } */

double foo(void) { return 0; }	/* { dg-error "SSE disabled" } */
void bar(double x) { }
