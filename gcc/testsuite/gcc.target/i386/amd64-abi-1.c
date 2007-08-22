/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-mno-sse" } */

double foo(void) { return 0; }	/* { dg-error "SSE disabled" } */
void bar(double x) { }
