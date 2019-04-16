/* PR rtl-optimization/90082 */
/* { dg-do compile } */
/* { dg-options "-O1 -fnon-call-exceptions -ftrapv" } */

void *buf[5];

void
foo (int a)
{
  if (__builtin_setjmp (buf) == 0)
    __asm__ ("" : : "n" (a * 2));	/* { dg-error "impossible constraint in 'asm'" } */
					/* { dg-warning "asm operand 0 probably doesn't match constraints" "" { target *-*-* } .-1 } */
}
