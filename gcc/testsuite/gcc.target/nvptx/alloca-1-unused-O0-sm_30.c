/* { dg-do assemble } */
/* { dg-options {-O0 -mno-soft-stack} } */
/* { dg-additional-options -march=sm_30 } */
/* { dg-additional-options -save-temps } */
/* { dg-final { check-function-bodies {** } {} } } */

void f(void)
{
  __builtin_alloca(123); /* { dg-warning "ignoring return value of '__builtin_alloca' declared with attribute 'warn_unused_result'" } */
  /* { dg-bogus {sorry, unimplemented: dynamic stack allocation not supported} {} { target *-*-* } .-1 } */
}
/*
** f:
** \.visible \.func f
** {
** 	ret;
*/
