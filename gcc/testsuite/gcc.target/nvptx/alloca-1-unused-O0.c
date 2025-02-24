/* { dg-do assemble } */
/* { dg-options {-O0 -mno-soft-stack} } */
/* { dg-add-options nvptx_alloca_ptx } */
/* { dg-additional-options -save-temps } */
/* { dg-final { check-function-bodies {** } {} } } */

void f(void)
{
  __builtin_alloca(123); /* { dg-warning "ignoring return value of '__builtin_alloca' declared with attribute 'warn_unused_result'" } */
}
/*
** f:
** \.visible \.func f
** {
** 	ret;
*/
