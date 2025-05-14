/* { dg-do assemble } */
/* { dg-options {-O1 -mno-soft-stack} } */
/* { dg-add-options nvptx_alloca_ptx } */
/* { dg-additional-options -save-temps } */
/* { dg-final { check-function-bodies {** } {} } } */

void f(void)
{
  char *a = __builtin_alloca(123);
  a[0] = 0;
}
/*
** f:
** \.visible \.func f
** {
** 	ret;
*/
