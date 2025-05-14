/* { dg-do assemble } */
/* { dg-options {-O1 -mno-soft-stack} } */
/* { dg-additional-options -march=sm_30 } */
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
