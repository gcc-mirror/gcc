/* { dg-do assemble } */
/* { dg-options {-O1 -mno-soft-stack} } */
/* { dg-additional-options -march=sm_30 } */
/* { dg-additional-options -save-temps } */
/* { dg-final { check-function-bodies {** } {} } } */

void f(int s)
{
  char a[s];
}
/*
** f:
** \.visible \.func f \(\.param\.u32 %in_ar0\)
** {
** 	\.reg\.u32 %ar0;
** 	ld\.param\.u32 %ar0, \[%in_ar0\];
** 	ret;
*/
