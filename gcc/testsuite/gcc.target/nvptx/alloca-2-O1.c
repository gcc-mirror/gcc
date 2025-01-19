/* { dg-do run } */
/* { dg-options {-O1 -mno-soft-stack} } */
/* { dg-additional-options -save-temps } */
/* { dg-final { check-function-bodies {** } {} } } */

int
main(void)
{
  return !(__builtin_alloca(100) != __builtin_alloca(10));
}
/*
** main:
** \.visible \.func \(\.param\.u32 %value_out\) main \(\.param\.u32 %in_ar0, \.param\.u64 %in_ar1\)
** {
** 	\.reg\.u32 %value;
** 		mov\.u32	%value, 0;
** 	st\.param\.u32	\[%value_out\], %value;
** 	ret;
*/
