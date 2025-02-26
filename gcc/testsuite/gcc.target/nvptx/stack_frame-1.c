/* GCC used to emit an unused stack frame.  */

/* { dg-do assemble } */
/* { dg-options {-O2 -mno-soft-stack} } */
/* { dg-additional-options -save-temps } */
/* { dg-final { check-function-bodies {** } {} } } */

/* Greatly reduced from libgcc code, where this issue was visible for
   '_divdi3.o:__divti3', '_divmoddi4.o:__divmodti4', '_moddi3.o:__modti3',
   '_udivdi3.o:__udivti3', '_udivmoddi4.o:__udivmodti4',
   '_umoddi3.o:__umodti3'.  */

int f (int n)
{
  const union {
    struct { long low, high; };
    __int128 ll;
  } ww = {{.low = n, .high = 0}};
  return (int) ww.ll;
}
/*
** f:
** \.visible \.func \(\.param\.u32 %value_out\) f \(\.param\.u32 %in_ar0\)
** {
** 	\.reg\.u32 %value;
** 	\.reg\.u32 %ar0;
** 	ld\.param\.u32 %ar0, \[%in_ar0\];
** 		mov\.u32	%value, %ar0;
** 	st\.param\.u32	\[%value_out\], %value;
** 	ret;
*/
