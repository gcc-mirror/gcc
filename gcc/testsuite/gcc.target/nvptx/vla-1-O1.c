/* { dg-do assemble } */
/* { dg-options {-O1 -mno-soft-stack} } */
/* { dg-add-options nvptx_alloca_ptx } */
/* { dg-additional-options -save-temps } */
/* { dg-final { check-function-bodies {** } {} } } */

void sink(void *);

void f(int s)
{
  char a[s];
  sink(a);
}
/*
** f:
** \.visible \.func f \(\.param\.u32 %in_ar0\)
** {
** 	\.reg\.u32 %ar0;
** 	ld\.param\.u32 %ar0, \[%in_ar0\];
** 	\.reg\.u32 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 		mov\.u32	\1, %ar0;
** 		cvt\.s64\.s32	\2, \1;
** 		add\.u64	\3, \2, 15;
** 		and\.b64	\4, \3, -16;
** 	{
** 		\.reg\.u64	\5_local;
** 		alloca\.u64	\5_local, \4;
** 		cvta\.local\.u64	\5, \5_local;
** 	}
** 	{
** 		\.param\.u64 %out_arg1;
** 		st\.param\.u64 \[%out_arg1\], \5;
** 		call sink, \(%out_arg1\);
** 	}
** 	ret;
*/
