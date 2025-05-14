/* { dg-do assemble } */
/* { dg-options {-O0 -mno-soft-stack} } */
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
** 	\.local \.align 16 \.b8 %frame_ar\[16\];
** 	\.reg\.u64 %frame;
** 	cvta\.local\.u64 %frame, %frame_ar;
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u32 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 		mov\.u64	\11, 16;
** 		add\.u64	\1, \11, -1;
** 		add\.u64	\2, \1, 123;
** 		div\.u64	\3, \2, 16;
** 		mul\.lo\.u64	\4, \3, 16;
** 	{
** 		\.reg\.u64	\5_local;
** 		alloca\.u64	\5_local, \4;
** 		cvta\.local\.u64	\5, \5_local;
** 	}
** 		add\.u64	\6, \5, 15;
** 		shr\.u64	\7, \6, 4;
** 		shl\.b64	\8, \7, 4;
** 		st\.u64	\[%frame\], \8;
** 		ld\.u64	\9, \[%frame\];
** 		mov\.u32	\10, 0;
** 		st\.u8	\[\9\], \10;
** 	ret;
*/
