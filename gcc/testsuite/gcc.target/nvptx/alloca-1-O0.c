/* { dg-do assemble } */
/* { dg-options {-O0 -mno-soft-stack} } */
/* { dg-add-options nvptx_alloca_ptx } */
/* { dg-additional-options -save-temps } */
/* { dg-final { check-function-bodies {** } {} } } */

void sink(void *);

void f(void)
{
  sink(__builtin_alloca(123));
}
/*
** f:
** \.visible \.func f
** {
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 		mov\.u64	\11, 16;
** 		add\.u64	\2, \11, -1;
** 		add\.u64	\3, \2, 123;
** 		div\.u64	\4, \3, 16;
** 		mul\.lo\.u64	\5, \4, 16;
** 	{
** 		\.reg\.u64	\6_local;
** 		alloca\.u64	\6_local, \5;
** 		cvta\.local\.u64	\6, \6_local;
** 	}
** 		add\.u64	\7, \6, 15;
** 		shr\.u64	\8, \7, 4;
** 		shl\.b64	\9, \8, 4;
** 		mov\.u64	\1, \9;
** 		mov\.u64	\10, \1;
** 	{
** 		\.param\.u64 %out_arg1;
** 		st\.param\.u64 \[%out_arg1\], \10;
** 		call sink, \(%out_arg1\);
** 	}
** 	ret;
*/
