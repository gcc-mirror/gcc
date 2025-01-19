/* { dg-do assemble } */
/* { dg-options {-O3 -mno-soft-stack} } */
/* { dg-add-options nvptx_alloca_ptx } */
/* { dg-additional-options -save-temps } */
/* { dg-final { check-function-bodies {** } {} } } */

void sink(void *);

void f(void)
{
  void *p;
  p = __builtin_stack_save();
  sink(__builtin_alloca(25));
  __builtin_stack_restore(p);
  sink(__builtin_alloca(13));
}
/*
** f:
** .visible .func f
** {
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 		stacksave\.u64	\1;
** 	{
** 		\.reg\.u64	\2_local;
** 		alloca\.u64	\2_local, 32;
** 		cvta\.local\.u64	\2, \2_local;
** 	}
** 		add\.u64	\3, \2, 15;
** 		and\.b64	\4, \3, -16;
** 	{
** 		\.param\.u64 %out_arg1;
** 		st\.param\.u64 \[%out_arg1\], \4;
** 		call sink, \(%out_arg1\);
** 	}
** 		stackrestore\.u64	\1;
** 	{
** 		\.reg\.u64	\5_local;
** 		alloca\.u64	\5_local, 16;
** 		cvta\.local\.u64	\5, \5_local;
** 	}
** 		add\.u64	\6, \5, 15;
** 		and\.b64	\7, \6, -16;
** 	{
** 		\.param\.u64 %out_arg1;
** 		st\.param\.u64 \[%out_arg1\], \7;
** 		call sink, \(%out_arg1\);
** 	}
** 	ret;
*/
