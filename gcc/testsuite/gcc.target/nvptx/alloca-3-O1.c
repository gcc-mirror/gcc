/* { dg-do assemble } */
/* { dg-options {-O1 -mno-soft-stack} } */
/* { dg-add-options nvptx_alloca_ptx } */
/* { dg-additional-options -save-temps } */
/* { dg-final { check-function-bodies {** } {} } } */

void sink(void *);

void *p;

void f(void)
{
  p = __builtin_stack_save();
  sink(__builtin_alloca(25));
  __builtin_stack_restore(p);
}
/*
** f:
** \.visible \.func f
** {
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 		stacksave\.u64	\1;
** 		st\.global\.u64	\[p\], \1;
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
** 	ret;
*/
