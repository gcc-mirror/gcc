/* { dg-do assemble } */
/* { dg-options {-O1 -mno-soft-stack} } */
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
** 	{
** 		\.reg\.u64	\1_local;
** 		alloca\.u64	\1_local, 128;
** 		cvta\.local\.u64	\1, \1_local;
** 	}
** 		add\.u64	\2, \1, 15;
** 		and\.b64	\3, \2, -16;
** 	{
** 		\.param\.u64 %out_arg1;
** 		st\.param\.u64 \[%out_arg1\], \3;
** 		call sink, \(%out_arg1\);
** 	}
** 	ret;
*/
