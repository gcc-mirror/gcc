/* { dg-do assemble } */
/* { dg-options {-O0 -mno-soft-stack} } */
/* { dg-add-options nvptx_alloca_ptx } */
/* { dg-additional-options -save-temps } */
/* { dg-final { check-function-bodies {**} {} } } */

void sink(void *);

void f(int s)
{
  char a[s];
  sink(a);
}
/*
** f:
**	...
**		cvt\.s64\.s32	(%r[0-9]+), (%r[0-9]+);
**		mov\.u64	(%r[0-9]+), 16;
**		add\.u64	(%r[0-9]+), \3, -1;
**		add\.u64	(%r[0-9]+), \1, \4;
**		div\.u64	(%r[0-9]+), \5, 16;
**		mul\.lo\.u64	(%r[0-9]+), \6, 16;
** 	{
** 		\.reg\.u64	(%r[0-9]+)_local;
** 		alloca\.u64	\8_local, \7;
** 		cvta\.local\.u64	\8, \8_local;
** 	}
**	...
*/
