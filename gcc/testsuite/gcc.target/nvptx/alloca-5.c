/* { dg-do link } */
/* { dg-do run { target nvptx_runtime_alloca_ptx } } */
/* { dg-options {-O2 -mno-soft-stack} } */
/* { dg-add-options nvptx_alloca_ptx } */
/* { dg-additional-options -save-temps } */
/* { dg-final { check-function-bodies {** } {} } } */

/* See also 'gcc.target/nvptx/softstack.c'.  */

static __attribute__((noipa)) int f(int *p)
{
  return __sync_lock_test_and_set(p, 1);
}
/*
** f:
** \.func \(\.param\.u32 %value_out\) f \(\.param\.u64 %in_ar0\)
** {
** 	\.reg\.u32 %value;
** 	\.reg\.u64 %ar0;
** 	ld\.param\.u64 %ar0, \[%in_ar0\];
** 	\.reg\.u32 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 		mov\.u64	\2, %ar0;
** 		atom\.exch\.b32	\1, \[\2\], 1;
** 		membar\.sys;
** 		mov\.u32	%value, \1;
** 	st\.param\.u32	\[%value_out\], %value;
** 	ret;
*/

static __attribute__((noipa)) int g(int n)
{
  /* Check that variable-length stack allocation works.  */
  int v[n];
  v[0] = 0;
  /* Check that atomic operations can be applied to auto data.  */
  return f(v) == 0 && v[0] == 1;
}
/*
** g:
** \.func \(\.param\.u32 %value_out\) g \(\.param\.u32 %in_ar0\)
** {
** 	\.reg\.u32 %value;
** 	\.reg\.u32 %ar0;
** 	ld\.param\.u32 %ar0, \[%in_ar0\];
** 	\.reg\.u32 (%r[0-9]+);
** 	\.reg\.u32 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u64 (%r[0-9]+);
** 	\.reg\.u32 (%r[0-9]+);
** 	\.reg\.u32 (%r[0-9]+);
** 	\.reg\.pred (%r[0-9]+);
** 	\.reg\.u32 (%r[0-9]+);
** 	\.reg\.pred (%r[0-9]+);
** 		mov\.u32	\2, %ar0;
** 		cvt\.s64\.s32	\3, \2;
** 		shl\.b64	\4, \3, 2;
** 		add\.u64	\5, \4, 15;
** 		and\.b64	\6, \5, -16;
** 	{
** 		\.reg\.u64	\7_local;
** 		alloca\.u64	\7_local, \6;
** 		cvta\.local\.u64	\7, \7_local;
** 	}
** 		add\.u64	\8, \7, 3;
** 		and\.b64	\9, \8, -4;
** 		mov\.u32	\10, 0;
** 		st\.u32	\[\9\], \10;
** 	{
** 		\.param\.u32 %value_in;
** 		\.param\.u64 %out_arg1;
** 		st\.param\.u64 \[%out_arg1\], \9;
** 		call \(%value_in\), f, \(%out_arg1\);
** 		ld\.param\.u32	\11, \[%value_in\];
** 	}
** 		setp\.ne\.u32	\12, \11, 0;
** 	@\12	bra	(\$L[0-9]+);
** 		ld\.u32	\13, \[\9\];
** 		setp\.eq\.u32	\14, \13, 1;
** 		selp\.u32	\1, 1, 0, \14;
** 		bra	(\$L[0-9]+);
** \15:
** 		mov\.u32	\1, \10;
** \16:
** 		mov\.u32	%value, \1;
** 	st\.param\.u32	\[%value_out\], %value;
** 	ret;
*/

int main()
{
  if (!g(1))
    __builtin_abort();
  return 0;
}

/* PTX 'atom' isn't acceptable for '.local' memory:
   'operation not supported on global/shared address space' [sic]
   ('CUDA_ERROR_INVALID_ADDRESS_SPACE'), thus FAILs for 'alloca'ed memory.
   We'd have to use the 'nvptx_mem_local_p' replacements, but currently lack a
   mechanism for doing so (TODO).
   { dg-xfail-run-if TODO { *-*-* } } */
