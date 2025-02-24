/* { dg-do assemble } */
/* { dg-options {-O0 -mno-soft-stack} } */
/* { dg-additional-options -march=sm_30 } */
/* { dg-additional-options -mfake-ptx-alloca } */
/* { dg-additional-options -save-temps } */
/* { dg-final { check-function-bodies {** } {} } } */

void sink(void *);

void f(void)
{
  sink(__builtin_alloca(123));
  /* { dg-bogus {sorry, unimplemented: dynamic stack allocation not supported} {} { target *-*-* } .-1 } */
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
** 		call	\(\6\), __GCC_nvptx__PTX_alloca_not_supported, \(\5\);
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

/* { dg-final { scan-assembler-times {(?n)^\.extern \.func \(\.param\.u64 %value_out\) __GCC_nvptx__PTX_alloca_not_supported \(\.param\.u64 %in_ar0\);$} 1 } } */
