/* { dg-do assemble } */
/* { dg-options {-O3 -mno-soft-stack} } */
/* { dg-additional-options {-march=sm_30 -mfake-ptx-alloca} } */
/* { dg-additional-options -save-temps } */
/* { dg-final { check-function-bodies {** } {} } } */

void sink(void *);

void f(void)
{
  void *p;
  p = __builtin_stack_save();
  sink(__builtin_alloca(25));
  /* { dg-bogus {sorry, unimplemented: dynamic stack allocation not supported} {} { target *-*-* } .-1 } */
  __builtin_stack_restore(p);
  sink(__builtin_alloca(13));
  /* { dg-bogus {sorry, unimplemented: dynamic stack allocation not supported} {} { target *-*-* } .-1 } */
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
** 		call	\(\1\), __GCC_nvptx__PTX_alloca_not_supported, \(32\);
** 		add\.u64	\2, \1, 15;
** 		and\.b64	\3, \2, -16;
** 	{
** 		\.param\.u64 %out_arg1;
** 		st\.param\.u64 \[%out_arg1\], \3;
** 		call sink, \(%out_arg1\);
** 	}
** 		call	\(\4\), __GCC_nvptx__PTX_alloca_not_supported, \(16\);
** 		add\.u64	\5, \4, 15;
** 		and\.b64	\6, \5, -16;
** 	{
** 		\.param\.u64 %out_arg1;
** 		st\.param\.u64 \[%out_arg1\], \6;
** 		call sink, \(%out_arg1\);
** 	}
** 	ret;
*/

/* { dg-final { scan-assembler-times {(?n)^\.extern \.func \(\.param\.u64 %value_out\) __GCC_nvptx__PTX_alloca_not_supported \(\.param\.u64 %in_ar0\);$} 1 } } */
