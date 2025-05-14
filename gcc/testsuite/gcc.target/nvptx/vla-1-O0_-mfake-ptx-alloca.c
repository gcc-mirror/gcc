/* { dg-do assemble } */
/* { dg-options {-O0 -mno-soft-stack} } */
/* { dg-additional-options -march=sm_30 } */
/* { dg-additional-options -mfake-ptx-alloca } */
/* { dg-additional-options -save-temps } */
/* { dg-final { check-function-bodies {** } {} } } */

void sink(void *);

void f(int s)
{
  char a[s];
  /* { dg-bogus {sorry, unimplemented: dynamic stack allocation not supported} {} { target *-*-* } .-1 } */
  sink(a);
}
/*
** f:
** 	...
** 		cvt\.s64\.s32	(%r[0-9]+), (%r[0-9]+);
** 		mov\.u64	(%r[0-9]+), 16;
** 		add\.u64	(%r[0-9]+), \3, -1;
** 		add\.u64	(%r[0-9]+), \1, \4;
** 		div\.u64	(%r[0-9]+), \5, 16;
** 		mul\.lo\.u64	(%r[0-9]+), \6, 16;
** 		call	\((%r[0-9]+)\), __GCC_nvptx__PTX_alloca_not_supported, \(\7\);
** 	...
*/

/* { dg-final { scan-assembler-times {(?n)^\.extern \.func \(\.param\.u64 %value_out\) __GCC_nvptx__PTX_alloca_not_supported \(\.param\.u64 %in_ar0\);$} 1 } } */
