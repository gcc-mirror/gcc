/* { dg-do run } */
/* { dg-options {-O0 -mno-soft-stack} } */
/* { dg-additional-options -march=sm_30 } */
/* { dg-additional-options -mfake-ptx-alloca } */
/* { dg-additional-options -save-temps } */

int
main(void)
{
  return !(__builtin_alloca(100) != __builtin_alloca(10));
}
/* { dg-final { scan-assembler-times {(?n)\tcall\t\(%r[0-9]+\), __GCC_nvptx__PTX_alloca_not_supported, \(%r[0-9]+\);$} 2 } } */

/* { dg-final { scan-assembler-times {(?n)^\.extern \.func \(\.param\.u64 %value_out\) __GCC_nvptx__PTX_alloca_not_supported \(\.param\.u64 %in_ar0\);$} 1 } } */

/* { dg-bogus __GCC_nvptx__PTX_alloca_not_supported {unresolved symbol} { target *-*-* } 0 } */

/* { dg-output {GCC/nvptx: sorry, unimplemented: dynamic stack allocation not supported[\r\n]+} }
   { dg-shouldfail __GCC_nvptx__PTX_alloca_not_supported } */
