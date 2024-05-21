/* PR target/107563.C */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-std=c++2b -O3 -msse2 -mno-sse3" } */
/* { dg-final { scan-assembler-times "psllw" 1 } } */
/* { dg-final { scan-assembler-times "psrlw" 1 } } */
/* { dg-final { scan-assembler-times "por" 1 } } */

using temp_vec_type2 [[__gnu__::__vector_size__(8)]] = char;

void foo2(temp_vec_type2& v) noexcept
{
  v = __builtin_shufflevector(v, v, 1, 0, 3, 2, 5, 4, 7, 6);
}
