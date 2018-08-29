/* PR target/70453 */
/* { dg-do assemble { target lp64 } } */
/* { dg-require-effective-target avx512vbmi } */
/* { dg-options "-Og -fschedule-insns -mavx512vbmi" } */


typedef char v64u8 __attribute__ ((vector_size (64)));
typedef short v64u16 __attribute__ ((vector_size (64)));
typedef __int128 v64u128 __attribute__ ((vector_size (64)));

int
foo(v64u8 v64u8_0, v64u16 v64u16_0, v64u128 v64u128_0)
{
  v64u128_0 /= (v64u128){ v64u8_0[28] }  | 0x1424171b0c;
  v64u8_0 %= (v64u8){ v64u16_0[25], v64u128_0[1]}  ;
  v64u128_0 %= (v64u128){ v64u16_0[8] };
  return v64u8_0[0] + v64u8_0[1] + v64u16_0[0] + v64u128_0[0];
}
