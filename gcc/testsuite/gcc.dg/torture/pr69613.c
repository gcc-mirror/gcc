/* PR target/69613.  */
/* { dg-do run { target int128 } } */
/* { dg-options "-w -Wno-psabi" } */
/* { dg-additional-options "-mavx" { target avx_runtime } } */

typedef unsigned short u16;
typedef unsigned short v32u16 __attribute__ ((vector_size (32)));
typedef unsigned int u32;
typedef unsigned int v32u32 __attribute__ ((vector_size (32)));
typedef unsigned long long u64;
typedef unsigned long long v32u64 __attribute__ ((vector_size (32)));
typedef unsigned __int128 u128;
typedef unsigned __int128 v32u128 __attribute__ ((vector_size (32)));

u128 __attribute__ ((noinline, noclone))
foo (u32 u32_0, u64 u64_1, u128 u128_1, v32u16 v32u16_0, v32u128 v32u128_0,
     v32u16 v32u16_1, v32u32 v32u32_1, v32u64 v32u64_1, v32u128 v32u128_1)
{
  u128 temp = (v32u128_1[0] << ((-u32_0) & 127));
  u32 t2 = (u32_0 & 127);
  v32u128_1[0] = (v32u128_1[0] >> t2);

  v32u128_1[0] ^= temp;
  v32u128_1 |= (v32u128){ v32u128_0[1] };

  return u64_1 + u128_1 + v32u16_0[0] + v32u16_0[1] + v32u16_1[11]
	 + v32u16_1[12] + v32u16_1[13] + v32u32_1[0] + v32u32_1[1]
	 + v32u32_1[2] + v32u64_1[1] + v32u64_1[2] + v32u64_1[3] + v32u128_1[0]
	 + v32u128_1[1];
}

int
main ()
{
  u128 x
      = foo (1, 1, 1, (v32u16){ 1, 1, 1 }, (v32u128){ 1 }, (v32u16){ 1, 1, 1 },
	     (v32u32){ 1 }, (v32u64){ 1, 1, 1 }, (v32u128){ -1 });
  if (x != 6)
    __builtin_abort ();
  return 0;
}
