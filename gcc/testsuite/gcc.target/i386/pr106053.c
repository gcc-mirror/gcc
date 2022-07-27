/* { dg-do run { target lp64 } } */
/* { dg-options "-O -fno-tree-fre -w -mno-avx" } */

typedef unsigned __attribute__((__vector_size__ (32))) v256u8;
typedef unsigned __attribute__((__vector_size__ (64))) v512u8;
typedef unsigned short u16;
typedef unsigned int u32;
typedef unsigned long long u64;
typedef unsigned long __attribute__((__vector_size__ (64))) v512u64;
typedef unsigned __int128 __attribute__((__vector_size__ (32))) v256u128;
unsigned u;
v512u64 foo0_v512u64_0;

static inline v256u8
foo (u32 u32_0, u64 u64_0, v256u128 v256u128_0)
{
  int o = __builtin_add_overflow_p (u64_0, 0, 0);
  v512u64 v512u64_1 =
    foo0_v512u64_0 & (u32) __builtin_sub_overflow_p (0, o, 0);
  u64_0 |= u;
  v256u128 v256u128_2 = u64_0 < v256u128_0;
  v256u128 v256u128_3 = -v256u128_2 == u64_0 * u32_0;
  v256u8 v256u8_r = ((union {
                      v512u8 a; v256u8 b[2];
                      }) (v512u8) v512u64_1).b[0] + (v256u8) v256u128_3;
  return v256u8_r;
}

int
main (void)
{
  v256u8 x = foo (3095179400, 23725760132, (v256u128) { 2, 2 });
  for (unsigned i = 0; i < sizeof (x) / sizeof (x[0]); i++)
    if (x[i])
      __builtin_abort ();
  return 0;
}
