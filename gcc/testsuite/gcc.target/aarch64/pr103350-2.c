/* { dg-do run { target le } } */
/* { dg-additional-options "-O2 -save-temps -fdump-rtl-ree-all -free -std=c99 -w" } */

typedef unsigned char __attribute__((__vector_size__ (8))) v64u8;
typedef unsigned char __attribute__((__vector_size__ (16))) v128u8;
typedef unsigned short __attribute__((__vector_size__ (8))) v64u16;
typedef unsigned short __attribute__((__vector_size__ (64))) v512u16;
typedef unsigned int __attribute__((__vector_size__ (16))) v128u32;
typedef unsigned long long u64;
typedef unsigned long long __attribute__((__vector_size__ (8))) v64u64;
typedef unsigned long long __attribute__((__vector_size__ (16))) v128u64;
typedef unsigned __int128 u128;
typedef unsigned __int128 __attribute__((__vector_size__ (64))) v512u128;
v512u16 foo0_v512u16_0;
u64 foo0_u64_0;
u64 foo0_u16_0;

void
foo0 (v64u16 v64u16_0, v64u64 v64u64_0, u128 u128_0, v64u8 * ret)
{
  /* { 0, 4, 0, 0 } */
  v128u32 v128u32_2 = __builtin_convertvector (v64u16_0, v128u32);
  /* 0 */
  foo0_u16_0 ^= foo0_u64_0 % u128_0;
  /* { 0, ... } */
  foo0_v512u16_0 *=
    __builtin_shufflevector (v64u16_0, v64u16_0, 7, 7, 2, 1, 0, 0, 3, 6, 2, 3,
			     1, 0, 7, 5, 6, 7, 4, 3, 2, 3, 0, 6, 1, 2, 3, 3,
			     6, 7, 6, 2, 4, 3);
  /* { 0, 0, 0, 0, 4, 0, ... } */
  v128u8 v128u8_r = (v128u8) ((v512u128) foo0_v512u16_0)[0] +
    (v128u8) v128u32_2;
  /* { 0, 0, 4, 0, 4, 0, 0, 0 } */
  v64u8 v64u8_r = (v64u8) ((v128u64) v128u8_r)[0] +
    (v64u8) v64u16_0 + (v64u8) v64u64_0;
  *ret = v64u8_r;
}

int
main (void)
{
  v64u8 x, exp = (v64u8){ 0, 0, 4, 0, 4, 0, 0, 0 };
  foo0 ((v64u16){0, 4}, (v64u64){}, 5, &x);
  /*
  for (unsigned i = 0; i < sizeof (x); i++)
    __builtin_printf ("%02x", x[i]);
  */
  for (unsigned i = 0; i < sizeof (x); i++)
    if (x[i] != exp[i]) __builtin_abort();
  return 0;
}

/* { dg-final { scan-rtl-dump {because some vector uses aren't extension} ree } } */
