/* { dg-do run } */
/* { dg-require-effective-target p8vector_hw } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-mdejagnu-cpu=power8 -O " } */

/* PR101129: The swaps pass was turning a mult-lopart into a mult-hipart.
   Make sure we aren't doing that anymore.  */

typedef unsigned char u8;
typedef unsigned char __attribute__((__vector_size__ (8))) U;
typedef unsigned char __attribute__((__vector_size__ (16))) V;
typedef unsigned int u32;
typedef unsigned long long u64;
typedef __int128 u128;

u8 g;
U u;

void
foo0 (u32 u32_0, U *ret)
{
  u128 u128_2 = u32_0 * (u128)((V){ 5 } > (u32_0 & 4));
  u64 u64_r = u128_2 >> 64;
  u8 u8_r = u64_r + g;
  *ret = u + u8_r;
}

int
main (void)
{
  U x;
  foo0 (7, &x);
  for (unsigned i = 0; i < sizeof (x); i++)
    if (x[i] != 0) __builtin_abort();
  return 0;
}
