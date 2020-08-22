/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-final { scan-assembler-not "call" } } */

__uint128_t foo (__uint128_t x)
{
  return __builtin_bswap128 (x);
}
