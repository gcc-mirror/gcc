/* PR rtl-optimization/70467 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */

__uint128_t
foo (__uint128_t x)
{
  return x + ((__uint128_t) 123456 << 64);
}

__uint128_t
bar (__uint128_t x)
{
  return x - ((__uint128_t) 123456 << 64);
}

/* Make sure there are no unnecessary additions with carry.  */
/* { dg-final { scan-assembler-not "adcq\[^\n\r\]*%" } } */
/* { dg-final { scan-assembler-not "sbbq\[^\n\r\]*%" } } */
