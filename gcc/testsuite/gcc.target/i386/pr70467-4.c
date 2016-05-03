/* PR rtl-optimization/70467 */
/* { dg-do compile { target int128 } } */
/* { dg-options "-O2" } */

__uint128_t
foo (__uint128_t x)
{
  return x + ((__uint128_t) 123456 << 64) + 0x1234567;
}

__uint128_t
bar (__uint128_t x)
{
  return x - ((__uint128_t) 123456 << 64) + 0x1234567;
}

/* Make sure the immediates are not loaded into registers first.  */
/* { dg-final { scan-assembler-not "mov\[lq\]\[ \t\]*.\[0-9-\]" } } */
