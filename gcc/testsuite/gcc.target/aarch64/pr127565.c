/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef unsigned int u32;
typedef unsigned long long u64;
typedef unsigned __int128 u128;

__attribute__((noinline)) u32
sat_mul_shift (u32 a, u32 b)
{
  u128 product = (u128) a * b;
  return product >> 32 ? -1 : (u32) product;
}

__attribute__((noinline)) u32
sat_mul_min (u32 a, u32 b)
{
  u128 product = (u128) a * b;
  return product > (u128) -1U ? -1 : (u32) product;
}

__attribute__((noinline)) u32
sat_mul_builtin (u32 a, u32 b)
{
  u32 product;
  return __builtin_umul_overflow (a, b, &product) ? -1 : product;
}

__attribute__((noinline)) u32
sat_mul_u64_shift (u32 a, u32 b)
{
  u64 product = (u64) a * b;
  return product >> 32 ? -1 : (u32) product;
}

__attribute__((noinline)) u32
sat_mul_u64_min (u32 a, u32 b)
{
  u64 product = (u64) a * b;
  return product > (u64) -1U ? -1 : (u32) product;
}

/* { dg-final { scan-assembler-times {umull\tx[0-9]+, w[0-9]+, w[0-9]+} 5 } } */
/* { dg-final { scan-assembler-times {tst\tx[0-9]+,} 5 } } */
/* { dg-final { scan-assembler-times {csinv\tw[0-9]+, w[0-9]+, wzr, eq} 5 } } */
