/* { dg-do run } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-O2 -Wno-psabi" } */

#define MAGIC 0x0706050403020100

typedef unsigned long long u64;
typedef unsigned __int128 v64u128 __attribute__ ((vector_size (64)));

v64u128 __attribute__ ((noinline, noclone))
foo (u64 x1, u64 x2, u64 x3, u64 x4, v64u128 x5)
{
  (void)x1, (void)x2;
  x4 >>= x4 & 63;
  return x3 + x4 + x5;
}

int
main ()
{
  v64u128 x = foo (0, 0, 0, MAGIC, (v64u128) {});
  if (x[0] != MAGIC || x[1] != MAGIC || x[2] != MAGIC || x[3] != MAGIC)
    __builtin_abort();
  return 0;
}
