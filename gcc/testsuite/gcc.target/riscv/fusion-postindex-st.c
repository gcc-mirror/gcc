/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-funroll-loops" "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fno-tree-loop-distribute-patterns -fno-tree-vectorize -fno-unroll-loops -fdump-rtl-sched2-details" } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_POSTINDEX_ST" 2 "sched2" { xfail *-*-* } } } */

typedef signed char int8_t;

extern void use_ptr (void *);

void
post_sb (int8_t *dst, int8_t value, int n)
{
  for (int i = 0; i < n; ++i)
    {
      *dst = value;
      ++dst;
    }
  use_ptr (dst);
}

void
post_fsd (double *dst, double value, int n)
{
  for (int i = 0; i < n; ++i)
    {
      *dst = value;
      ++dst;
    }
  use_ptr (dst);
}
