/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-funroll-loops" "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fno-tree-vectorize -fno-unroll-loops -fdump-rtl-sched2-details" } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_POSTINDEX_LD" 2 "sched2" { xfail *-*-* } } } */

typedef signed char int8_t;

extern void use_ptr (void *);

int8_t
post_lb (int8_t *src, int n)
{
  int8_t sum = 0;
  for (int i = 0; i < n; ++i)
    {
      sum += *src;
      ++src;
    }
  use_ptr (src);
  return sum;
}

double
post_fld (double *src, int n)
{
  double sum = 0;
  for (int i = 0; i < n; ++i)
    {
      sum += *src;
      ++src;
    }
  use_ptr (src);
  return sum;
}
