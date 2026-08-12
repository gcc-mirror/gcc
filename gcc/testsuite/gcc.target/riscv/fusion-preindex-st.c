/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-funroll-loops" "-flto" } } */
/* { dg-options "-march=rv64gc_zfh -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fdump-rtl-sched2-details" } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_PREINDEX_ST" 2 "sched2" { xfail *-*-* } } } */

typedef signed char int8_t;
/* Pre-increment store of a byte should fuse.  */
void
test_preindex_sb (int8_t *p, int8_t value, int n)
{
  for (int i = 0; i < n; ++i)
    {
      p += 2;
      *p = value;
    }
}

/* Pre-increment store of a double should fuse.  */
void
test_preindex_fsd (double *p, double value, int n)
{
  for (int i = 0; i < n; ++i)
    {
      p += 2;
      *p = value;
    }
}
