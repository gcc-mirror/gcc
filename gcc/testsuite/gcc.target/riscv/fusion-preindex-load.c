/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-funroll-loops" "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fdump-rtl-sched2-details" } */
/* No tune enables this fusion pair yet.  */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_LDPREINCREMENT" 2 "sched2" { xfail *-*-* } } } */

typedef signed char int8_t;

int8_t
test_preindex_lb (int8_t *p, int n)
{
  int8_t sum = 0;
  for (int i = 0; i < n; ++i)
    {
      p += 2;
      sum += *p;
    }
  return sum;
}

double
test_preindex_fld (double *p, int n)
{
  double sum = 0;
  for (int i = 0; i < n; ++i)
    {
      p += 2;
      sum += *p;
    }
  return sum;
}
