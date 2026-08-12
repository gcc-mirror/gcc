/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fdump-rtl-sched2-details" } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_LDST_PAIR_INC" 4 "sched2" { xfail *-*-* } } } */

typedef int int32_t;
typedef long int64_t;

int64_t
test_ld_pair_inc (int64_t *p)
{
  return p[0] + p[1];
}

int64_t
test_lw_pair_inc (int32_t *p)
{
  return (int64_t) p[0] + p[1];
}

void
test_sd_pair_inc (int64_t *p, int64_t a, int64_t b)
{
  p[0] = a;
  p[1] = b;
}

void
test_sw_pair_inc (int32_t *p, int32_t a, int32_t b)
{
  p[0] = a;
  p[1] = b;
}
