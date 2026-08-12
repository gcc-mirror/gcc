/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fdump-rtl-sched2-details" } */
/* No tune enables RISCV_FUSE_FLDFST_PAIR_INC yet.  */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_FLDFST_PAIR_INC" 4 "sched2" { xfail *-*-* } } } */

float
test_flw_pair_inc (float *p)
{
  return p[0] + p[1];
}

double
test_fld_pair_inc (double *p)
{
  return p[0] + p[1];
}

void
test_fsw_pair_inc (float *p, float a, float b)
{
  p[0] = a;
  p[1] = b;
}

void
test_fsd_pair_inc (double *p, double a, double b)
{
  p[0] = a;
  p[1] = b;
}
