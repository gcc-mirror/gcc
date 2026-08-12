/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc_zbkb -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fdump-rtl-sched2-details" } */
/* No tune enables these fusion pairs yet.  */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_LOGIC_LOGIC" 3 "sched2" { xfail *-*-* } } } */

typedef long int64_t;

/* andn + andi should fuse under Zbkb.  */
int64_t
test_zbkb_andn_andi (int64_t a, int64_t b)
{
  return (~a & b) & 0x55;
}

/* orn + andi should fuse under Zbkb.  */
int64_t
test_zbkb_orn_andi (int64_t a, int64_t b)
{
  return (~a | b) & 0x55;
}

/* xnor + andi should fuse under Zbkb.  */
int64_t
test_zbkb_xnor_andi (int64_t a, int64_t b)
{
  return ~(a ^ b) & 0x55;
}
