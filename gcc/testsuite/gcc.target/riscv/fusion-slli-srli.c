/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fdump-rtl-sched2-details" } */
/* No tune enables these fusion pairs yet.  */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_SLLI_SRLI" 2 "sched2" { xfail *-*-* } } } */

typedef unsigned long uint64_t;
typedef unsigned int uint32_t;

/* slli + srli should fuse.  */
uint64_t
test_slli_srli (uint64_t a)
{
  return (a << 4) >> 8;
}

/* slliw + srliw should fuse.  */
uint32_t
test_slliw_srliw (uint32_t a)
{
  return (a << 4) >> 8;
}
