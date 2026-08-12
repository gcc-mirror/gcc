/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc_zba -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fdump-rtl-sched2-details" } */
/* No tune enables these fusion pairs yet.  */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_ADD_ANDI" 5 "sched2" { xfail *-*-* } } } */

typedef long int64_t;
typedef unsigned long uint64_t;
typedef int int32_t;
typedef unsigned int uint32_t;

/* addi + andi should fuse.  */
int64_t
test_addi_andi (int64_t a)
{
  return (a + 16) & 0x55;
}

/* addiw + andi should fuse.  */
int64_t
test_addiw_andi (int32_t a)
{
  return (int64_t) (a + 5) & 0x55;
}

/* add + andi should fuse.  */
int64_t
test_add_andi (int64_t a, int64_t b)
{
  return (a + b) & 0x55;
}

/* addw + andi should fuse.  */
int64_t
test_addw_andi (int32_t a, int32_t b)
{
  return (int64_t) (a + b) & 0x33;
}

/* add.uw + andi should fuse.  */
uint64_t
test_adduw_andi (uint64_t a, uint64_t b)
{
  return ((uint64_t) (uint32_t) a + b) & -2UL;
}
