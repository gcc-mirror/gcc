/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64gc_zba -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fdump-rtl-sched2-details" } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_ANDI_ADD" 3 "sched2" } } */

typedef long int64_t;
typedef int int32_t;

/* andi + addi should fuse.  */
int64_t
test_andi_addi (int64_t a)
{
  return (a & 0x55) + 1;
}

/* andi + add should fuse.  */
int64_t
test_andi_add (int64_t a, int64_t b)
{
  return (a & 0x55) + b;
}

/* andi + addw should fuse.  */
int64_t
test_andi_addw (int64_t a, int32_t b)
{
  return (int64_t) ((int32_t) (a & 0x55) + b);
}
