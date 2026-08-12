/* Verify RISCV_FUSE_LDINDEXED correctly matches add+ld pairs.
   This exercises the removal of the duplicate REG_P check.  */
/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Og" "-Os" "-Oz" "-flto" } } */
/* { dg-options "-march=rv64gc -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fdump-rtl-sched1" } */
/* { dg-final { scan-rtl-dump "RISCV_FUSE_LDINDEXED" "sched1" } } */

long
load_indexed (long *base, long idx)
{
  return base[idx];
}
