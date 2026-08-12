/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64g -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fdump-rtl-sched2-details" } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_ADD_ST" 1 "sched2" { xfail *-*-* } } } */

typedef long int64_t;

/* add + sd should fuse.  */
void
test_add_sd (int64_t *base, long off, int64_t v)
{
  *(int64_t *) ((char *) base + off) = v;
}
