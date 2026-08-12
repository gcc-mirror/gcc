/* { dg-do compile { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-O3" "-O[sgz]" "-flto" } } */
/* { dg-options "-march=rv64g -mabi=lp64d -mtune=xt-c9501fdvt -O2 -fdump-rtl-sched2-details" } */
/* { dg-final { scan-rtl-dump-times "RISCV_FUSE_LDINDEXED" 1 "sched2" { xfail *-*-* } } } */

typedef long int64_t;

extern void use_addr (void *);

/* add + ld should fuse.  */
int64_t
test_add_ld (int64_t *base, long off)
{
  int64_t *p = (int64_t *) ((char *) base + off);
  int64_t v = *p;
  use_addr (p);
  return v;
}
