/* { dg-do compile { target { riscv64*-*-* } } } */
/* { dg-options "-O2 -march=rv64gc_zicfiss -mabi=lp64d -fcf-protection=return" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */
struct ad {
  void *ae;
};
struct af {
  union {
    int *ai;
    int *aj;
    struct ad *ak;
  } u;
  struct {
    struct {
      long al : 1;
      long am : 1;
      long : 21;
    } b;
    long i;
  } s;
};
void fdes (struct af *, void *, long *);

void foo (struct af *bv, long *bw) {
  bw[0] = bw[1] = 0;
  if (bv->s.b.al)
    fdes (bv, bv->u.ak->ae, bw);
  else if (bv->s.b.am) {
    int **p = (int**)bv->u.aj;
    for (; *p; ++p)
     fdes (bv, *p, bw);
  } else
    fdes (bv, bv->u.ai, bw);
}

/* { dg-final { scan-assembler-times "ld\tt0" 1 } } */
/* { dg-final { scan-assembler-times "sspopchk\tt0" 1 } } */
/* { dg-final { scan-assembler-times "jr\tt0" 1 } } */
/* { dg-final { scan-assembler-times "ld\tra" 2 } } */
/* { dg-final { scan-assembler-times "sspopchk\tra" 2 } } */
/* { dg-final { scan-assembler-times "tail" 2 } } */
