/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */
/* Predictive commining is supposed to happen.  */
/* { dg-additional-options "-O3 -fdump-tree-pcom" } */

struct res {
    double r0;
    double r1;
    double r2;
    double r3;
};

struct pxl {
    double v0;
    double v1;
    double v2;
    double v3;
};

#define IS_NAN(x) ((x) == (x))

void fold(struct res *r, struct pxl *in, double k, int sz)
{
  int i;

  for (i = 0; i < sz; i++) {
      if (IS_NAN(k)) continue;
      r->r0 += in[i].v0 * k;
      r->r1 += in[i].v1 * k;
      r->r2 += in[i].v2 * k;
      r->r3 += in[i].v3 * k;
  }
}

/* { dg-final { scan-tree-dump "# r__r0_lsm\[^\r\n\]* = PHI" "pcom" } } */
/* { dg-final { scan-tree-dump "optimized: basic block part vectorized" "slp1" } } */
/* { dg-final { scan-tree-dump "# vect\[^\r\n\]* = PHI" "slp1" } } */
