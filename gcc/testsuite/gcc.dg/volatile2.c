/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-gimple -fdump-tree-optimized" } */

struct GTeth_desc
{
  unsigned ed_cmdsts;
};
struct GTeth_softc
{
  struct GTeth_desc txq_desc[32];
};

void foo(struct GTeth_softc *sc)
{
  /* Verify that we retain the cast to (volatile struct GTeth_desc *)
     after gimplification and that we keep the volatileness on the
     store until after optimization.  */
  volatile struct GTeth_desc *p = &sc->txq_desc[0];
  p->ed_cmdsts = 0;
}

/* { dg-final { scan-tree-dump "\\(volatile struct GTeth_desc \\*\\) D" "gimple" } } */
/* { dg-final { scan-tree-dump "{v}" "optimized" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
