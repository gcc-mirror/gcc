/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

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
  /* Verify that we retain the volatileness on the
     store until after optimization.  */
  volatile struct GTeth_desc *p = &sc->txq_desc[0];
  p->ed_cmdsts = 0;
}

/* { dg-final { scan-tree-dump "{v}" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
