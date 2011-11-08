/* { dg-do compile } */
/* { dg-options "-fgnu-tm -fdump-tree-optimized -O2" } */

static void candy() { candycane(); }

static void tootsie_roll () __attribute__((transaction_wrap (candy)));
static void tootsie_roll () { bark(); }

void foo()
{
  __transaction_relaxed { candy(); }
}

/* { dg-final { scan-tree-dump-times "candy" 0 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
