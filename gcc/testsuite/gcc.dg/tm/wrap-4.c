/* { dg-do compile } */
/* { dg-options "-fgnu-tm -fdump-tree-optimized -O2" } */

void bark (void);
void candycane (void);
static void candy() { candycane(); }

static void tootsie_roll () __attribute__((transaction_wrap (candy)));
static void tootsie_roll () { bark(); }

void foo()
{
  __transaction_relaxed { candy(); }
}

/* We still have one call to candy()-- on the uninstrumented path
   everything is as usual.  */
/* { dg-final { scan-tree-dump-times "candy \\(\\);" 1 "optimized" } } */

