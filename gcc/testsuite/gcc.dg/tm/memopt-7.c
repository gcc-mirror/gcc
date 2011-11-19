/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O -fdump-tree-tmedge --param tm-max-aggregate-size=999" } */

/* Test save/restore pairs for aggregates.  */

struct large { int x[100]; };
extern struct large foobie (void) __attribute__((transaction_safe));
int asdf;

int f()
{
  struct large lala;
  struct large lacopy = foobie();
  __transaction_atomic {
    lala = lacopy;
  }
  return lala.x[asdf];
}

/* { dg-final { scan-tree-dump-times "tm_save.\[0-9_\]+ = lala" 1 "tmedge" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "lala = tm_save" 1 "tmedge" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "tmedge" } } */
