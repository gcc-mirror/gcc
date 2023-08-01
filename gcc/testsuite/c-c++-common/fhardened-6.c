/* { dg-do compile { target *-*-linux* *-*-gnu* } } */
/* { dg-options "-fhardened -O -ftrivial-auto-var-init=uninitialized -fdump-tree-gimple" } */

int
foo ()
{
  int i;
  return i;
}

/* { dg-final { scan-tree-dump-not ".DEFERRED_INIT" "gimple" } } */
/* { dg-warning ".-ftrivial-auto-var-init=zero. is not enabled" "" { target *-*-* } 0 } */
