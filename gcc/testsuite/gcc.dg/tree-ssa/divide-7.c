/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

int f(int x) {
  return 1 / x;
}

/* { dg-final { scan-tree-dump-not "1 / x_..D.;" "optimized" } } */
/* { dg-final { scan-tree-dump ".. <= 2 ? x_..D. : 0;" "optimized" } } */
