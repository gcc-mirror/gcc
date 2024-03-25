/* { dg-do compile } */
/* Phi-OPT should be able to optimize this without sinking being invoked. */
/* { dg-options "-O -fdump-tree-phiopt2 -fdump-tree-optimized -fno-tree-sink" } */

char *f(char *a, __SIZE_TYPE__ b) {
  char *d = a + b;
  if (b == 0) return a;
  return d;
}
int sub(int a, int b, int c) {
  int d = a - b;
  if (b == 0) return a;
  return d;
}

/* { dg-final { scan-tree-dump-not "goto" "phiopt2" } } */
/* { dg-final { scan-tree-dump-not "goto" "optimized" } } */
