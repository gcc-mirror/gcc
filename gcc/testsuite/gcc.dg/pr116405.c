/* PR rtl-optimization/116405 */
/* { dg-do run } */
/* { dg-options "-O2 -fno-ssa-phiopt -fno-tree-dce" } */ 

int printf(const char *, ...);
int a, b = 2, c = 1;
unsigned d, e;
int main() {
 L:
  a = -1 / c;
  d = ~(b && (c && ~e) & b);
  printf("0\n");
  c = 0;
  if (d != -1)
    goto L;
  return 0;
}
