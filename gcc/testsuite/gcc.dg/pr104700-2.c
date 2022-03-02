/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-ccp -fno-tree-dce -fno-tree-vrp" } */

int a, b;
int main() {
  int c = 2, d, e = 0;
  if (a)
    e = 2;
  int f, g = -(1L | (e && f && f & e));
  if (g)
  L:
    g = c;
  c = 0;
  d = e * g;
  if (d)
    goto L;
  while (e) {
    int i = (a && b) * i;
  }
  return 0;
}
