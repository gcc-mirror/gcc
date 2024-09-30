/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fallow-store-data-races -fno-tree-ch -ftree-loop-distribution" } */

static int b;
int *a, c, *d = &c;
int main() {
  int e = 0;
  for (; e < 8; e = (char)(e + 1)) {
    int *f = &b, g[8], h = 0;
    for (; h < 8; h++)
      g[h] = 0;
    --*f != (*d = g[0] || a);
  }
  return 0;
}
