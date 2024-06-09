/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-loop-if-convert -fno-tree-loop-distribute-patterns" } */

int a, b, c, d[3];
void e() {
  int f, g = 0;
  for (; g < 3; g++) {
    if (f || a || b && c) {
      int h, *i = &h, **j = &i;
    }
    d[g] = 0;
  }
}
