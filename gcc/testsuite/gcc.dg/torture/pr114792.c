/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-ccp -fno-tree-copy-prop" } */

int a;
int main() {
  int b = -1, c;
  for (; b >= 0; b++) {
    for (c = 0; c; c++) {
    L:
      while (a)
        if (a)
          goto L;
    }
  }
  return 0;
}
