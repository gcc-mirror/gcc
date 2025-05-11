/* { dg-add-options vect_early_break } */
/* { dg-additional-options "-O3 -fno-tree-copy-prop -fno-tree-dominator-opts -fno-tree-loop-ivcanon -fno-tree-pre -fno-code-hoisting" } */

int a, b[1];
int main() {
  int c = 0;
  for (; c < 1; c++) {
    while (a)
      c++;
    b[c] = 0;
  }
}
