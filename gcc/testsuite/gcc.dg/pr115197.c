/* { dg-do compile } */
/* { dg-options "-O1 -fno-tree-scev-cprop -ftree-pre -ftree-loop-distribute-patterns" } */

int a, b[2], c, d, e, f[2];
int main() {
  while (a)
    if (d) {
      if (e)
        return 0;
      for (; c; c++)
        f[c] = 0 < (b[c] = ~(f[c + 1] < a));
    }
  return 0;
}
