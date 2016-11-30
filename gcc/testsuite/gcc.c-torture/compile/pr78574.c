/* PR tree-optimization/78574 */

int a, d, f, g;
int b[1];
short h;
int main() {
  long j;
  int k, i;
  for (; j; j++) {
    i = 0;
    for (; i < 6; i++) {
      int l = a, m = d || g;
    L:
      l ^ m | a;
    }
    b[j + 1] = 2;
    ++k;
    for (; g; g++) {
      d ^= h;
      if (f)
        for (;;)
          ;
    }
  }
  if (k)
    goto L;
}
