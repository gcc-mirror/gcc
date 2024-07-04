/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-dce -ftree-loop-distribution" } */

short a, d;
int b, c, f, g, h, i, j[2], o;
__attribute__((const)) int s(char r);
int main() {
  int l, m, k, n;
  if (b) {
    char p;
    for (; p >= 0; p--) {
      int e[] = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0,
                 1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0, 0, 1,
                 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0, 0};
      if (j[p]) {
        int q[1];
        i = o;
        o = q[h];
        if (g)
          n = d;
        m = 4;
        for (; m; m--) {
          if (l)
            k |= c;
          if (a)
            break;
        }
      }
      s(n);
      f |= b;
    }
  }
  return 0;
}
