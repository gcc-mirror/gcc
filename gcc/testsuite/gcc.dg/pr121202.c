/* { dg-do compile } */
/* { dg-options "-O3 -fno-tree-copy-prop" } */

int a, b, c;
int e(int f, int g) { return f >> g; }
int h(int f) { return a > 1 ? 0 : f << a; }
int main() {
  while (c--)
    b = e(h(1), a);
  return 0;
}
