/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-dce -fno-inline" } */

int a, b, c;
char d(char p) { return p; }
int main () {
  while (c) {
    for (b = 2; b; b--) {
      int *g = &a;
      *g = d((d(*g) >= 128) | b);
    }
    c--;
  }
  return 0;
}
