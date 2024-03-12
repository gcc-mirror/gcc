/* { dg-options "-fno-tree-dce -fno-tree-vrp" } */
int a;
int main() {
  int b = 1;
  while (a) {
    short c = b && ((a || a) & (a * c));
  }
  return 0;
}
