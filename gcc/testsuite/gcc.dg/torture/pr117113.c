/* { dg-do compile } */
/* { dg-additional-options "-fno-tree-dce -fno-inline" } */

int a, b, c;
volatile int d[1];
void e() {}
void f(int g) {}
int main() {
  int i;
  for (; b; b--) {
    for (i = 0; i < 3; i++) {
      e();
      f(d[0]);
      d[0];
    }
    if (a)
      c++;
  }
  return 0;
}
