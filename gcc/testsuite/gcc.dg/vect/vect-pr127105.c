/* { dg-do compile } */
/* { dg-additional-options "-O3 -fno-tree-pre" } */

int a;
char c_0;
void main() {
  int f;
  for (int d = 0; d < 3; d++) {
    f = a ? c_0 : 0;
    c_0 = f;
  }
}
