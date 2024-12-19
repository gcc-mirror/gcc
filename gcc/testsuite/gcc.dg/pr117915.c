/* { dg-do compile } */
/* { dg-options "-O3 -fno-tree-copy-prop -fno-tree-vrp" } */

unsigned a;
int b, c;
int main() {
  a = a & b || (c || b) | a;
  return 0;
}
