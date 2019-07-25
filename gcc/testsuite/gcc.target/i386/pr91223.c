/* { dg-do compile } */
/* { dg-options "-Og -fno-tree-fre" } */
int a;
void fn2(short, short);

void fn1(void) {
  short b[8];
  b[0] |= a & 3;
  b[1] = a;
  fn2(b[0], b[1]);
}
