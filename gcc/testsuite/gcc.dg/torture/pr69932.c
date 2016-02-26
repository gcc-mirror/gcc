/* { dg-do compile } */

int a;
void fn1() {
  int b = 4;
  short c[4];
  c[b] = c[a];
  if (c[2]) {}

}
