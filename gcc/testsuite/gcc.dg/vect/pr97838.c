/* { dg-do compile } */

int a, b, c, d;

void f() {
  while (c++) {
    int e = -1;
    d = a ? e / a : e;
    b ^= ~d;
  }
}
