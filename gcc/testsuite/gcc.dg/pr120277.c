/* { dg-do compile } */
/* { dg-options "-O2" } */

int a, b;
int c(int d, long e) {
  switch (d) {
  case 129:
    a = 1;
  case 128:
    break;
  default:
    return 1;
  }
  *(int *)e = 0;
}
void f(int d, long e) { c(d, e); }
void g() {
  int h = b * sizeof(int);
  f(h + 7, h);
}
void main() {}
