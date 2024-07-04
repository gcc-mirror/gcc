/* { dg-do compile } */

extern void c();
int a, b;
int main() {
  while (b) {
    int d, e = 0, *f = &a;
    *f = 1;
    e = 1 >> d ? : 1 << d;
    if (e)
      a = 0;
    c();
  }
  return 0;
}
