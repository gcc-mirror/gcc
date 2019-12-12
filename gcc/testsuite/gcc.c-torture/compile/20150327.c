/* { dg-require-effective-target indirect_calls } */

int a;
int (*b)(), (*c)();
int fn1(int p1) {
  if (a)
    return 0;
  if (p1) {
    c();
    b();
  }
}
void fn2() { fn1(0); }
