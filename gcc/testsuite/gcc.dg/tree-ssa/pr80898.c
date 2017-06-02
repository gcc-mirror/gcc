/* { dg-do run } */
/* { dg-options "-O2" } */

struct S0 {
  int f0 : 24;
  int f1;
  int f74;
} a, *c = &a;
struct S0 fn1() {
  struct S0 b = {4, 3};
  return b;
}

int main() {
  *c = fn1();

  if (a.f1 != 3)
    __builtin_abort ();
  return 0;
}
