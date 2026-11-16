/* { dg-do run } */
/* { dg-options "-O0 -std=gnu23" } */
/* { dg-require-effective-target trampolines } */

void call(void (*f)(int), int k) { f(k); }
int main(void) {
  int x = 0;
  void n2(int k) {
    void n3(void) { call(n2, 0); }
    if (k)
      n3();
    else
      x = 42;
  }
  call(n2, 1);
  if (x != 42)
    __builtin_abort();
}
