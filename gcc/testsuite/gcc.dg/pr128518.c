/* { dg-do run } */
/* { dg-options "-O1" } */
/* { dg-require-effective-target trampolines } */
/* { dg-require-effective-target nonlocal_goto } */

void call(void (*f)(void)) { f(); }

int main(void)
{
  __label__ o1;
  void n1(void) { goto o1; }
  void n2(void)
  {
    __label__ o2;
    int v = 0;
    void n3(void) { v = 1; n1(); goto o2; }
    n3();
  o2:;
  }
  void n4(void) { call(n2); }
  n4();
o1:
  return 0;
}

