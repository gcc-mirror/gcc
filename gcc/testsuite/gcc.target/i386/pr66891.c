/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2" } */

__attribute__((__stdcall__)) void fn1();

int a;

static void fn2() {
  for (;;)
    ;
}

void fn3() {
  fn1(0);
  fn2(a == 0);
}
