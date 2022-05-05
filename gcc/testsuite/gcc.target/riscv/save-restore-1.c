/* { dg-do run } */
/* { dg-options "-msave-restore -fomit-frame-pointer" } */

#include <stdlib.h>

__attribute__((noinline)) int g(void) { return 42; }

__attribute__((noinline)) int f(void) {
  asm volatile ("li s0, 0x87654321" ::: "s0");
  return g();
}

int main(void) {
  asm volatile ("li s0, 0x12345678" ::: "s0");

  f();

  long s0;
  asm volatile ("mv %0, s0" : "=r"(s0));

  if (s0 == 0x12345678)
    exit (0);
  else
    abort();
}
