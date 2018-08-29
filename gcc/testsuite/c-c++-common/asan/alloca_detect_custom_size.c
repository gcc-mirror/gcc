/* { dg-do run } */
/* { dg-shouldfail "asan" } */

#include <assert.h>

struct A {
  char a[3];
  int b[3];
};

volatile int ten = 10;

__attribute__((noinline)) void foo(int index, int len) {
  volatile struct A str[len] __attribute__((aligned(32)));
  assert(!((long) str & 31L));
  str[index].a[0] = '1'; // BOOM
}

int main(int argc, char **argv) {
  foo(ten, ten);
  return 0;
}

/* { dg-output "WRITE of size 1 at 0x\[0-9a-f\]+ thread T0\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "    #0 0x\[0-9a-f\]+ +(in _*foo(\[^\n\r]*alloca_detect_custom_size.c:16|\[^\n\r]*:0|\[^\n\r]*\\+0x\[0-9a-z\]*)|\[(\])\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*Address 0x\[0-9a-f\]+ is located in stack of thread T0.*(\n|\r\n|\r)" */
/* { dg-output "\[^\n\r]*in foo.*alloca_detect_custom_size.c.*(\n|\r\n|\r)" */
