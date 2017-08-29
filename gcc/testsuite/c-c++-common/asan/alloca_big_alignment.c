/* { dg-do run } */
/* { dg-shouldfail "asan" } */

#include <assert.h>

volatile int ten = 10;

__attribute__((noinline)) void foo(int index, int len) {
  volatile char str[len] __attribute__((aligned(128)));
  assert(!((long) str & 127L));
  str[index] = '1'; // BOOM
}

int main() {
  foo(ten, ten);
  return 0;
}

/* { dg-output "WRITE of size 1 at 0x\[0-9a-f\]+ thread T0\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "    #0 0x\[0-9a-f\]+ +(in _*foo(\[^\n\r]*alloca_big_alignment.c:11|\[^\n\r]*:0|\[^\n\r]*\\+0x\[0-9a-z\]*)|\[(\])\[^\n\r\]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*Address 0x\[0-9a-f\]+ is located in stack of thread T0.*(\n|\r\n|\r)" */
/* { dg-output "\[^\n\r]*in foo.*alloca_big_alignment.c.*(\n|\r\n|\r)" */
