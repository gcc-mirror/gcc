/* { dg-do run } */
/* { dg-shouldfail "asan" } */

#include <assert.h>

volatile const int ten = 10;

__attribute__((noinline)) void foo(int index, int len) {
  volatile char str[len] __attribute__((aligned(32)));
  assert(!((long) str & 31L));
  str[index] = '1'; // BOOM
}

int main(int argc, char **argv) {
  foo(33, ten);
  return 0;
}

/* { dg-output "WRITE of size 1 at 0x\[0-9a-f\]+ thread T0\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "    #0 0x\[0-9a-f\]+ +(in _*foo(\[^\n\r]*alloca_overflow_right.c:11|\[^\n\r]*:0|\[^\n\r]*\\+0x\[0-9a-z\]*)|\[(\])\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*Address 0x\[0-9a-f\]+ is located in stack of thread T0.*(\n|\r\n|\r)" */
/* { dg-output "\[^\n\r]*in foo.*alloca_overflow_right.c.*(\n|\r\n|\r)" */
