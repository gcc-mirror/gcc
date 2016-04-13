/* { dg-do run } */
/* { dg-options "-fno-builtin-malloc -fno-builtin-free" } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O0" } } */
/* { dg-shouldfail "asan" } */

#include <stdio.h>
#include <stdlib.h>

struct Simple {
  int value;
};

int f(struct Simple simple) {
  return simple.value;
}

int main() {
  struct Simple *psimple = (struct Simple *) malloc(sizeof(struct Simple));
  psimple->value = 42;
  free(psimple);
  printf("%d\n", f(*psimple));
  return 0;
}

/* { dg-output "ERROR: AddressSanitizer:? heap-use-after-free on address\[^\n\r]*" } */
/* { dg-output "0x\[0-9a-f\]+ at pc 0x\[0-9a-f\]+ bp 0x\[0-9a-f\]+ sp 0x\[0-9a-f\]+\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*READ of size 4 at 0x\[0-9a-f\]+ thread T0\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "    #0 0x\[0-9a-f\]+ +(in _*main (\[^\n\r]*pr70541.c:21|\[^\n\r]*:0)|\[(\]).*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*freed by thread T0 here:\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "    #0 0x\[0-9a-f\]+ +(in _*(interceptor_|wrap_|)free|\[(\])\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "    #1 0x\[0-9a-f\]+ +(in _*main (\[^\n\r]*pr70541.c:20|\[^\n\r]*:0)|\[(\]).*(\n|\r\n|\r)" } */
/* { dg-output "\[^\n\r]*previously allocated by thread T0 here:\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "    #0 0x\[0-9a-f\]+ +(in _*(interceptor_|wrap_|)malloc|\[(\])\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "    #1 0x\[0-9a-f\]+ +(in _*main (\[^\n\r]*pr70541.c:18|\[^\n\r]*:0)|\[(\])\[^\n\r]*(\n|\r\n|\r)" } */
