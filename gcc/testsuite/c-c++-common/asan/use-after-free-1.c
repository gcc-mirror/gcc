/* { dg-do run } */
/* { dg-options "-fno-builtin-malloc -fno-builtin-free" } */
/* { dg-shouldfail "asan" } */

#include <stdlib.h>
int main() {
  char *x = (char*)malloc(10);
  free(x);
  return x[5];
}

/* { dg-output "ERROR: AddressSanitizer:? heap-use-after-free on address\[^\n\r]*" } */
/* { dg-output "0x\[0-9a-f\]+ at pc 0x\[0-9a-f\]+ bp 0x\[0-9a-f\]+ sp 0x\[0-9a-f\]+\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "READ of size 1 at 0x\[0-9a-f\]+ thread T0\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "    #0 0x\[0-9a-f\]+ (in _*main (\[^\n\r]*use-after-free-1.c:9|\[^\n\r]*:0)|\[(\]).*(\n|\r\n|\r)" } */
/* { dg-output "0x\[0-9a-f\]+ is located 5 bytes inside of 10-byte region .0x\[0-9a-f\]+,0x\[0-9a-f\]+\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "freed by thread T0 here:\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "    #0 0x\[0-9a-f\]+ (in _*(interceptor_|)free|\[(\])\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "    #1 0x\[0-9a-f\]+ (in _*main (\[^\n\r]*use-after-free-1.c:8|\[^\n\r]*:0)|\[(\]).*(\n|\r\n|\r)" } */
/* { dg-output "previously allocated by thread T0 here:\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "    #0 0x\[0-9a-f\]+ (in _*(interceptor_|)malloc|\[(\])\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "    #1 0x\[0-9a-f\]+ (in _*main (\[^\n\r]*use-after-free-1.c:7|\[^\n\r]*:0)|\[(\])\[^\n\r]*(\n|\r\n|\r)" } */
