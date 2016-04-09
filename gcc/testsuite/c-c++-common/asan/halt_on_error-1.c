/* Test recovery mode.  */
/* { dg-do run } */
/* { dg-options "-fsanitize-recover=address" } */
/* { dg-set-target-env-var ASAN_OPTIONS "halt_on_error=false" } */

#include <string.h>

volatile int ten = 10;

int main() {
  char x[10];
  __builtin_memset(x, 0, ten + 1);
  asm volatile ("" : : : "memory");
  volatile int res = x[ten];
  x[ten] = res + 3;
  res = x[ten];
  return 0;
}

/* { dg-output "WRITE of size 11 at 0x\[0-9a-f\]+ thread T0\[^\n\r]*(\n|\r\n|\r).*" } */
/* { dg-output "\[^\n\r]*READ of size 1 at 0x\[0-9a-f\]+ thread T0\[^\n\r]*(\n|\r\n|\r).*" } */
/* { dg-output "\[^\n\r]*WRITE of size 1 at 0x\[0-9a-f\]+ thread T0\[^\n\r]*(\n|\r\n|\r).*" } */
/* { dg-output "\[^\n\r]*READ of size 1 at 0x\[0-9a-f\]+ thread T0\[^\n\r]*(\n|\r\n|\r).*" } */
