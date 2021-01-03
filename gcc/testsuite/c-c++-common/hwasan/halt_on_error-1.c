/* Test recovery mode.  */
/* { dg-do run } */
/* { dg-require-effective-target hwaddress_exec } */
/* { dg-options "-fsanitize-recover=hwaddress" } */
/* { dg-set-target-env-var HWASAN_OPTIONS "halt_on_error=false" } */
/* { dg-shouldfail "hwasan" } */

volatile int sixteen = 16;

int main() {
  char x[16];
  __builtin_memset(x, 0, sixteen + 1);
  asm volatile ("" : : : "memory");
  volatile int res = x[sixteen];
  x[sixteen] = res + 3;
  res = x[sixteen];
  return 0;
}

/* { dg-output "WRITE of size 17 at 0x\[0-9a-f\]+.*" } */
/* { dg-output "READ of size 1 at 0x\[0-9a-f\]+.*" } */
/* { dg-output "WRITE of size 1 at 0x\[0-9a-f\]+.*" } */
/* { dg-output "READ of size 1 at 0x\[0-9a-f\]+.*" } */

