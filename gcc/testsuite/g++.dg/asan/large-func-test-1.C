// { dg-do run }
// { dg-shouldfail "asan" }

#include <stdlib.h>
__attribute__((noinline))
static void LargeFunction(int *x, int zero) {
  x[0]++;
  x[1]++;
  x[2]++;
  x[3]++;
  x[4]++;
  x[5]++;
  x[6]++;
  x[7]++;
  x[8]++;
  x[9]++;

  x[zero + 111]++;  // we should report this exact line

  x[10]++;
  x[11]++;
  x[12]++;
  x[13]++;
  x[14]++;
  x[15]++;
  x[16]++;
  x[17]++;
  x[18]++;
  x[19]++;
}
volatile int one = 1;
int main() {
  int *x = new int[100];
  LargeFunction(x, one - 1);
  delete x;
}

// { dg-output "ERROR: AddressSanitizer:? heap-buffer-overflow on address\[^\n\r]*" }
// { dg-output "0x\[0-9a-f\]+ at pc 0x\[0-9a-f\]+ bp 0x\[0-9a-f\]+ sp 0x\[0-9a-f\]+\[^\n\r]*(\n|\r\n|\r)" }
// { dg-output "\[^\n\r]*READ of size 4 at 0x\[0-9a-f\]+ thread T0\[^\n\r]*(\n|\r\n|\r)" }
// { dg-output "    #0 0x\[0-9a-f\]+ +(in \[^\n\r]*LargeFunction\[^\n\r]*(large-func-test-1.C:18|\[^\n\r]*:0)|\[(\]).*(\n|\r\n|\r)" }
// { dg-output "\[^\n\r]*0x\[0-9a-f\]+ is located 44 bytes to the right of 400-byte region.*(\n|\r\n|\r)" }
// { dg-output "\[^\n\r]*allocated by thread T0 here:\[^\n\r]*(\n|\r\n|\r)" }
// { dg-output "    #0( 0x\[0-9a-f\]+ +(in _*(interceptor_|)malloc|\[(\])\[^\n\r]*(\n|\r\n|\r)" }
// { dg-output "    #1|) 0x\[0-9a-f\]+ +(in (operator new|(wrap|)_*_Zn\[aw\]\[mj\])|\[(\])\[^\n\r]*(\n|\r\n|\r)" }
