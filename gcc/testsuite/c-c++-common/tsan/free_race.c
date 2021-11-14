/* { dg-shouldfail "tsan" } */
/* { dg-additional-options "-fno-ipa-modref" } */

#include <stdlib.h>

void __attribute__((noinline)) foo(int *mem) {
  free(mem);
}

void __attribute__((noinline)) bar(int *mem) {
  mem[0] = 42;
}

int main() {
  int *mem =(int*)malloc (100);
  foo(mem);
  bar(mem);
  return 0;
}

/* { dg-output "WARNING: ThreadSanitizer: heap-use-after-free.*(\n|\r\n|\r)" } */
/* { dg-output "  Write of size 4 at.* by main thread:(\n|\r\n|\r)" } */
/* { dg-output "    #0 bar.*(\n|\r\n|\r)" } */
/* { dg-output "    #1 main.*(\n|\r\n|\r)" } */
/* { dg-output "  Previous write of size 8 at.* by main thread:(\n|\r\n|\r)" } */
/* { dg-output "    #0 free.*(\n|\r\n|\r)" } */
/* { dg-output "    #\(1|2\) foo.*(\n|\r\n|\r)" } */
/* { dg-output "    #\(2|3\) main.*(\n|\r\n|\r)" } */
