/* { dg-do run } */
/* { dg-options "-fno-builtin-malloc -fno-builtin-free" } */
/* { dg-shouldfail "asan" } */

#include <stdlib.h>
int main() {
  char *x = (char*)malloc(10);
  free(x);
  return x[5];
}

/* { dg-output "heap-use-after-free.*(\n|\r\n|\r)" } */
/* { dg-output "    #0 \[^\n\r]*(in _*(interceptor_|)free|\[(\])\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "    #1 \[^\n\r]*(in _*main (\[^\n\r]*sanity-check-pure-c-1.c:8|\[^\n\r]*:0)|\[(\]).*(\n|\r\n|\r)" } */
/* { dg-output "    #0 \[^\n\r]*(in _*(interceptor_|)malloc|\[(\])\[^\n\r]*(\n|\r\n|\r)" } */
/* { dg-output "    #1 \[^\n\r]*(in _*main (\[^\n\r]*sanity-check-pure-c-1.c:7|\[^\n\r]*:0)|\[(\])\[^\n\r]*(\n|\r\n|\r)" } */
