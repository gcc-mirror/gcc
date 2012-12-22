/* { dg-do run } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-set-target-env-var ASAN_OPTIONS "strip_path_prefix='/'" } */
/* { dg-options "-fno-builtin-malloc -fno-builtin-free" } */
/* { dg-shouldfail "asan" } */

#include <stdlib.h>
int main() {
  char *x = (char*)malloc(10);
  free(x);
  return x[5];
}

/* { dg-output "heap-use-after-free.*(\n|\r\n|\r)" } */
/* { dg-output "    #0 0x\[0-9a-f\]+ \[(\]\[^/\]\[^\n\r]*(\n|\r\n|\r)" } */
