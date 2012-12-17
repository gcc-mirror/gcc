/* { dg-do run } */
/* { dg-set-target-env-var ASAN_OPTIONS "sleep_before_dying=1" } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O2" } } */
/* { dg-options "-fno-builtin-malloc -fno-builtin-free" } */
/* { dg-shouldfail "asan" } */

#include <stdlib.h>
int main() {
  char *x = (char*)malloc(10);
  free(x);
  return x[5];
}

/* { dg-output "Sleeping for 1 second" } */
