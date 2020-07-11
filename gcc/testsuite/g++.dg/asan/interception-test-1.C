// ASan interceptor can be accessed with __interceptor_ prefix.

// { dg-do run }
// { dg-options "-fno-builtin-malloc -fno-builtin-free" }
// { dg-additional-options "-D__NO_INLINE__" { target { *-*-linux-gnu *-*-freebsd* } } }
// { dg-shouldfail "asan" }
// { dg-skip-if "Darwin uses mac function interposition" { *-*-darwin* } }

#include <stdlib.h>
#include <stdio.h>

extern "C" long __interceptor_strtol(const char *nptr, char **endptr, int base);
extern "C" long strtol(const char *nptr, char **endptr, int base) {
  fprintf(stderr, "my_strtol_interceptor\n");
  return __interceptor_strtol(nptr, endptr, base);
}

int main() {
  char *x = (char*)malloc(10);
  free(x);
  return (int)strtol(x, 0, 10);
}

// { dg-output "my_strtol_interceptor.*(\n|\r\n|\r)" }
// { dg-output "\[^\n\r]*heap-use-after-free" }
