// If user provides his own libc functions, ASan doesn't
// intercept these functions.

// { dg-do run }
// { dg-options "-fno-builtin-malloc -fno-builtin-free" }
// { dg-additional-options "-D__NO_INLINE__" { target { *-*-linux-gnu } } }

#include <stdlib.h>
#include <stdio.h>

extern "C" long strtol(const char *nptr, char **endptr, int base) {
  fprintf(stderr, "my_strtol_interceptor\n");
  return 0;
}

int main() {
  char *x = (char*)malloc(10);
  free(x);
  return (int)strtol(x, 0, 10);
}

// { dg-output "my_strtol_interceptor" }
