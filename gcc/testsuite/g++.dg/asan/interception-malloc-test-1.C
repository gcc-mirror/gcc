// ASan interceptor can be accessed with __interceptor_ prefix.

// { dg-do run { target *-*-linux* } }
// { dg-options "-fno-builtin-free" }
// { dg-additional-options "-D__NO_INLINE__" { target { *-*-linux-gnu } } }
// { dg-shouldfail "asan" }

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

extern "C" void *__interceptor_malloc(size_t size);
extern "C" void *malloc(size_t size) {
  write(2, "malloc call\n", sizeof("malloc call\n") - 1);
  return __interceptor_malloc(size);
}

int main() {
  char *x = (char*)malloc(10);
  free(x);
  return (int)strtol(x, 0, 10);
}

// { dg-output "malloc call.*(\n|\r\n|\r)" }
// { dg-output "\[^\n\r]*heap-use-after-free" }
