// { dg-do run }
// { dg-skip-if "" { *-*-* }  { "*" } { "-O2" } }
// { dg-options "-fno-builtin-malloc -fno-builtin-free" }
// { dg-shouldfail "asan" }

#include <stdio.h>
#include <stdlib.h>

extern "C"
bool __asan_symbolize(const void *, char *out_buffer, int out_size) {
  snprintf(out_buffer, out_size, "MySymbolizer");
  return true;
}

int main() {
  char *x = (char*)malloc(10);
  free(x);
  return x[5];
}

// { dg-output "MySymbolizer" }
