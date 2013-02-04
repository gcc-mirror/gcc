// Check that we can store lots of stack frames if asked to.

// { dg-do run }
// { dg-skip-if "" { *-*-* } { "-flto" } { "" } }
// { dg-set-target-env-var ASAN_OPTIONS "malloc_context_size=120:redzone=512" }
// { dg-options "-fno-omit-frame-pointer -fno-optimize-sibling-calls" }
// { dg-additional-options "-mno-omit-leaf-frame-pointer" { target { i?86-*-* x86_64-*-* } } }
// { dg-shouldfail "asan" }

#include <stdlib.h>
#include <stdio.h>

template <int depth>
struct DeepFree {
  static void __attribute__((noinline))
  free(char *x) {
    DeepFree<depth - 1>::free(x);
  }
};

template<>
struct DeepFree<0> {
  static void __attribute__((noinline))
  free(char *x) {
    ::free(x);
  }
};

int main() {
  char *x = (char*)malloc(10);
  // deep_free(x);
  DeepFree<200>::free(x);
  return x[5];
}

// { dg-output "ERROR: AddressSanitizer:? heap-use-after-free on address.*(\n|\r\n|\r)" }
// { dg-output "    #37 0x\[0-9a-f\]+ (in \[^\n\r]*DeepFree\[^\n\r]*36|\[(\]).*(\n|\r\n|\r)" }
// { dg-output "    #99 0x\[0-9a-f\]+ (in \[^\n\r]*DeepFree\[^\n\r]*98|\[(\]).*(\n|\r\n|\r)" }
// { dg-output "    #116 0x\[0-9a-f\]+ (in \[^\n\r]*DeepFree\[^\n\r]*115|\[(\])\[^\n\r]*(\n|\r\n|\r)" }
