/* { dg-do run } */
/* { dg-set-target-env-var ASAN_OPTIONS "detect_invalid_pointer_pairs=1:halt_on_error=1" } */
/* { dg-options "-fsanitize=address,pointer-compare" } */

int foo(char *p, char *q) {
  return p <= q;
}

char global[8192] = {};
char small_global[7] = {};

int main() {
  // Heap allocated memory.
  char *p = (char *)__builtin_malloc(42);
  int r = foo(p, 0);
  __builtin_free(p);

  p = (char *)__builtin_malloc(1024);
  foo(0, p);
  __builtin_free(p);

  p = (char *)__builtin_malloc(4096);
  foo(p, 0);
  __builtin_free(p);

  // Global variable.
  foo(&global[0], 0);
  foo(&global[1000], 0);

  p = &small_global[0];
  foo(p, 0);

  // Stack variable.
  char stack[10000];
  foo(&stack[0], 0);
  foo(0, &stack[9000]);

  return 0;
}
