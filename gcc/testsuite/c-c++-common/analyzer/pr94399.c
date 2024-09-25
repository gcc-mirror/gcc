/* { dg-skip-if "requires hosted libstdc++ for stdlib free" { ! hostedlib } } */

#include <stdlib.h>

#define _cleanup_(f) __attribute__((cleanup(f)))

static inline void freep(void **p) {
  free(*p);
}

void test(void) {
  _cleanup_(freep) void *ptr;

  ptr = malloc(3);
} /* { dg-bogus "leak" } */
