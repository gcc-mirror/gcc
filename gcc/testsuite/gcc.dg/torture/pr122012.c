/* { dg-do compile } */
#include <stdlib.h>

void foo();

void test(size_t step) {
  char *buf = malloc(64);
  char *p = buf;
  size_t i;

  for(i = 0; i < 64; ++i) {
    p += 4;
    if (__builtin_object_size (p, 2) != 0)
      foo();
    p += step;
  }
  free(buf);
}
