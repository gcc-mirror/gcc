// { dg-do compile }
// { dg-options "-O2" }

#include <stdint.h>
uint8_t a;
void
b(int8_t c) {
  int d;
e:
  uint32_t f;
  for (;;)
    for (c = 10; c; c++)
      if (0 > (a = c) ?: d) {
        f = a;
        goto e;
      }
}
