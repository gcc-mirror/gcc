/* { dg-do run } */
/* { dg-require-effective-target int32 } */
/* { dg-options "-O2" } */

#include <stdint.h>

int16_t b[1];
int16_t *d = b;
int64_t c;

int main() {
  b[0] = -40;
  uint32_t t = (uint32_t)d[0];

  c = (t < 0xFFFFFFF6u ? t : 0xFFFFFFF6u) + 9;

  if (c != 4294967265)
    __builtin_abort ();
}
