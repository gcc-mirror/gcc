/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-expand-details" } */

#include <stdint.h>

int8_t b[1];
int8_t *d = b;
int32_t c;

int main() {
  b[0] = -40;
  uint16_t t = (uint16_t)d[0];

  c = (t < 0xFFF6 ? t : 0xFFF6) + 9;

  if (c != 65505)
    __builtin_abort ();
}

/* { dg-final { scan-rtl-dump-times ".SAT_ADD " 2 "expand" } } */
