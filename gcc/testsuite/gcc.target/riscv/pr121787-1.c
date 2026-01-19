/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O" { target rv64 } } */
/* { dg-options "-march=rv32gcv -mabi=ilp32 -O" { target rv32 } } */

#include <stdint-gcc.h>
typedef int a;
int32_t b, e;
int32_t **c;
uint8_t d(int32_t *, int32_t *, uint16_t, const int32_t *, int32_t **);
uint32_t f() {
  uint16_t g;
  d(0, &b, g, &b, c);
  e = (__attribute__((__vector_size__(sizeof(a)))) a){}[4];
}
uint8_t d(int32_t *, int32_t *, uint16_t, const int32_t *, int32_t **) {
  asm goto("" : : : : i);
i:
j:
  asm goto("" : : : : j);
}
