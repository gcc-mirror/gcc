// { dg-do run }
// { dg-options "-O2" }

#include <stdint.h>
#include <stdio.h>
int16_t a;
static uint32_t *b ;
static uint8_t func_2();
static int32_t func_1() {
  int16_t a = 1;
  func_2(0, a, a);
  return 0;
}
uint8_t func_2(uint32_t p1, uint32_t p2, uint32_t p3) {
  int p = 0;
  for (15;; a++) {
    for (0;;) {
      if (p2)
        break;
      b = &p2;
      return p2;
    }
     p3 = (p2 = p3, p);
  }
  return 0;
}

int main() {
  func_1();
  if (a != 2)
    __builtin_abort ();
  return 0;
}
