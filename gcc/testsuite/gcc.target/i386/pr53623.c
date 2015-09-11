/* { dg-do compile { target {! ia32 } } } */
/* { dg-options "-O2 -fdump-rtl-ree" } */


#include <stdint.h>

typedef int (*inst_t)(int64_t rdi, int64_t rsi, int64_t rdx);

int16_t code[256];
inst_t dispatch[256];

void an_inst(int64_t rdi, int64_t rsi, int64_t rdx) {
  rdx = code[rdx];
  uint8_t inst = (uint8_t) rdx;
  rdx >>= 8;
  dispatch[inst](rdi, rsi, rdx);
}

int main(void) {
  return 0;
}

/* { dg-final { scan-rtl-dump "copy needed" "ree" } } */

