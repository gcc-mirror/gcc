/* { dg-do run { target { riscv_v } } } */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-additional-options " -O0 " } */

#include <stdint.h>

typedef int8_t v4i8 __attribute__((vector_size(4)));
v4i8 g2 = { 7, 0, 8, 70 }, g12;

int main()
{
    g12 = __builtin_shufflevector(g2, g2, 7, 0, 7, 0);
    if (g12[2] != 70)
      __builtin_abort();
    return 0;
}
