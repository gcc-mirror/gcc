/* { dg-do assemble } */
/* { dg-options "-O3 --save-temps" } */

#include <stdint.h>

#pragma GCC target "+nosve"

void fun1(int32_t *x, int n)
{
    for (int i = 0; i < (n & -16); i++)
      x[i] = (-x[i]) >> 31;
}

void fun2(int32_t *x, int n)
{
    for (int i = 0; i < (n & -16); i++)
      x[i] = (-x[i]) >> 30;
}

/* { dg-final { scan-assembler-times {\tcmgt\t} 1 } } */
