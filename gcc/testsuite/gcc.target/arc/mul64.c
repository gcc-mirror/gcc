/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=ARC600 -mmul64" } */
#include <stdint.h>

int64_t i;
int j, k;

int f (void)
{
        i = j * k;
}

/* { dg-final { scan-assembler "mul64" } } */
