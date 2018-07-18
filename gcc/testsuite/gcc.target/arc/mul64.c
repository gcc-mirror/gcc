/* { dg-do compile } */
/* { dg-skip-if "MUL64 is ARC600 extension" { ! { arc6xx } } } */
/* { dg-options "-O2 -mmul64" } */

#include <stdint.h>

int64_t i;
int j, k;

int f (void)
{
        i = j * k;
}

/* { dg-final { scan-assembler "mul64" } } */
