/* { dg-do compile } */
/* { dg-options "-O2 -march=corei7" } */

#include <stdlib.h>

int foo(unsigned char a, unsigned char b)
{
    int isum=abs(a - b);
    return isum;
}

/* { dg-final { scan-assembler-not "cmov*" } } */
/* { dg-final { scan-assembler "(cltd|cdq|shr)" } } */
/* { dg-final { scan-assembler-times "xor" 1 } } */
/* { dg-final { scan-assembler-times "sub" 2 } } */

