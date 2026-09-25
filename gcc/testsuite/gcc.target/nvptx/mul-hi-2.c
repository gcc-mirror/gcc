/* { dg-do assemble } */
/* { dg-options "-O2 -save-temps" } */

int smul_hi(int x, int y)
{
    return ((long)x*(long)y)>>32;
}

int s2(int x)
{
    return smul_hi(x,2);
}

int s4(int x)
{
    return smul_hi(x,4);
}

unsigned int umul_hi(unsigned int x, unsigned int y)
{
    return ((unsigned long)x * (unsigned long)y) >> 32;
}

unsigned int u2(unsigned int x)
{
    return umul_hi(x,2);
}

unsigned int u4(unsigned int x)
{
    return umul_hi(x,4);
}

/* { dg-final { scan-assembler-times "mul.hi.s32" 1 } } */
/* { dg-final { scan-assembler-times "shr.s32" 2 } } */
/* { dg-final { scan-assembler-times "mul.hi.u32" 1 } } */
/* { dg-final { scan-assembler-times "shr.u32" 2 } } */
