/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2" } */

unsigned long long m;

unsigned long long foo(unsigned long long x, unsigned int y)
{
    return x - y;
}

void bar(unsigned long long x, unsigned int y)
{
    m = x - y;
}

/* { dg-final { scan-assembler-not "xor" } } */
