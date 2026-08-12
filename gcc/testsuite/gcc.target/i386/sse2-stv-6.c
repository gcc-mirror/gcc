/* { dg-do compile { target ia32 } } */
/* { dg-options "-m32 -O2 -msse2 -mno-stackrealign -mtune=generic" } */

long long y,z;
unsigned int p;

void foo()
{
    long long t = p;
    t ^= y;
    z = t;
}

/* { dg-final { scan-assembler-not "movl" } } */
/* { dg-final { scan-assembler-not "xorl" } } */
/* { dg-final { scan-assembler-not "punpckldq" } } */

