/* { dg-do compile } */
/* { dg-options "-O1" } */

unsigned long long or64 (unsigned long long input)
{
    return input | 0x200000004ULL;
}

/* { dg-final { scan-assembler-not "mov\[\\t \]+.+,\[\\t \]*.+" } } */
