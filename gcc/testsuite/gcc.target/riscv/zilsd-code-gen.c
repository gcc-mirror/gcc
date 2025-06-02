/* { dg-do compile } */
/* { dg-options "-march=rv32i_zilsd -mabi=ilp32" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

long long foo1(long long *a)
{
    return *a;
}

long long g;

void foo2(long long a)
{
    g = a;
}

/* { dg-final { scan-assembler-times "ld\t" 1 } } */
/* { dg-final { scan-assembler-times "sd\t" 1 } } */
