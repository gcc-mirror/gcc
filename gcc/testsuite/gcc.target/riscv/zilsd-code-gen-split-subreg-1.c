/* { dg-do compile } */
/* { dg-options "-march=rv32i_zilsd -mabi=ilp32" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

long long y;
long long foo(long long x)
{
    return y + x;
}

/* { dg-final { scan-assembler-times "ld\t" 1 } } */
/* { dg-final { scan-assembler-not "lw\t" } } */
