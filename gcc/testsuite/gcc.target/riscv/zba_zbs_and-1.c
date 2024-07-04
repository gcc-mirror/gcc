/* { dg-do compile } */
/* { dg-options "-march=rv64gcb -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */


unsigned long long w32mem_1(unsigned long long w32)
{
    return w32 & ~(1U << 0);
}

unsigned long long w32mem_2(unsigned long long w32)
{
    return w32 & ~(1U << 30);
}

unsigned long long w32mem_3(unsigned long long w32)
{
    return w32 & ~(1U << 31);
}

/* If we do synthesis, then we'd see an addi.  */
/* { dg-final { scan-assembler-not "addi\t" } } */
