/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbb -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-g" } } */

unsigned long foo1(unsigned long rs1, unsigned long rs2)
{
    long shamt = rs2 & (64 - 1);
    return (rs1 << shamt) | (rs1 >> ((64 - shamt) & (64 - 1)));
}
unsigned long foo2(unsigned long rs1, unsigned long rs2)
{
    unsigned long shamt = rs2 & (64 - 1);
    return (rs1 >> shamt) | (rs1 << ((64 - shamt) & (64 - 1)));
}

/* { dg-final { scan-assembler-times {\mrol} 2 } } */
/* { dg-final { scan-assembler-times {\mror} 2 } } */
/* { dg-final { scan-assembler-not {\mand} } } */
