/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbb -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

/* RV64 only*/
unsigned int rol(unsigned int rs1, unsigned int rs2)
{
    int shamt = rs2 & (32 - 1);
    return (rs1 << shamt) | (rs1 >> ((64 - shamt) & (32 - 1)));
}
unsigned int ror(unsigned int rs1, unsigned int rs2)
{
    int shamt = rs2 & (64 - 1);
    return (rs1 >> shamt) | (rs1 << ((32 - shamt) & (32 - 1)));
}

/* { dg-final { scan-assembler-times {\mrolw} 1 } } */
/* { dg-final { scan-assembler-times {\mrorw} 1 } } */
/* { dg-final { scan-assembler-not {\mand} } } */
/* { dg-final { scan-assembler-not {\msext\.w\M} } } */
