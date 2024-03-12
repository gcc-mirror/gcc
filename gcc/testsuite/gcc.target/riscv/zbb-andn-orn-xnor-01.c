/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbb -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-g" } } */

unsigned long long foo1(unsigned long long rs1, unsigned long long rs2)
{
return rs1 & ~rs2;
}

unsigned long long foo2(unsigned long long rs1, unsigned long long rs2)
{
return rs1 | ~rs2;
}

unsigned long long foo3(unsigned long long rs1, unsigned long long rs2)
{
return rs1 ^ ~rs2;
}

/* { dg-final { scan-assembler-times {\mandn} 2 } } */
/* { dg-final { scan-assembler-times {\morn} 2 } } */
/* { dg-final { scan-assembler-times {\mxnor} 2 } } */
