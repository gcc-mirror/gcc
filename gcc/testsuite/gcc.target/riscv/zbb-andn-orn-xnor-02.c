/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zbb -mabi=ilp32" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-g" } } */

unsigned int foo1(unsigned int rs1, unsigned int rs2)
{
return rs1 & ~rs2;
}

unsigned int foo2(unsigned  int rs1, unsigned  int rs2)
{
return rs1 | ~rs2;
}

unsigned int foo3(unsigned int rs1, unsigned int rs2)
{
return rs1 ^ ~rs2;
}

/* { dg-final { scan-assembler-times {\mandn} 2 } } */
/* { dg-final { scan-assembler-times {\morn} 2 } } */
/* { dg-final { scan-assembler-times {\mxnor} 2 } } */
