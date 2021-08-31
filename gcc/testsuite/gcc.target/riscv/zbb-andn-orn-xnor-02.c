/* { dg-do compile } */
/* { dg-options "-march=rv32gc_zbb -mabi=ilp32 -O2" } */

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

/* { dg-final { scan-assembler-times "andn" 2 } } */
/* { dg-final { scan-assembler-times "orn" 2 } } */
/* { dg-final { scan-assembler-times "xnor" 2 } } */