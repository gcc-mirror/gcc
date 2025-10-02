/* { dg-do compile { target { rv32 } } } */
/* { dg-options "-march=rv32gc_zbb -mabi=ilp32 -O2" } */

unsigned int
foo (unsigned int a)
{
  return (a << 1) | ((a >> 31) ^ 1);
}

 /* { dg-final { scan-assembler {\mrori} } } */
 /* { dg-final { scan-assembler {\mxori} } } */
