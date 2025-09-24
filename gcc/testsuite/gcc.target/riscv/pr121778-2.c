/* { dg-do compile { target { rv64 } } } */
/* { dg-options "-march=rv64gc_zbb -mabi=lp64 -O2" } */

unsigned long
foo (unsigned long a)
{
  return (a << 1) | ((a >> 63) ^ 1);
}

 /* { dg-final { scan-assembler {\mrori} } } */
 /* { dg-final { scan-assembler {\mxori} } } */
