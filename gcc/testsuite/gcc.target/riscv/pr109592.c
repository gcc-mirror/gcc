/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbb -mabi=lp64d" } */

int sextb32(int x) { return (x << 24) >> 24; }

/* { dg-final { scan-assembler-times {sext.b} 1 } } */
/* { dg-final { scan-assembler-not {slli} } } */
/* { dg-final { scan-assembler-not {srai} } } */

