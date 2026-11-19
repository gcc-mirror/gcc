/* { dg-do compile } */
/* { dg-options "-O2 -march=rv32gc_zbkb -mabi=ilp32d" { target rv32 } } */
/* { dg-options "-O2 -march=rv64gc_zbkb -mabi=lp64d" { target rv64 } } */

unsigned long f_and (unsigned long x) { return x & ~0xf000UL; }
unsigned long f_or  (unsigned long x) { return x | ~0xf000UL; }
unsigned long f_xor (unsigned long x) { return x ^ ~0xf000UL; }

/* { dg-final { scan-assembler-times {\mandn\M} 1 } } */
/* { dg-final { scan-assembler-times {\morn\M} 1 } } */
/* { dg-final { scan-assembler-times {\mxnor\M} 1 } } */
