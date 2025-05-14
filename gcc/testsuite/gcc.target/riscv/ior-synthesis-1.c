/* { dg-do compile { target { rv64 } } } */
/* { dg-options "-march=rv64gb -mabi=lp64d" } */

unsigned long foo(unsigned long src) { return src | 0x8c00000000000001; }

/* { dg-final { scan-assembler-times "\\srori\t" 2 } } */
/* { dg-final { scan-assembler-times "\\sori\t" 1 } } */

