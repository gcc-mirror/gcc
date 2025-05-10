/* { dg-do compile { target { rv64 } } } */
/* { dg-options "-march=rv64gb -mabi=lp64d" } */

unsigned long foo(unsigned long src) { return src | 0x8800000000000007; }

/* { dg-final { scan-assembler-times "\\sbseti\t" 2 } } */
/* { dg-final { scan-assembler-times "\\sori\t" 1 } } */

