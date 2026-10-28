/* { dg-do compile { target rv64 } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */
/* { dg-options "-march=rv64gc_zbkb -mabi=lp64d" } */

unsigned long f_or  (unsigned long x) { return x | 0xF80000000000003Ful; }
unsigned long f_xor (unsigned long x) { return x ^ 0xF80000000000003Ful; }

/* { dg-final { scan-assembler-times "\tori\t\[a-z0-9\]+,\[a-z0-9\]+,2047" 1 } } */
/* { dg-final { scan-assembler-times "\txori\t\[a-z0-9\]+,\[a-z0-9\]+,2047" 1 } } */
/* { dg-final { scan-assembler-times "\trori\t" 4 } } */
