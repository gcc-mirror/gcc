/* { dg-do compile { target { rv64 } } } */
/* { dg-additional-options "-march=rv64gc_zicond -mabi=lp64d -mbranch-cost=4" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Og" "-Os" "-Oz" } } */

long foo1 (long n) { return n / 4096; }

/* { dg-final { scan-assembler-times {srai\t} 2 } } */
/* { dg-final { scan-assembler-times {srli\t} 1 } } */
/* { dg-final { scan-assembler-times {add\t} 1 } } */
/* { dg-final { scan-assembler-not {czero} } } */

