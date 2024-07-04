/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zicond -mabi=lp64d -mbranch-cost=4" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zicond -mabi=ilp32f -mbranch-cost=4" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-Og" "-Os" "-Oz"} } */

long cond_select_if_zero(long a, long b, long c) {
  return a == 0 ? c : b;
}

long cond_select_if_non_zero(long a, long b, long c) {
  return a != 0 ? c : b;
}

/* { dg-final { scan-assembler-times {add\t}  2 } } */
/* { dg-final { scan-assembler-not {or\t} } } */

