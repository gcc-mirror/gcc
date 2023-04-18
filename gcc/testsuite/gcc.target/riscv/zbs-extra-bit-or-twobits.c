/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbs -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

int or_two_bit(int idx) {
    return idx|3;
}

int xor_two_bit(int idx) {
    return idx^3;
}

/* { dg-final { scan-assembler-times "\tori\t" 1 } } */
/* { dg-final { scan-assembler-times "\txori\t" 1 } } */
