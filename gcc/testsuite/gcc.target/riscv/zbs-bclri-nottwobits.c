/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbs -mabi=lp64" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Og" } } */

int and_two_bit(int idx) {
    return idx & ~3;
}

int and_bclr_two_bit(int idx) {
    return idx & ~(0x4001);
}

/* { dg-final { scan-assembler-times "\tandi\t" 2 } } */
/* { dg-final { scan-assembler-times "\tbclri\t" 1 } } */
