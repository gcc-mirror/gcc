/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gcb_zicond -mabi=lp64d" { target { rv64 } } } */
/* { dg-options "-O2 -march=rv32gcb_zicond -mabi=ilp32d" { target { rv32 } } } */

typedef unsigned int uint32_t;
uint32_t ct_select(uint32_t bit, uint32_t a, uint32_t b) {
    uint32_t mask = -(bit & 1);
    return (a & mask) | (b & ~mask);
}

/* { dg-final { scan-assembler-times {\mczero} 2 } } */


