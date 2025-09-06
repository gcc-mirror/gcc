/* { dg-do compile } */
/* { dg-options "-O2 -march=rv64gc_zbb -mabi=lp64" } */

#include <stdint-gcc.h>

int8_t foo1(uint8_t a) {
    return a;
}

int16_t foo2(uint16_t a) {
    return a;
}

/* { dg-final { scan-assembler "sext.b" } } */
/* { dg-final { scan-assembler "sext.h" } } */
