/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-crc" } */

#include <stdint.h>
//no conditional xor
uint16_t not_crc(uint16_t crc, uint8_t a) {
    int i;
    crc ^= a;
    for (i = 0; i < 8; ++i) {
            crc = (crc << 1) ^ 0xA001;
    }
    return crc;
}

/* { dg-final { scan-tree-dump-times "Attention! not_crc function calculates CRC." 0 "crc"} } */

