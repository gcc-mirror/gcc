/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-crc" } */

#include <stdint.h>
//in one case is called shift left, in another shift right
uint16_t not_crc(uint16_t crc, uint8_t a) {
    int i;
    crc ^= a;
    for (i = 0; i < 8; ++i) {
        if (crc & 1)
            crc = (crc << 1) ^ 0xA001;
        else
            crc = crc >> 1;
    }
    return crc;
}

/* { dg-final { scan-tree-dump-times "not_crc function maybe calculates CRC" 0 "crc"} } */

