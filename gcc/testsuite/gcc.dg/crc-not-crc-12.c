/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc -O2" } */

#include <stdint.h>

uint8_t not_crc(uint8_t crc, uint8_t data) {
    uint8_t i;
    crc = crc ^ data;
    for (i = 0; i < 8; i++) {
        if (data & 0x01)
            crc = (crc >> 1) ^ 0x8C;
        else
            crc >>= 1;
    }
    return crc;
}
/* { dg-final { scan-tree-dump-times "not_crc function maybe contains CRC calculation" 0 "crc" } } */
