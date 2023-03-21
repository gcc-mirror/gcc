/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-crc -march=rv64gc_zbc" } */

#include <stdio.h>

typedef unsigned char uint8_t;

//no xor
uint8_t not_crc(uint8_t *data, size_t len) {
    uint8_t crc = 0xff;
    size_t i, j;
    for (i = 0; i < len; i++) {
        crc ^= data[i];
        for (j = 0; j < 8; j++) {
            if ((crc & 0x80) != 0)
                crc = (uint8_t) ((crc << 1) | 0x31);
            else
                crc <<= 1;
        }
    }
    return crc;
}

/* { dg-final { scan-tree-dump-times "not_crc function maybe calculates CRC" 0 "crc"} } */


