/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-crc" } */

#include <stdio.h>

typedef unsigned char uint8_t;

uint8_t gencrc(uint8_t *data, size_t len) {
    uint8_t crc = 0xff;
    size_t i, j;
    for (i = 0; i < len; i++) {
        crc ^= data[i];
        for (j = 0; j < 8; j++) {
            if ((crc & 0x80) != 0)
                crc = (uint8_t) ((crc << 1) ^ 0x31);
            else
                crc <<= 1;
        }
    }
    return crc;
}

/* { dg-final { scan-tree-dump "Attention! gencrc function calculates CRC." "crc"} } */
/* { dg-final { scan-tree-dump "Return size is 8" "crc"} } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc"} } */
/* { dg-final { scan-tree-dump "Bit forward" "crc"} } */


