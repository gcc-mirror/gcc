/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -O2" } */

// File - protocol
#include <stdint.h>
#include <stddef.h>
uint16_t crc16_mcrf4xx(uint16_t crc, uint8_t *data, size_t len) {
    int i;

    if (!data || !len)
        return crc;

    while (len--) {
        crc ^= *data++;
        for (i = 0; i < 8; i++) {
            if (crc & 1)
                crc = (crc >> 1) ^ 0x8408;
            else
                crc = (crc >> 1);
        }
    }

    return crc;
}

/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
