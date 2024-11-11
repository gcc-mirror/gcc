/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc -O2" } */

#include <stdint.h>

//no shift in case of xor
uint16_t not_crc16_update(uint16_t crc, uint8_t a) {
	
    int i;
    crc ^= a;
    for (i = 0; i < 8; ++i) {
        if (crc & 1)
            crc = crc ^ 0xA001;
        else
            crc = (crc >> 1);
    }
    return crc;
}

/* { dg-final { scan-tree-dump-times "not_crc16_update function maybe contains CRC calculation" 0 "crc" } } */

