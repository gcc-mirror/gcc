/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-crc -march=rv64gc_zbc" } */

#include <stdint.h>

//xor is done in case lsb is 0
int16_t not_crc(int16_t crc, int8_t a) {
    int i;
    crc ^= a;
    for (i = 0; i < 8; ++i) {
        if (crc << 15 == 0)
            crc = (crc >> 1) ^ 0xA001;
        else
            crc = crc >> 1;
    }
    return crc;
}

/* { dg-final { scan-tree-dump-times "not_crc function maybe calculates CRC" 0 "crc"} } */

