/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-crc -march=rv64gc_zbc" } */

#include <stdint.h>

typedef uint8_t crc;
#define TOPBIT (1 << 7)

//shift not by one
crc
notCrc(uint8_t const message[], int nBytes) {
    crc remainder = 0;
    for (int byte = 0; byte < nBytes; ++byte) {
        remainder ^= message[byte] ;
        for (uint8_t bit = 8; bit > 0; --bit) {
            if (remainder & TOPBIT) {
                remainder = (remainder << 3) ^ 1234;
            } else {
                remainder = (remainder << 9);
            }
        }
    }
    return (remainder);
}

/* { dg-final { scan-tree-dump-times "notCrc function maybe calculates CRC" 0 "crc"} } */

