/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc -O2" } */

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

/* { dg-final { scan-tree-dump-times "notCrc function maybe contains CRC calculation" 0 "crc" } } */

