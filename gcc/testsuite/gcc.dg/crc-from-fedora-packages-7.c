/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -O2" } */

// File - lsf_decode, modified.
// We don't support this case.

#include <stddef.h>
#include <stdint.h>

uint16_t crc(uint8_t byte, uint16_t reg) {
    for (size_t i = 0; i != 8; ++i) {
        uint16_t msb = reg & 0x8000;
        reg = ((reg << 1) & 0xFFFF) | ((byte >> (7 - i)) & 0x0001);
        if (msb) reg ^= 0x5935;
    }
    return reg & 0xFFFF;
}
/* { dg-final { scan-tree-dump "maybe contains CRC calculation." "crc" } } */
