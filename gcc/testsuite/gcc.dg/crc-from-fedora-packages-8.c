/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details -O2" } */
/* { dg-require-effective-target int32plus } */

#include <stdint.h>
#include <stddef.h>
// File - misc
uint32_t _crc32(uint32_t crc, uint32_t data) {
    int i;
    crc = crc ^ data;

    for (i = 0; i < 32; i++) {
        if (crc & 0x80000000)
            crc = (crc << 1) ^ 0x04C11DB7;
        else
            crc = (crc << 1);
    }

    return crc;
}

uint32_t stm_crc32(const uint8_t *data, size_t size) {
    uint32_t crc = 0xffffffff;
    const uint32_t *pend = (const uint32_t *) (data + size);
    for (const uint32_t *p = (const uint32_t *) (data); p < pend; p++)
        crc = _crc32(crc, *p);
    return crc;
}

/* { dg-final { scan-tree-dump-times "calculates CRC!" 2 "crc" } } */
