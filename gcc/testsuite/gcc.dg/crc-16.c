/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-crc" } */

#include <stdint.h>

#define POLY (0x1070U << 3)
#define u8 uint8_t
#define u16 uint16_t

//xor in case 0
u8 not_crc(u16 data) {
    int i;
    for (i = 0; i < 8; i++) {
        if (data & 0x0000)
            data = data ^ POLY;
        data = data << 1;
    }
    return (u8)(data >> 8);
}

/* { dg-final { scan-tree-dump-times "Attention! not_crc function calculates CRC." 0 "crc"} } */


