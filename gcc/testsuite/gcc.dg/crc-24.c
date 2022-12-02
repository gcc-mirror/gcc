/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-crc" } */

#include <stdio.h>
#include <stdint.h>

#define CRC16 0x8005

uint16_t gen_crc16(const uint8_t *data, uint16_t size) {
    uint16_t out = 0;
    int bits_read = 0, bit_flag;

    printf("buffer in function %s\n", data);

    if (data == NULL)
        return 0;

    while (size > 0) {
        bit_flag = out >> 15;

        out <<= 1;
        out |= (*data >> bits_read) & 1;

        bits_read++;
        if (bits_read > 7) {
            bits_read = 0;
            data++;
            size--;
        }

        if (bit_flag)
            out ^= CRC16;

    }

    int i;
    for (i = 0; i < 16; ++i) {
        bit_flag = out >> 15;
        out <<= 1;
        if (bit_flag)
            out ^= CRC16;
    }

    uint16_t crc = 0;
    i = 0x8000;
    int j = 0x0001;
    for (; i != 0; i >>= 1, j <<= 1) {
        if (i & out) crc |= j;
    }

    return crc;
}

/* { dg-final { scan-tree-dump "Found naive crc implementation in gen_crc16." "crc"} } */
/* { dg-final { scan-tree-dump "Return size is 16" "crc"} } */
/* { dg-final { scan-tree-dump "Loop iteration number is 15" "crc"} } */
/* { dg-final { scan-tree-dump "Bit forward" "crc"} } */


