/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-flto" } } */

#include <stdio.h>
typedef unsigned short ee_u16;
typedef unsigned char ee_u8;

int a;
ee_u16 crcu8 (ee_u8 data, ee_u16 crc) {
    ee_u16 i = 0, x16 = 0, carry = 0;
    int c;
    for (i = 0; i < 8; i++) {
        c += i*2;
        printf ("%d", c);
        x16 = (ee_u8) ((data & 1) ^ ((ee_u8) crc & 1));
        data >>= 1;
        if (x16 == 1) {
            crc ^= 0x4002;
            carry = 1;
        } else
            carry = 0;
        crc >>= 1;
        a = c;
        if (carry)
        {
            crc |= 0x8000;
        }
        else
            crc &= 0x7fff;
    }
    return crc;
}

/* { dg-final { scan-tree-dump-times "calculates CRC!" 0 "crc" } } */
