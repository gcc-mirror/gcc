/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc -O2" } */

typedef unsigned short ee_u16;
typedef unsigned char ee_u8;

ee_u16 not_crc(ee_u8 data, ee_u16 crc) {
    ee_u8 i = 0, carry = 0;
    for (i = 0; i < 8; i++) {
        data >>= 1;
        if (data == 1) {
            crc ^= 0x4002;
            carry = 1;
        } else
            carry = 0;
        crc >>= 1;
        if (carry)
            crc |= 0x8000;
        else
            crc &= 0x7fff;
    }
    return crc;
}

/* { dg-final { scan-tree-dump-times "not_crc function maybe contains CRC calculation" 0 "crc" } } */
