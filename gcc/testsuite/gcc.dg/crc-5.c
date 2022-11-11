/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-crc" } */

typedef unsigned short ee_u16;
typedef unsigned char ee_u8;

ee_u16 crcu8(ee_u8 data, ee_u16 crc) {
    ee_u8 i = 0, x16 = 0, carry = 0;
    for (i = 0; i < 8; i++) {
        x16 = (ee_u8) ((data & 1) ^ ((ee_u8) crc & 1));
        data >>= 1;
        if (x16 == 1) {
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
/* { dg-final { scan-tree-dump "Attention! crcu8 function calculates CRC." "crc"} } */
/* { dg-final { scan-tree-dump "Return size is 16" "crc"} } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc"} } */
/* { dg-final { scan-tree-dump "Bit reversed" "crc"} } */

