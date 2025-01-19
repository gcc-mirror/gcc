/* { dg-do compile } */
/* { dg-options "-fdump-tree-crc-details" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-O3" "-flto" } } */
/* { dg-require-effective-target int32plus } */

//We don't verify this case as for the CRC unsigned int (32 bit) is used and
// the polynomial fits into 16 bit. Also, as the loop's iteration number and
// data's size differ.

unsigned short crc16 (char *data_p, unsigned short length) {
    unsigned char i;
    unsigned int data;
    unsigned int crc = 0xffff;

    if (length == 0)
        return (~crc);

    do {
        for (i = 0, data = (unsigned int) 0xff & *data_p++;
             i < 8;
             i++, data >>= 1) {
            if ((crc & 0x0001) ^ (data & 0x0001))
                crc = (crc >> 1) ^ 0x8408;
            else crc >>= 1;
        }
    } while (--length);

    crc = ~crc;
    data = crc;
    crc = (crc << 8) | (data >> 8 & 0xff);

    return (crc);
}

/* { dg-final { scan-tree-dump "function maybe contains CRC calculation." "crc" } } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc" } } */
/* { dg-final { scan-tree-dump "Bit reversed" "crc" } } */
/* { dg-final { scan-tree-dump "Loop iteration number and data's size differ." "crc" } } */
