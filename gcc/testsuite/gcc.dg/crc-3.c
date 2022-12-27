/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-crc-details" } */

unsigned short crc16(char *data_p, unsigned short length) {
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

/* { dg-final { scan-tree-dump "crc16 function maybe calculates CRC and returns it." "crc"} } */
/* { dg-final { scan-tree-dump "Return size is 16" "crc"} } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc"} } */
/* { dg-final { scan-tree-dump "Bit reversed" "crc"} } */
/* { dg-final { scan-tree-dump "Executing \[a-zA-Z_\]\[a-zA-Z0-9_\]* = \[a-zA-Z_\]\[a-zA-Z0-9_\]* \\\^ \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?;" "crc" } } */
/* { dg-final { scan-tree-dump "Executing \[a-zA-Z_\]\[a-zA-Z0-9_\]* = \[a-zA-Z_\]\[a-zA-Z0-9_\]* \(<<|>>\) \[0-9]+;" "crc" } } */
