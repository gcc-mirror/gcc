/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-crc-details" } */

#define CRC16_CCITT     0x102
#define POLYNOM         CRC16_CCITT

unsigned int crc16(unsigned int crcValue, unsigned char newByte) {
    unsigned char i;

    for (i = 0; i < 8; i++) {

        if (((crcValue & 0x8000) >> 8) ^ (newByte & 0x80)) {
            crcValue = (crcValue << 1) ^ POLYNOM;
        } else {
            crcValue = (crcValue << 1);
        }

        newByte <<= 1;
    }

    return crcValue;
}

/* { dg-final { scan-tree-dump "crc16 function maybe calculates CRC and returns it." "crc"} } */
/* { dg-final { scan-tree-dump "Return size is 32" "crc"} } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc"} } */
/* { dg-final { scan-tree-dump "Bit forward" "crc"} } */
/* { dg-final { scan-tree-dump "Executing \[a-zA-Z_\]\[a-zA-Z0-9_\]* = \[a-zA-Z_\]\[a-zA-Z0-9_\]* \\\^ \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?;" "crc" } } */
/* { dg-final { scan-tree-dump "Executing \[a-zA-Z_\]\[a-zA-Z0-9_\]* = \[a-zA-Z_\]\[a-zA-Z0-9_\]* \(<<|>>\) \[0-9]+;" "crc" } } */
/* { dg-final { scan-tree-dump "crc16 function calculates CRC!" "crc"} } */
