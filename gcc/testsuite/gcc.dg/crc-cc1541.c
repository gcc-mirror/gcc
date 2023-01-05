/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-crc-details" } */
unsigned char
crc8(unsigned char value)
{
    for (int i = 0; i < 8; ++i) {
        value = (value & 0x80) ? ((value << 1) ^ 0x31) : (value << 1);
    }

    return value;
}
/* { dg-final { scan-tree-dump "crc8 function maybe calculates CRC and returns it." "crc"} } */
/* { dg-final { scan-tree-dump "Return size is 8" "crc"} } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc"} } */
/* { dg-final { scan-tree-dump "Bit forward" "crc"} } */
/* { dg-final { scan-tree-dump "crc8 function calculates CRC." "crc"} } */