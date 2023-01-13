/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-crc-details" } */

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
/* { dg-final { scan-tree-dump "crcu8 function maybe calculates CRC and returns it." "crc"} } */
/* { dg-final { scan-tree-dump "Return size is 16" "crc"} } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc"} } */
/* { dg-final { scan-tree-dump "Bit reversed" "crc"} } */
/* { dg-final { scan-tree-dump "Executing \[a-zA-Z_\]\[a-zA-Z0-9_\]* = \[a-zA-Z_\]\[a-zA-Z0-9_\]* \\\^ \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?;" "crc" } } */
/* { dg-final { scan-tree-dump "Executing \[a-zA-Z_\]\[a-zA-Z0-9_\]* = \[a-zA-Z_\]\[a-zA-Z0-9_\]* \(<<|>>\) \[0-9]+;" "crc" } } */
/* { dg-final { scan-tree-dump "crcu8 function calculates CRC!" "crc"} } */
/* { dg-final { scan-tree-dump "Return value is \\\{1, \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[15\\\], \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[14\\\] \\\^ 1\\\), \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[13\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[12\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[11\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[10\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[9\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[8\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[7\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[6\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[5\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[4\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[3\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[2\\\], \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[1\\\] \\\^ 1\\\)\\\}" "crc"} } */
/* { dg-final { scan-tree-dump "Return value is \\\{0, \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[15\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[14\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[13\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[12\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[11\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[10\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[9\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[8\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[7\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[6\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[5\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[4\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[3\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[2\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[1\\\]\\\}" "crc"} } */
/* { dg-final { scan-tree-dump "Polynomial's value is \\\{1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1\\\}" "crc" } } */

