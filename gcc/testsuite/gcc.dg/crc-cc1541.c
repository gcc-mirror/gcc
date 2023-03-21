/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-crc-details -march=rv64gc_zbc" } */
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
/* { dg-final { scan-tree-dump "Executing \[a-zA-Z_\]\[a-zA-Z0-9_\]* = \[a-zA-Z_\]\[a-zA-Z0-9_\]* \\\^ \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?;" "crc" } } */
/* { dg-final { scan-tree-dump "Executing \[a-zA-Z_\]\[a-zA-Z0-9_\\\.\]* = \[a-zA-Z_\]\[a-zA-Z0-9_\]* \(<<|>>\) \[0-9]+;" "crc" } } */
/* { dg-final { scan-tree-dump "Return value is \\\{\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[6\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[5\\\], \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[4\\\] \\\^ 1\\\), \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[3\\\] \\\^ 1\\\), \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[2\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[1\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[0\\\], 1\\\}" "crc"} } */
/* { dg-final { scan-tree-dump "Return value is \\\{\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[6\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[5\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[4\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[3\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[2\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[1\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[0\\\], 0\\\}" "crc"} } */
/* { dg-final { scan-tree-dump "Polynomial's value is \\\{0, 0, 1, 1, 0, 0, 0, 1\\\}" "crc"} } */
