/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-crc-details" } */

#include <stdint.h>

#define POLY (0x1070U << 3)
#define u8 uint8_t
#define u16 uint16_t

u8 crc8(u16 data) {
    int i;
    for (i = 0; i < 8; i++) {
        if (data & 0x8000)
            data = data ^ POLY;
        data = data << 1;
    }
    return (u8)(data >> 8);
}

/* { dg-final { scan-tree-dump "crc8 function maybe calculates CRC and returns it." "crc"} } */
/* { dg-final { scan-tree-dump "Return size is 8" "crc"} } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc"} } */
/* { dg-final { scan-tree-dump "Bit forward" "crc"} } */
/* { dg-final { scan-tree-dump "Executing \[a-zA-Z_\]\[a-zA-Z0-9_\]* = \[a-zA-Z_\]\[a-zA-Z0-9_\]* \\\^ \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?;" "crc" } } */
/* { dg-final { scan-tree-dump "Executing \[a-zA-Z_\]\[a-zA-Z0-9_\]* = \[a-zA-Z_\]\[a-zA-Z0-9_\]* \(<<|>>\) \[0-9]+;" "crc" } } */
/* { dg-final { scan-tree-dump "Return value is \\\{\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[14\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[13\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[12\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[11\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[10\\\], \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[9\\\] \\\^ 1\\\), \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[8\\\] \\\^ 1\\\), \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[7\\\] \\\^ 1\\\)\\\}" "crc" } } */
/* { dg-final { scan-tree-dump "Return value is \\\{\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[14\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[13\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[12\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[11\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[10\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[9\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[8\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[7\\\]\\\}" "crc" } } */
/* { dg-final { scan-tree-dump "Polynomial's value is \\\{0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0\\\}" "crc" } } */
