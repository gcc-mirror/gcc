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
