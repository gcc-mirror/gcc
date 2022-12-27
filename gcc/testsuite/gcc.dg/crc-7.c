/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-crc-details" } */

#include <stdint.h>

uint16_t crc_xmodem_update(uint16_t crc, uint8_t data) {
    int i;
    crc = crc ^ ((uint16_t) data << 8);
    for (i = 0; i < 8; i++) {
        if (crc & 0x8000)
            crc = (crc << 1) ^ 0x1021;
        else
            crc <<= 1;
    }
    return crc;
}

/* { dg-final { scan-tree-dump "crc_xmodem_update function maybe calculates CRC and returns it." "crc"} } */
/* { dg-final { scan-tree-dump "Return size is 16" "crc"} } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc"} } */
/* { dg-final { scan-tree-dump "Bit forward" "crc"} } */
/* { dg-final { scan-tree-dump "Executing \[a-zA-Z_\]\[a-zA-Z0-9_\]* = \[a-zA-Z_\]\[a-zA-Z0-9_\]* \\\^ \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?;" "crc" } } */
/* { dg-final { scan-tree-dump "Executing \[a-zA-Z_\]\[a-zA-Z0-9_\]* = \[a-zA-Z_\]\[a-zA-Z0-9_\]* \(<<|>>\) \[0-9]+;" "crc" } } */
/* { dg-final { scan-tree-dump "crc_xmodem_update function calculates CRC!" "crc"} } */