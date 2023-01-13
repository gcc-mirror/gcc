/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-crc-details" } */

#include <stdint.h>

uint8_t _crc_ibutton_update(uint8_t crc, uint8_t data) {
    uint8_t i;
    crc = crc ^ data;
    for (i = 0; i < 8; i++) {
        if (crc & 0x01)
            crc = (crc >> 1) ^ 0x8C;
        else
            crc >>= 1;
    }
    return crc;
}

/* { dg-final { scan-tree-dump "_crc_ibutton_update function maybe calculates CRC and returns it." "crc"} } */
/* { dg-final { scan-tree-dump "Return size is 8" "crc"} } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc"} } */
/* { dg-final { scan-tree-dump "Bit reversed" "crc"} } */
/* { dg-final { scan-tree-dump "Executing \[a-zA-Z_\]\[a-zA-Z0-9_\]* = \[a-zA-Z_\]\[a-zA-Z0-9_\]* \\\^ \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?;" "crc" } } */
/* { dg-final { scan-tree-dump "Executing \[a-zA-Z_\]\[a-zA-Z0-9_\]* = \[a-zA-Z_\]\[a-zA-Z0-9_\]* \(<<|>>\) \[0-9]+;" "crc" } } */
/* { dg-final { scan-tree-dump "_crc_ibutton_update function calculates CRC!" "crc"} } */
/* { dg-final { scan-tree-dump "Return value is \\\{1, \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[7\\\] \\\^ \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[7\\\]\\\), \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[6\\\] \\\^ \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[6\\\]\\\), \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[5\\\] \\\^ \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[5\\\]\\\), \\\(\\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[4\\\] \\\^ \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[4\\\]\\\) \\\^ 1\\\), \\\(\\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[3\\\] \\\^ \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[3\\\]\\\) \\\^ 1\\\), \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[2\\\] \\\^ \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[2\\\]\\\), \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[1\\\] \\\^ \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[1\\\]\\\)\\\}" "crc" } } */
/* { dg-final { scan-tree-dump "Return value is \\\{0, \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[7\\\] \\\^ \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[7\\\]\\\), \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[6\\\] \\\^ \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[6\\\]\\\), \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[5\\\] \\\^ \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[5\\\]\\\), \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[4\\\] \\\^ \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[4\\\]\\\), \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[3\\\] \\\^ \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[3\\\]\\\), \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[2\\\] \\\^ \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[2\\\]\\\), \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[1\\\] \\\^ \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[1\\\]\\\)\\\}" "crc" } } */
/* { dg-final { scan-tree-dump "Polynomial's value is \\\{1, 0, 0, 0, 1, 1, 0, 0\\\}" "crc" } } */
