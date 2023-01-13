/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-crc-details" } */

#include <stdio.h>

typedef unsigned char uint8_t;

uint8_t gencrc(uint8_t *data, size_t len) {
    uint8_t crc = 0xff;
    size_t i, j;
    for (i = 0; i < len; i++) {
        crc ^= data[i];
        for (j = 0; j < 8; j++) {
            if ((crc & 0x80) != 0)
                crc = (uint8_t) ((crc << 1) ^ 0x31);
            else
                crc <<= 1;
        }
    }
    return crc;
}

/* { dg-final { scan-tree-dump "gencrc function maybe calculates CRC and returns it." "crc"} } */
/* { dg-final { scan-tree-dump "Return size is 8" "crc"} } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc"} } */
/* { dg-final { scan-tree-dump "Bit forward" "crc"} } */
/* { dg-final { scan-tree-dump "Executing \[a-zA-Z_\]\[a-zA-Z0-9_\]* = \[a-zA-Z_\]\[a-zA-Z0-9_\]* \\\^ \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?;" "crc" } } */
/* { dg-final { scan-tree-dump "Executing \[a-zA-Z_\]\[a-zA-Z0-9_\]* = \[a-zA-Z_\]\[a-zA-Z0-9_\]* \(<<|>>\) \[0-9]+;" "crc" } } */
/* { dg-final { scan-tree-dump "Return value is \\\{\\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[6\\\] \\\^ 1\\\), \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[5\\\] \\\^ 1\\\), \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[4\\\] \\\^ 0\\\), \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[3\\\] \\\^ 0\\\), \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[2\\\] \\\^ 1\\\), \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[1\\\] \\\^ 1\\\), \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[0\\\] \\\^ 1\\\), 1\\\}" "crc" } } */
/* { dg-final { scan-tree-dump "Return value is \\\{\\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[6\\\] \\\^ 1\\\), \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[5\\\] \\\^ 1\\\), \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[4\\\] \\\^ 1\\\), \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[3\\\] \\\^ 1\\\), \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[2\\\] \\\^ 1\\\), \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[1\\\] \\\^ 1\\\), \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[0\\\] \\\^ 1\\\), 0\\\}" "crc" } } */
/* { dg-final { scan-tree-dump "Return value is \\\{\(1, \)*1\\\}" "crc" } } */
/* { dg-final { scan-tree-dump "Polynomial's value is \\\{0, 0, 1, 1, 0, 0, 0, 1\\\}" "crc" } } */
