/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-crc-details -march=rv64gc_zbc" } */

#include <stdint.h>

typedef uint8_t crc;
#define WIDTH (8 * sizeof(crc))
#define TOPBIT (1 << (WIDTH - 1))

crc
crcSlow(uint8_t message) {
  crc remainder = 0;
      remainder ^= (message << (WIDTH - 8));
      for (uint8_t bit = 8; bit > 0; --bit) {
	  if (remainder & TOPBIT) {
	      remainder = (remainder << 1) ^ 1234;
	    } else {
	      remainder = (remainder << 1);
	    }
	}
  return (remainder);
}

/* { dg-final { scan-tree-dump "crcSlow function maybe calculates CRC and returns it." "crc"} } */
/* { dg-final { scan-tree-dump "Return size is 8" "crc"} } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc"} } */
/* { dg-final { scan-tree-dump "Bit forward" "crc"} } */
/* { dg-final { scan-tree-dump "Executing \[a-zA-Z_\]\[a-zA-Z0-9_\]* = \[a-zA-Z_\]\[a-zA-Z0-9_\]* \(<<|>>\) \[0-9]+;" "crc" } } */
/* { dg-final { scan-tree-dump "Executing \[a-zA-Z_\]\[a-zA-Z0-9_\]* = \[a-zA-Z_\]\[a-zA-Z0-9_\]* \\\^ \(-\)?\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?;" "crc" } } */
/* { dg-final { scan-tree-dump "crcSlow function calculates CRC." "crc"} } */
/* { dg-final { scan-tree-dump "Return value is \\\{\\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[6\\\] \\\^ 1\\\), \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[5\\\] \\\^ 1\\\), \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[4\\\], \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[3\\\] \\\^ 1\\\), \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[2\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[1\\\], \\\(\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[0\\\] \\\^ 1\\\), 0\\\}" "crc"} } */
/* { dg-final { scan-tree-dump "Return value is \\\{\[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[6\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[5\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[4\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[3\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[2\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[1\\\], \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?\\\[0\\\], 0\\\}" "crc"} } */
/* { dg-final { scan-tree-dump "Polynomial's value is \\\{1, 1, 0, 1, 0, 0, 1, 0\\\}" "crc"} } */
