/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-crc-details -march=rv64gc_zbc" } */

#include <stdint.h>
uint32_t _crc32(uint32_t crc, uint32_t data) {
  int i;
  crc = crc ^ data;

  for (i = 0; i < 32; i++) {
      if (crc & 0x80000000)
	crc = (crc << 1) ^ 0x04C11DB7;
      else
	crc = (crc << 1);
    }

  return crc;
}

/* { dg-final { scan-tree-dump "_crc32 function maybe calculates CRC and returns it." "crc"} } */
/* { dg-final { scan-tree-dump "Return size is 32" "crc"} } */
/* { dg-final { scan-tree-dump "Loop iteration number is 31" "crc"} } */
/* { dg-final { scan-tree-dump "Bit forward" "crc"} } */
/* { dg-final { scan-tree-dump "_crc32 function calculates CRC." "crc"} } */
