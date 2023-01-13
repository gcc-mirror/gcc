/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-crc-details" } */

#include <stdio.h>

typedef unsigned char uint8_t;

uint8_t gencrc (uint8_t *data)
{
  uint8_t crc = 0xff;
  size_t j;
  crc ^= *data;
  for (j = 0; j < 8; j++)
    {
      if ((crc & 0x80) != 0)
	crc = (uint8_t) ((crc << 1) ^ 0x31);
      else
	crc <<= 1;
    }
  return crc;
}

/* { dg-final { scan-tree-dump "gencrc function maybe calculates CRC and returns it." "crc"} } */
/* { dg-final { scan-tree-dump "Return size is 8" "crc"} } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc"} } */
/* { dg-final { scan-tree-dump "Bit forward" "crc"} } */
/* { dg-final { scan-tree-dump "Executing \[a-zA-Z_\]\[a-zA-Z0-9_\]* = \[a-zA-Z_\]\[a-zA-Z0-9_\]* \\\^ \[a-zA-Z0-9_\]+\(\\\(\[a-zA-Z\]\\\)\)?;" "crc" } } */
/* { dg-final { scan-tree-dump "Executing \[a-zA-Z_\]\[a-zA-Z0-9_\]* = \[a-zA-Z_\]\[a-zA-Z0-9_\]* \(<<|>>\) \[0-9]+;" "crc" } } */