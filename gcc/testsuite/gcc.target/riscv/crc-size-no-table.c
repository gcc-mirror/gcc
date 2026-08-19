/* { dg-do compile } */
/* { dg-options "-march=rv32gc -mabi=ilp32 -fdump-tree-crc-details -fdisable-tree-phiopt2 -fdisable-tree-phiopt3" { target { rv32 } } } */
/* { dg-options "-march=rv64gc -mabi=lp64 -fdump-tree-crc-details -fdisable-tree-phiopt2 -fdisable-tree-phiopt3" { target { rv64 } } } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-Os" "-Oz" } } */

typedef unsigned char uint8_t;
typedef unsigned int uint32_t;

uint32_t
crc32r (const uint8_t *data, uint32_t size)
{
  uint32_t crc = 0xffffffff;

  for (uint32_t i = 0; i != size; i++)
    {
      crc ^= data[i];
      for (int j = 0; j < 8; j++)
	if (crc & 1)
	  crc = (crc >> 1) ^ 0xedb88320;
	else
	  crc >>= 1;
    }

  return ~crc;
}

/* The CRC loop must be recognized, but retained because rv32gc/rv64gc has no
   CRC optab that is smaller than the original loop.  */
/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
/* { dg-final { scan-tree-dump "Couldn't generate faster CRC code." "crc" } } */
/* { dg-final { scan-tree-dump-not {\.CRC_REV} "crc" } } */
