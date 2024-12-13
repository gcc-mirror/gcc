/* { dg-do run } */
/* { dg-options "-fdump-tree-crc-details" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-g" "-Os" "-funroll-loops" "-fpeel-loops" "-flto" } } */
/* { dg-require-effective-target int32plus } */

// Test from roms/u-boot/drivers/ram/octeon/dimm_spd_eeprom.c

#include <stdint.h>
#include <stdlib.h>

typedef uint8_t u8;
typedef uint16_t u16;

// We don't verify as crc is of type int (32 bit),
// but the polynomial fits into 16 bit.
u16 ddr3_crc16_orig (u8 *ptr, int count)
{
  /* From DDR3 SPD specification */
  int crc, i;

  crc = 0;
  while (--count >= 0) {
      crc = crc ^ (int)*ptr++ << 8;
      for (i = 0; i < 8; ++i) {
	  if (crc & 0x8000)
	    crc = crc << 1 ^ 0x1021;
	  else
	    crc = crc << 1;
	}
    }

  return (crc & 0xFFFF);
}

__attribute__ ((noinline,optimize(0)))
u16 ddr3_crc16_modified_O0  (u8 *ptr, int count)
{
  /* From DDR3 SPD specification */
  u16 crc, i;

  crc = 0;
  while (--count >= 0) {
      crc = crc ^ (int)*ptr++ << 8;
      for (i = 0; i < 8; ++i) {
	  if (crc & 0x8000)
	    crc = crc << 1 ^ 0x1021;
	  else
	    crc = crc << 1;
	}
    }

  return (crc & 0xFFFF);
}

u16 ddr3_crc16_modified  (u8 *ptr, int count)
{
  /* From DDR3 SPD specification */
  u16 crc, i;

  crc = 0;
  while (--count >= 0) {
      crc = crc ^ (int)*ptr++ << 8;
      for (i = 0; i < 8; ++i) {
	  if (crc & 0x8000)
	    crc = crc << 1 ^ 0x1021;
	  else
	    crc = crc << 1;
	}
    }

  return (crc & 0xFFFF);
}

int main ()
{
  u8 st[2] = {'H', 'i'};
  for (u8 i = 0; i < 255; i++)
    {
      st[0] = i;
      u16 res1 = ddr3_crc16_modified_O0 (st, 2);
      u16 res2 = ddr3_crc16_modified (st, 2);
      if (res1 != res2)
	__builtin_abort ();
    }
  __builtin_exit (0);
}

/* { dg-final { scan-tree-dump "function maybe contains CRC calculation." "crc"} } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc"} } */
/* { dg-final { scan-tree-dump "Bit forward" "crc"} } */
/* { dg-final { scan-tree-dump "calculates CRC!" "crc"} } */
/* { dg-final { scan-tree-dump-times "Couldn't generate faster CRC code." 0 "crc"} } */
