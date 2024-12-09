/* { dg-do run } */
/* { dg-options "-march=armv8-a+crc -O2 -fdump-rtl-dfinish -fdump-tree-crc" } */
/* { dg-skip-if "" { *-*-* } { "-flto"} } */

#include <stdint.h>
#include <stdlib.h>
__attribute__ ((noinline,optimize(0)))
uint32_t _crc32_O0 (uint32_t crc, uint32_t data) {
  int i;
  crc = crc ^ data;

  for (i = 0; i < 32; i++) {
      if (crc & 1)
	crc = (crc >> 1) ^ 0xEDB88320;
      else
	crc = (crc >> 1);
    }

  return crc;
}

uint32_t _crc32 (uint32_t crc, uint32_t data) {
  int i;
  crc = crc ^ data;

  for (i = 0; i < 32; i++) {
      if (crc & 1)
	crc = (crc >> 1) ^ 0xEDB88320;
      else
	crc = (crc >> 1);
    }

  return crc;
}

int main ()
{
  uint32_t crc = 0x0D800D80;
  for (uint8_t i = 0; i < 0xff; i++)
    {
      uint32_t res1 = _crc32_O0 (crc, i);
      uint32_t res2 = _crc32 (crc, i);
      if (res1 != res2)
	 abort ();
      crc = res1;
    }
}

/* { dg-final { scan-tree-dump "calculates CRC!" "crc"} } */
/* { dg-final { scan-tree-dump-times "Couldn't generate faster CRC code." 0 "crc"} } */
/* { dg-final { scan-rtl-dump "UNSPEC_CRC32" "dfinish"} } */
/* { dg-final { scan-rtl-dump-times "pmull" 0 "dfinish"} } */
