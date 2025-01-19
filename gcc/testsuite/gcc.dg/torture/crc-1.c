/* { dg-do run } */
/* { dg-options "-fdump-tree-crc-details -fdisable-tree-phiopt2 -fdisable-tree-phiopt3" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-flto" } } */
/* { dg-require-effective-target int32plus } */

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>

#define CRC16 0x8005

__attribute__ ((noinline,optimize(0)))
uint16_t gen_crc16_O0 (const uint8_t *data, uint16_t size) {
    uint16_t out = 0;
    int bits_read = 0, bit_flag;

    if (data == NULL)
        return 0;

    while (size > 0) {
        bit_flag = out >> 15;

        out <<= 1;
        out |= (*data >> bits_read) & 1;

        bits_read++;
        if (bits_read > 7) {
            bits_read = 0;
            data++;
            size--;
        }

        if (bit_flag)
            out ^= CRC16;
    }

    int i;
    for (i = 0; i < 16; ++i) {
        bit_flag = out >> 15;
        out <<= 1;
        if (bit_flag)
            out ^= CRC16;
    }

    uint16_t crc = 0;
    i = 0x8000;
    int j = 0x0001;
    for (; i != 0; i >>= 1, j <<= 1) {
        if (i & out) crc |= j;
    }

    return crc;
}

uint16_t gen_crc16 (const uint8_t *data, uint16_t size) {
  uint16_t out = 0;
  int bits_read = 0, bit_flag;

  if (data == NULL)
    return 0;

  while (size > 0) {
      bit_flag = out >> 15;

      out <<= 1;
      out |= (*data >> bits_read) & 1;

      bits_read++;
      if (bits_read > 7) {
	  bits_read = 0;
	  data++;
	  size--;
	}

      if (bit_flag)
	out ^= CRC16;
    }

  int i;
  for (i = 0; i < 16; ++i) {
      bit_flag = out >> 15;
      out <<= 1;
      if (bit_flag)
	out ^= CRC16;
    }

  uint16_t crc = 0;
  i = 0x8000;
  int j = 0x0001;
  for (; i != 0; i >>= 1, j <<= 1) {
      if (i & out) crc |= j;
    }

  return crc;
}

int main ()
{
  if (gen_crc16 ("hello", 5) != 13522)
    __builtin_abort ();

  for (uint8_t i = 0; i < 255; i++)
    {
      uint16_t res1 = gen_crc16_O0 (&i, 1);
      uint16_t res2 = gen_crc16 (&i, 1);
      if (res1 != res2)
	__builtin_abort ();
    }
  __builtin_exit (0);
}
/* { dg-final { scan-tree-dump "function maybe contains CRC calculation." "crc" } } */
/* { dg-final { scan-tree-dump "Loop iteration number is 15" "crc" } } */
/* { dg-final { scan-tree-dump "Bit forward" "crc" } } */
/* { dg-final { scan-tree-dump "Polynomial's value is \\\{\[0, \]*1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1\\\}" "crc" } } */
/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
/* { dg-final { scan-tree-dump-times "Couldn't generate faster CRC code." 0 "crc" } } */
