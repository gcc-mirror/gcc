/* { dg-do run } */
/* { dg-options "-fdump-tree-crc-details" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-O3" "-flto"} } */

#include <stdint.h>
#include <stdlib.h>

typedef uint8_t crc;
#define WIDTH (8 * sizeof(crc))
#define TOPBIT (1 << (WIDTH - 1))

__attribute__ ((noinline,optimize(0)))
crc crcSlow_O0 (uint8_t const message[], int nBytes) {
    crc remainder = 0;
/*
* Perform modulo-2 division, a byte at a time.
*/
    for (int byte = 0; byte < nBytes; ++byte) {
        remainder ^= (message[byte] << (WIDTH - 8));
        for (uint8_t bit = 8; bit > 0; --bit) {
            if (remainder & TOPBIT) {
                remainder = (remainder << 1) ^ 1234;
            } else {
                remainder = (remainder << 1);
            }
        }
    }
    return (remainder);
}

crc crcSlow (uint8_t const message[], int nBytes) {
  crc remainder = 0;
/*
* Perform modulo-2 division, a byte at a time.
*/
  for (int byte = 0; byte < nBytes; ++byte) {
      remainder ^= (message[byte] << (WIDTH - 8));
      for (uint8_t bit = 8; bit > 0; --bit) {
	  if (remainder & TOPBIT) {
	      remainder = (remainder << 1) ^ 1234;
	    } else {
	      remainder = (remainder << 1);
	    }
	}
    }
  return (remainder);
}

int main ()
{
  for (crc i = 0; i < 255; i++)
    {
      crc res1 = crcSlow_O0 (&i, 1);
      crc res2 = crcSlow (&i, 1);
      if (res1 != res2)
	__builtin_abort ();
    }
  __builtin_exit (0);
}
/* { dg-final { scan-tree-dump "crcSlow function maybe contains CRC calculation." "crc"} } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc"} } */
/* { dg-final { scan-tree-dump "Bit forward" "crc"} } */
/* { dg-final { scan-tree-dump "Polynomial's value is \\\{1, 1, 0, 1, 0, 0, 1, 0\\\}" "crc" } } */
/* { dg-final { scan-tree-dump "calculates CRC!" "crc"} } */
/* { dg-final { scan-tree-dump-times "Couldn't generate faster CRC code." 0 "crc"} } */
