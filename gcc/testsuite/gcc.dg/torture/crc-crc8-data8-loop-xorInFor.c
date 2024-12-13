/* { dg-do run } */
/* { dg-options "-fdump-tree-crc-details" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-g" "-Os" "-funroll-loops" "-fpeel-loops" "-flto" } } */

#include <stddef.h>

typedef unsigned char uint8_t;

uint8_t gencrc (uint8_t *message, size_t len) {
  uint8_t crc = 0;
  size_t i, j;
  for (i = 0; i < len; i++) {
      uint8_t data = message[i];
      for (j = 0; j < 8; j++) {
	  if (((crc & 0x80) ^ (data & 0x80)) != 0)
	    crc = (uint8_t) ((crc << 1) ^ 0x31);
	  else
	    crc <<= 1;
	  data <<=1;
	}
    }
  return crc;
}

int main()
{
  uint8_t message[] = "Hello world!";
  if (gencrc(message, 12) != 0x24)
    __builtin_abort ();
  __builtin_exit (0);
}

/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
/* { dg-final { scan-tree-dump-times "Couldn't generate faster CRC code." 0 "crc" } } */
