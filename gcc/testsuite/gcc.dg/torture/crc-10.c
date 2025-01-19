/* { dg-do run } */
/* { dg-options "-fdump-tree-crc-details" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-flto" } } */
/* { dg-require-effective-target int32plus } */

#include <stdint.h>
#include <stdlib.h>

#define POLY (0x1070U << 3)
#define u8 uint8_t
#define u16 uint16_t

 __attribute__ ((noinline,optimize(0)))
u8 crc8_O0 (u16 data) {
    int i;
    for (i = 0; i < 8; i++) {
        if (data & 0x8000)
            data = data ^ POLY;
        data = data << 1;
    }
    return (u8)(data >> 8);
}

u8 crc8 (u16 data) {
    int i;
    for (i = 0; i < 8; i++) {
        if (data & 0x8000)
            data = data ^ POLY;
        data = data << 1;
    }
    return (u8)(data >> 8);
}

int main ()
{
  for (u8 i = 0; i < 255; i++)
    {
      u8 res1 = crc8_O0 (i);
      u8 res2 = crc8 (i);
      if (res1 != res2)
	__builtin_abort ();
    }
  __builtin_exit (0);
}
/* { dg-final { scan-tree-dump "function maybe contains CRC calculation." "crc" } } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc" } } */
/* { dg-final { scan-tree-dump "Bit forward" "crc" } } */
/* { dg-final { scan-tree-dump "Polynomial's value is \\\{0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0\\\}" "crc" } } */
/* { dg-final { scan-tree-dump "calculates CRC!" "crc" } } */
/* { dg-final { scan-tree-dump-times "Couldn't generate faster CRC code." 0 "crc" } } */
