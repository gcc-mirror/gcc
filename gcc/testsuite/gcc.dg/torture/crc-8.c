/* { dg-do run } */
/* { dg-options "-fdump-tree-crc-details" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-flto"} } */

#include <stdint.h>
#include <stdlib.h>

__attribute__ ((noinline,optimize(0)))
uint8_t _crc_ibutton_update_O0 (uint8_t crc, uint8_t data) {
    uint8_t i;
    crc = crc ^ data;
    for (i = 0; i < 8; i++) {
        if (crc & 0x01)
            crc = (crc >> 1) ^ 0x8C;
        else
            crc >>= 1;
    }
    return crc;
}

uint8_t _crc_ibutton_update (uint8_t crc, uint8_t data) {
    uint8_t i;
    crc = crc ^ data;
    for (i = 0; i < 8; i++) {
        if (crc & 0x01)
            crc = (crc >> 1) ^ 0x8C;
        else
            crc >>= 1;
    }
    return crc;
}

int main ()
{
  uint8_t crc = 0x0D;
  for (uint8_t i = 0; i < 255; i++)
    {
      uint8_t res1 = _crc_ibutton_update_O0 (i, crc);
      uint8_t res2 = _crc_ibutton_update (i, crc);
      if (res1 != res2)
	__builtin_abort ();
      crc = res1;
    }
  __builtin_exit (0);
}
/* { dg-final { scan-tree-dump "_crc_ibutton_update function maybe contains CRC calculation." "crc"} } */
/* { dg-final { scan-tree-dump "Loop iteration number is 7" "crc"} } */
/* { dg-final { scan-tree-dump "Bit reversed" "crc"} } */
/* { dg-final { scan-tree-dump "calculates CRC!" "crc"} } */
/* { dg-final { scan-tree-dump "Polynomial's value is \\\{\[0, \]*1, 0, 0, 0, 1, 1, 0, 0\\\}" "crc" } } */
/* { dg-final { scan-tree-dump-times "Couldn't generate faster CRC code." 0 "crc"} } */
