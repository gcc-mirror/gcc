/* { dg-do run } */
/* { dg-additional-options -std=gnu99 } */

#define USE_VALUE 0

#include "test-gprs.h"

void test12 (void)
{
#ifndef __AVR_TINY__
  rtest (32,  2, 0x02020103);
  rtest (32,  2, 0xff00ff00);
  rtest (32,  2, 0xfd02fb08);
  rtest (32,  2, 0xfefbfdf7);
  rtest (32, 16, 0xb1b2b3b4);
  rtest (32,  2, 0xc1c2c3c4);
  rtest (32,  2, 0x1c2c3c4c);
  rtest (32,  2, 0x1ff);
  rtest (32,  2, 0);
  rtest (32,  2, 0x01020408);
  rtest (32,  2, 0xffeeddbb);
  rtest (32,  6, 0x11223344);
  rtest (32,  6, 0x22334411);
  rtest (32, 10, 0x11122233);
  rtest (32, 14, 0x0a0b0c0d);
  rtest (32,  2, 0xa0b0c0d0);
  rtest (32,  2, 0xffffffff);
  rtest (32,  2, 0xfdffffbf);
  rtest (32,  2, 0x12345678);
#endif
}

int main (void)
{
  test12 ();

  return 0;
}
