/* { dg-do run } */
/* { dg-additional-options { -std=gnu99 -fwrapv } } */

#define OP >>
#define BITS 8
typedef __UINT8_TYPE__ T;

#include "test-shift.h"

const AS T vals[] =
  {
    1, 0x55, 0x7f, 0x78, 0x54
  };
  
int main (void)
{
  for (int i = 0; i < ARRAY_SIZE (vals); ++i)
    {
      testx (vals[i]);
      testx (-vals[i]);
    }
  testx (0x80);

  return 0;
}
