/* { dg-do run } */
/* { dg-additional-options { -std=gnu99 -fwrapv } } */

#define OP >>
#define BITS 32
typedef __INT32_TYPE__ T;

#include "test-shift.h"

const AS T vals[] =
  {
    1, 0x01a273b9, 0x7f324b01, 0x7e80102, 0x76543219
  };
  
int main (void)
{
  for (int i = 0; i < ARRAY_SIZE (vals); ++i)
    {
      testx (vals[i]);
      testx (-vals[i]);
    }
  testx (0x80000000);

  return 0;
}
