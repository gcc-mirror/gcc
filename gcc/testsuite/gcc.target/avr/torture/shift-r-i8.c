/* { dg-do run } */
/* { dg-additional-options { -std=gnu99 -fwrapv } } */

#define OP >>
#define BITS 8
typedef __INT8_TYPE__ T;

#include "test-shift.h"

const AS T vals[] =
  {
    1, 0x72, 0x32, 0x6f, 0x76
  };
  
int main (void)
{
  for (int i = 0; i < ARRAY_SIZE (vals); ++i)
    {
      testx (vals[i]);
      testx (-vals[i]);
    }
  testx ((T) 0x80);

  return 0;
}
