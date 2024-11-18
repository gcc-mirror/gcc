/* { dg-do run } */
/* { dg-additional-options { -std=gnu99 -fwrapv } } */

#define OP >>
#define BITS 16
typedef __INT16_TYPE__ T;

#include "test-shift.h"

const AS T vals[] =
  {
    1, 0x01a2, 0x7f32, 0x7e81, 0x7654
  };
  
int main (void)
{
  for (int i = 0; i < ARRAY_SIZE (vals); ++i)
    {
      testx (vals[i]);
      testx (-vals[i]);
    }
  testx ((T) 0x8000);

  return 0;
}
