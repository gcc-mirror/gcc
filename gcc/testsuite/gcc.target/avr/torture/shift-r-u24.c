/* { dg-do run } */
/* { dg-additional-options { -std=gnu99 -fwrapv } } */

#define OP >>
#define BITS 24
typedef __uint24 T;

#include "test-shift.h"

const AS T vals[] =
  {
    1, 0x01a273, 0x7f324b, 0x7e801, 0x765432
  };
  
int main (void)
{
  for (int i = 0; i < ARRAY_SIZE (vals); ++i)
    {
      testx (vals[i]);
      testx (-vals[i]);
    }
  testx (0x800000);

  return 0;
}
