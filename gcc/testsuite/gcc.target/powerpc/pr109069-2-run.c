/* { dg-do run } */
/* { dg-require-effective-target vsx_hw } */
/* { dg-options "-O2 -mvsx" } */

/* Verify it doesn't generate wrong code.  */

#include "pr109069-2.h"

int
main ()
{
  vector unsigned char res1 = test1 ();
  for (int i = 0; i < 16; i++)
    if (res1[i] != 0xd)
      __builtin_abort ();

  vector signed short res2 = test2 ();
  for (int i = 0; i < 8; i++)
    if (res2[i] != 0x7777)
      __builtin_abort ();

  vector signed int res3 = test3 ();
  vector unsigned int res4 = test4 ();
  vector float res6 = test6 ();
  for (int i = 0; i < 4; i++)
    {
      if (res3[i] != 0xbbbbbbbb)
	__builtin_abort ();
      if (res4[i] != 0x7070707)
	__builtin_abort ();
      U32b u;
      u.f = res6[i];
      if (u.i != 0x17171717)
	__builtin_abort ();
    }

  vector unsigned long long res5 = test5 ();
  vector double res7 = test7 ();
  for (int i = 0; i < 2; i++)
    {
      if (res5[i] != 0x4545454545454545ll)
	__builtin_abort ();
      U64b u;
      u.f = res7[i];
      if (u.i != 0x5454545454545454ll)
	__builtin_abort ();
    }
  return 0;
}

