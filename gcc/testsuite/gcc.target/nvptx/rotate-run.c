/* { dg-do run } */
/* { dg-options "-O2" } */

#include "rotate.c"

#define ASSERT(EXPR)				\
  do						\
    {						\
      if (!(EXPR))				\
	__builtin_abort ();			\
    } while (0)

int
main (void)
{
  ASSERT (rotl (0x12345678, 8) == 0x34567812);
  ASSERT (rotl (0x12345678, 8 + 32) == 0x34567812);

  ASSERT (rotr (0x12345678, 8) == 0x78123456);
  ASSERT (rotr (0x12345678, 8 + 32) == 0x78123456);

  return 0;
}
