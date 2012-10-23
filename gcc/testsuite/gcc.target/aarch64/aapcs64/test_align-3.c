/* Test AAPCS64 layout.

   C.8  If the argument has an alignment of 16 then the NGRN is rounded up
	the next even number.
   C.9  If the argument is an Integral Type, the size of the argument is
	equal to 16 and the NGRN is less than 7, the argument is copied
	to x[NGRN] and x[NGRN+1]. x[NGRN] shall contain the lower addressed
	double-word of the memory representation of the argument.  The
	NGRN is incremented by two.  The argument has now been allocated.

   The case of passing a 128-bit integer in two general registers is covered
   in this test.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_align-3.c"
#include "type-def.h"

union int128_t qword;

int gInt[4];

#define HAS_DATA_INIT_FUNC
void init_data ()
{
  /* Initialize the quadword integer via the union.  */
  qword.l64 = 0xDEADBEEFCAFEBABELL;
  qword.h64 = 0x123456789ABCDEF0LL;

  gInt[0] = 12345;
  gInt[1] = 23456;
  gInt[2] = 34567;
  gInt[3] = 45678;
}


#include "abitest.h"
#else
  ARG(int, gInt[0], W0)
  ARG(int, gInt[1], W1)
  ARG(int, gInt[2], W2)
  ARG(__int128, qword.i, X4)
  LAST_ARG(int, gInt[3], W6)

#endif
