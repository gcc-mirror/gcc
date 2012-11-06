/* Test AAPCS64 layout.

   C.8  If the argument has an alignment of 16 then the NGRN is rounded up
	the next even number.

   The case of a small struture containing only one 16-byte aligned
   quad-word integer is covered in this test.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define TESTFILE "test_align-2.c"
#include "type-def.h"

struct y
{
  union int128_t v;
} w;

struct x
{
  long long p;
  int q;
} s = {0xDEADBEEFCAFEBABELL, 0xFEEBDAED};

#define HAS_DATA_INIT_FUNC
void init_data ()
{
  /* Init signed quad-word integer.  */
  w.v.l64 = 0xfdb9753102468aceLL;
  w.v.h64 = 0xeca8642013579bdfLL;
}

#include "abitest.h"
#else
  ARG(int, 0xAB, W0)
  ARG(struct y, w, X2)
  ARG(int, 0xCD, W4)
  ARG(struct x, s, X5)
  LAST_ARG(int, 0xFF00FF00, W7)

#endif
