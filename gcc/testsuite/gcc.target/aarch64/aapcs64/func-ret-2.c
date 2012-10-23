/* Test AAPCS64 function result return.

   This test covers most composite types as described in AAPCS64 \S 4.3.
   Homogeneous floating-point aggregate types are covered in func-ret-3.c.  */

/* { dg-do run { target aarch64*-*-* } } */
/* { dg-additional-sources "abitest.S" } */

#ifndef IN_FRAMEWORK
#define TESTFILE "func-ret-2.c"

struct x0
{
  char ch;
  int i;
} ys0 = { 'a', 12345 };

struct x1
{
  int a;
  unsigned int b;
  unsigned int c;
  unsigned int d;
} ys1 = { 0xdeadbeef, 0xcafebabe, 0x87654321, 0xbcedf975 };

struct x2
{
  long long a;
  long long b;
  char ch;
} y2 = { 0x12, 0x34, 0x56 };

union x3
{
  char ch;
  int i;
  long long ll;
} y3;

union x4
{
  int i;
  struct x2 y2;
} y4;

#define HAS_DATA_INIT_FUNC
void init_data ()
{
  /* Init small union.  */
  y3.ll = 0xfedcba98LL;

  /* Init big union.  */
  y4.y2.a = 0x78;
  y4.y2.b = 0x89;
  y4.y2.ch= 0x9a;
}


#include "abitest-2.h"
#else
  /* Composite smaller than or equal to 16 bytes returned in X0 and X1.  */
FUNC_VAL_CHECK ( 0, struct x0, ys0, X0, flat)
FUNC_VAL_CHECK ( 1, struct x1, ys1, X0, flat)
FUNC_VAL_CHECK ( 2, union  x3, y3, X0, flat)

  /* Composite larger than 16 bytes returned in the caller-reserved memory
     block of which the address is passed as an additional argument to the
     function in X8.  */
FUNC_VAL_CHECK (10, struct x2, y2, X8, flat)
FUNC_VAL_CHECK (11, union  x4, y4, X8, flat)
#endif
