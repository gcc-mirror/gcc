/* Test AAPCS64 layout and __builtin_va_arg.

   This test covers most composite types as described in AAPCS64 \S 4.3.
   Homogeneous floating-point aggregate types are covered in other tests.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define AAPCS64_TEST_STDARG
#define TESTFILE "va_arg-3.c"
#include "type-def.h"

struct x0
{
  char ch;
  int i;
} y0 = { 'a', 12345 };

struct x1
{
  int a;
  int b;
  int c;
  int d;
} y1 = { 0xdeadbeef, 0xcafebabe, 0x87654321, 0xabcedf975 };

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

struct x5
{
  union int128_t qword;
} y5;

#define HAS_DATA_INIT_FUNC
void init_data ()
{
  /* Init small union.  */
  y3.ll = 0xfedcba98LL;

  /* Init big union.  */
  y4.y2.a = 0x78;
  y4.y2.b = 0x89;
  y4.y2.ch= 0x9a;

  /* Init signed quad-word integer.  */
  y5.qword.l64 = 0xfdb9753102468aceLL;
  y5.qword.h64 = 0xeca8642013579bdfLL;
}

#include "abitest.h"
#else
  ARG      (_Decimal32 ,1.0df, S0, LAST_NAMED_ARG_ID)
  DOTS
  ANON     (struct x0, y0, X0,        1)
  ANON     (struct x1, y1, X1,        2)
  PTR_ANON (struct x2, y2, X3,        3)
  ANON     (union  x3, y3, X4,        4)
  PTR_ANON (union  x4, y4, X5,        5)
  ANON     (struct x5, y5, X6,        6)
  ANON     (struct x0, y0, STACK,     7)
  ANON     (struct x1, y1, STACK+8,   8)
  PTR_ANON (struct x2, y2, STACK+24,  9)
  ANON     (union  x3, y3, STACK+32, 10)
  PTR_ANON (union  x4, y4, STACK+40, 11)
#ifndef __AAPCS64_BIG_ENDIAN__
  ANON     (int      ,  1, STACK+48, 12)
#else
  ANON     (int      ,  1, STACK+52, 12)
#endif
  ANON     (struct x5, y5, STACK+64, 13)
#ifndef __AAPCS64_BIG_ENDIAN__
  LAST_ANON(int      ,  2, STACK+80, 14)
#else
  LAST_ANON(int      ,  2, STACK+84, 14)
#endif
#endif
