/* Test AAPCS64 function result return.

   This test covers most fundamental data types as specified in
   AAPCS64 \S 4.1.  */

/* { dg-do run { target aarch64*-*-* } } */
/* { dg-additional-sources "abitest.S" } */

#ifndef IN_FRAMEWORK
#define TESTFILE "func-ret-1.c"
#include "type-def.h"

vf2_t vf2 = (vf2_t){ 17.f, 18.f };
vi4_t vi4 = (vi4_t){ 0xdeadbabe, 0xbabecafe, 0xcafebeef, 0xbeefdead };
vlf1_t vlf1 = (vlf1_t) { 17.0 };

union int128_t qword;

int *int_ptr = (int *)0xabcdef0123456789ULL;

#define HAS_DATA_INIT_FUNC
void init_data ()
{
  /* Init signed quad-word integer.  */
  qword.l64 = 0xfdb9753102468aceLL;
  qword.h64 = 0xeca8642013579bdfLL;
}

#include "abitest-2.h"
#else
FUNC_VAL_CHECK (0, unsigned char , 0xfe      , X0, i8in64)
FUNC_VAL_CHECK (1,   signed char , 0xed      , X0, i8in64)
FUNC_VAL_CHECK (2, unsigned short, 0xdcba    , X0, i16in64)
FUNC_VAL_CHECK (3,   signed short, 0xcba9    , X0, i16in64)
FUNC_VAL_CHECK (4, unsigned int  , 0xdeadbeef, X0, i32in64)
FUNC_VAL_CHECK (5,   signed int  , 0xcafebabe, X0, i32in64)
FUNC_VAL_CHECK (6, unsigned long long, 0xba98765432101234ULL, X0, flat)
FUNC_VAL_CHECK (7,   signed long long, 0xa987654321012345LL, X0, flat)
FUNC_VAL_CHECK (8,       __int128, qword.i, X0, flat)
FUNC_VAL_CHECK (9,          float, 65432.12345f, S0, flat)
FUNC_VAL_CHECK (10,        double, 9876543.212345, D0, flat)
FUNC_VAL_CHECK (11,   long double, 98765432123456789.987654321L, Q0, flat)
FUNC_VAL_CHECK (12,         vf2_t,        vf2, D0, f32in64)
FUNC_VAL_CHECK (13,         vi4_t,        vi4, Q0, i32in128)
FUNC_VAL_CHECK (14,         int *,    int_ptr, X0, flat)
FUNC_VAL_CHECK (15,         vlf1_t,    vlf1, Q0, flat)
FUNC_VAL_CHECK (16,         __fp16,    0xabcd, H0, flat)
#endif
