/* Test AAPCS64 layout and __builtin_va_arg.

   This test covers fundamental data types as specified in AAPCS64 \S 4.1.
   It is focus on unnamed parameter passed in registers.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define AAPCS64_TEST_STDARG
#define TESTFILE "va_arg-1.c"
#include "type-def.h"

vf2_t vf2 = (vf2_t){ 17.f, 18.f };
vi4_t vi4 = (vi4_t){ 0xdeadbabe, 0xbabecafe, 0xcafebeef, 0xbeefdead };
union int128_t qword;
signed char sc = 0xed;
signed int sc_promoted = 0xffffffed;
signed short ss = 0xcba9;
signed int ss_promoted = 0xffffcba9;
float fp = 65432.12345f;
double fp_promoted = (double)65432.12345f;
__fp16 fp16 = 2.0f;
__fp16 fp16_promoted = (double)2.0f;

#define HAS_DATA_INIT_FUNC
void init_data ()
{
  /* Init signed quad-word integer.  */
  qword.l64 = 0xfdb9753102468aceLL;
  qword.h64 = 0xeca8642013579bdfLL;
}

#include "abitest.h"
#else
  ARG          (         int      , 0xff  ,                            W0, LAST_NAMED_ARG_ID)
  DOTS
  ANON_PROMOTED(unsigned char     , 0xfe  , unsigned int, 0xfe       , W1,       1)
  ANON_PROMOTED(  signed char     , sc    ,   signed int, sc_promoted, W2,       2)
  ANON_PROMOTED(unsigned short    , 0xdcba, unsigned int, 0xdcba     , W3,       3)
  ANON_PROMOTED(  signed short    , ss    ,   signed int, ss_promoted, W4,       4)
  ANON         (unsigned int      , 0xdeadbeef,                        W5,       5)
  ANON         (  signed int      , 0xcafebabe,                        W6,       6)
  ANON         (unsigned long long, 0xba98765432101234ULL,             X7,       7)
  ANON         (  signed long long, 0xa987654321012345LL ,             STACK,    8)
  ANON         (          __int128, qword.i              ,             STACK+16, 9)
  ANON_PROMOTED(         float    , fp    ,       double, fp_promoted, D0,      10)
  ANON         (         double   , 9876543.212345,                    D1,      11)
  ANON         (    long double   , 98765432123456789.987654321L,      Q2,      12)
  ANON         (             vf2_t, vf2   ,                            D3,      13)
  ANON         (             vi4_t, vi4   ,                            Q4,      14)
  /* 7.2: For unprototyped (i.e. pre- ANSI or K&R C) and variadic functions,
     in addition to the normal conversions and promotions, arguments of
     type __fp16 are converted to type double.  */
  ANON_PROMOTED(            __fp16, fp16  ,     double, fp16_promoted, D5,      15)
#ifndef __AAPCS64_BIG_ENDIAN__
  LAST_ANON    (         int      , 0xeeee,                            STACK+32,16)
#else
  LAST_ANON    (         int      , 0xeeee,                            STACK+36,16)
#endif
#endif
