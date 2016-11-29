/* Test AAPCS64 layout and __builtin_va_arg.

   This test covers fundamental data types as specified in AAPCS64 \S 4.1.
   It is focus on unnamed parameter passed on stack.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define AAPCS64_TEST_STDARG
#define TESTFILE "va_arg-2.c"
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
  ARG          (         int      , 0xff  ,                            W0,        0)
  ARG          (         float    , 1.0f  ,                            S0,        1)
  ARG          (         float    , 1.0f  ,                            S1,        2)
  ARG          (         float    , 1.0f  ,                            S2,        3)
  ARG          (         float    , 1.0f  ,                            S3,        4)
  ARG          (         float    , 1.0f  ,                            S4,        5)
  ARG          (         float    , 1.0f  ,                            S5,        6)
  ARG          (         float    , 1.0f  ,                            S6,        7)
  ARG          (         float    , 1.0f  ,                            S7, LAST_NAMED_ARG_ID)
  DOTS
  ANON         (          __int128, qword.i              ,             X2,        8)
  ANON         (  signed long long, 0xa987654321012345LL ,             X4,        9)
  ANON         (          __int128, qword.i              ,             X6,       10)
#ifndef __AAPCS64_BIG_ENDIAN__
  ANON_PROMOTED(unsigned char     , 0xfe  , unsigned int, 0xfe       , STACK,    11)
  ANON_PROMOTED(  signed char     , sc    ,   signed int, sc_promoted, STACK+8,  12)
  ANON_PROMOTED(unsigned short    , 0xdcba, unsigned int, 0xdcba     , STACK+16, 13)
  ANON_PROMOTED(  signed short    , ss    ,   signed int, ss_promoted, STACK+24, 14)
  ANON         (unsigned int      , 0xdeadbeef,                        STACK+32, 15)
  ANON         (  signed int      , 0xcafebabe,                        STACK+40, 16)
#else
  ANON_PROMOTED(unsigned char     , 0xfe  , unsigned int, 0xfe       , STACK+4,  11)
  ANON_PROMOTED(  signed char     , sc    ,   signed int, sc_promoted, STACK+12, 12)
  ANON_PROMOTED(unsigned short    , 0xdcba, unsigned int, 0xdcba     , STACK+20, 13)
  ANON_PROMOTED(  signed short    , ss    ,   signed int, ss_promoted, STACK+28, 14)
  ANON         (unsigned int      , 0xdeadbeef,                        STACK+36, 15)
  ANON         (  signed int      , 0xcafebabe,                        STACK+44, 16)
#endif
  ANON         (unsigned long long, 0xba98765432101234ULL,             STACK+48, 17)
  ANON_PROMOTED(         float    , fp    ,       double, fp_promoted, STACK+56, 18)
  ANON         (         double   , 9876543.212345,                    STACK+64, 19)
  ANON         (    long double   , 98765432123456789.987654321L,      STACK+80, 20)
  ANON         (             vf2_t, vf2   ,                            STACK+96, 21)
  ANON         (             vi4_t, vi4   ,                            STACK+112,22)
  ANON_PROMOTED(         __fp16   , fp16  ,     double, fp16_promoted, STACK+128,23)
#ifndef __AAPCS64_BIG_ENDIAN__
  LAST_ANON    (         int      , 0xeeee,                            STACK+136,24)
#else
  LAST_ANON    (         int      , 0xeeee,                            STACK+140,24)
#endif
#endif
