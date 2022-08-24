/* Test AAPCS64 layout and __builtin_va_arg.

   This test is focus on certain unnamed homogeneous floating-point aggregate
   types passed in fp/simd registers.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define AAPCS64_TEST_STDARG
#define TESTFILE "va_arg_dfp-6.c"
#include "type-def.h"

struct hfa_dfx1_t hfa_dfx1 = {12.345df};
struct hfa_ddx2_t hfa_ddx2 = {234.567dd, 345.678dd};
struct hfa_dffs_t hfa_dffs;
union hfa_dunion_t hfa_dunion;

#define HAS_DATA_INIT_FUNC
void init_data ()
{
  hfa_dunion.s.a = 37.df;
  hfa_dunion.s.b = 38.df;
  hfa_dunion.c   = 39.df;

  hfa_ffs.a = 42.f;
  hfa_ffs.b = 43.f;
  hfa_ffs.c.a = 44.f;
  hfa_ffs.c.b = 45.f;
}

#include "abitest.h"
#else
  ARG      (int, 1, W0, LAST_NAMED_ARG_ID)
  DOTS
  ANON     (struct hfa_dffs_t , hfa_dffs , S0     , 0)
  ANON     (union  hfa_dunion_t, hfa_dunion, S4   , 1)
  ANON     (struct hfa_ddx2_t , hfa_ddx2 , D6     , 2)
  ANON     (struct hfa_dfx1_t , hfa_dfx1 , STACK  , 3)
  LAST_ANON(_Decimal64        , 1.0dd    , STACK+8, 4)
#endif
