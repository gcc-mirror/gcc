/* Test AAPCS64 layout and __builtin_va_arg.

   This test is focus on certain unnamed homogeneous floating-point aggregate
   types passed in fp/simd registers.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define AAPCS64_TEST_STDARG
#define TESTFILE "va_arg_dfp-5.c"
#include "type-def.h"

struct hfa_dfx1_t hfa_dfx1 = {12.345df};
struct hfa_dfx2_t hfa_dfx2 = {123.456df, 234.456df};
struct hfa_ddx2_t hfa_ddx2 = {234.567dd, 345.678dd};
struct hfa_ddx4_t hfa_ddx4 = {1234.123dd, 2345.234dd, 3456.345dd, 4567.456dd};
struct hfa_dldx3_t hfa_dldx3 = {123456.7890dl, 234567.8901dl, 345678.9012dl};
struct hfa_dffs_t hfa_dffs;
union hfa_dunion_t hfa_dunion;

#define HAS_DATA_INIT_FUNC
void init_data ()
{
  hfa_dunion.s.a = 37.df;
  hfa_dunion.s.b = 38.df;
  hfa_dunion.c   = 39.df;

  hfa_dffs.a = 42.df;
  hfa_dffs.b = 43.df;
  hfa_dffs.c.a = 44.df;
  hfa_dffs.c.b = 45.df;
}

#include "abitest.h"
#else
  ARG      (int, 1, W0, LAST_NAMED_ARG_ID)
  DOTS
  /* HFA passed in fp/simd registers or on stack.  */
  ANON     (struct hfa_ddx4_t , hfa_ddx4 , D0      , 0)
  ANON     (struct hfa_dldx3_t, hfa_dldx3, Q4      , 1)
  ANON     (struct hfa_dffs_t , hfa_dffs , STACK   , 2)
  ANON     (union  hfa_dunion_t, hfa_dunion, STACK+16, 3)
  ANON     (struct hfa_dfx1_t , hfa_dfx1 , STACK+24, 4)
  ANON     (struct hfa_dfx2_t , hfa_dfx2 , STACK+32, 5)
  ANON     (struct hfa_ddx2_t , hfa_ddx2 , STACK+40, 6)
  LAST_ANON(_Decimal64        , 1.0dd    , STACK+56, 9)
#endif
