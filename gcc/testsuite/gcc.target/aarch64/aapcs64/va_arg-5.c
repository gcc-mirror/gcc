/* Test AAPCS64 layout and __builtin_va_arg.

   This test is focus on certain unnamed homogeneous floating-point aggregate
   types passed in fp/simd registers.  */

/* { dg-do run { target aarch64*-*-* } } */

#ifndef IN_FRAMEWORK
#define AAPCS64_TEST_STDARG
#define TESTFILE "va_arg-5.c"
#include "type-def.h"

struct hfa_fx1_t hfa_fx1 = {12.345f};
struct hfa_fx2_t hfa_fx2 = {123.456f, 234.456f};
struct hfa_dx2_t hfa_dx2 = {234.567, 345.678};
struct hfa_dx4_t hfa_dx4 = {1234.123, 2345.234, 3456.345, 4567.456};
struct hfa_ldx3_t hfa_ldx3 = {123456.7890, 234567.8901, 345678.9012};
struct hfa_ffs_t hfa_ffs;
union hfa_union_t hfa_union;

#define HAS_DATA_INIT_FUNC
void init_data ()
{
  hfa_union.s.a = 37.f;
  hfa_union.s.b = 38.f;
  hfa_union.c   = 39.f;

  hfa_ffs.a = 42.f;
  hfa_ffs.b = 43.f;
  hfa_ffs.c.a = 44.f;
  hfa_ffs.c.b = 45.f;
}

#include "abitest.h"
#else
  ARG      (int, 1, X0, LAST_NAMED_ARG_ID)
  DOTS
  /* HFA passed in fp/simd registers or on stack.  */
  ANON     (struct hfa_dx4_t  , hfa_dx4  , D0      , 0)
  ANON     (struct hfa_ldx3_t , hfa_ldx3 , Q4      , 1)
  ANON     (struct hfa_ffs_t  , hfa_ffs  , STACK   , 2)
  ANON     (union  hfa_union_t, hfa_union, STACK+16, 3)
  ANON     (struct hfa_fx1_t  , hfa_fx1  , STACK+24, 4)
  ANON     (struct hfa_fx2_t  , hfa_fx2  , STACK+32, 5)
  ANON     (struct hfa_dx2_t  , hfa_dx2  , STACK+40, 6)
  LAST_ANON(double            , 1.0      , STACK+56, 7)
#endif
