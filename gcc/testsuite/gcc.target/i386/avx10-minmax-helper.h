#ifndef AVX10MINMAX_HELPERFUNC_INCLUDED
#define AVX10MINMAX_HELPERFUNC_INCLUDED

#include <math.h>
#include <limits.h>
#include <stdbool.h>
#include <float.h>
#include "avx512f-helper.h"
#define SNAN_float __builtin_nansf ("")
#define SNAN_flag_float 0x7fa00000
#define QNAN_float __builtin_nanf ("")
#define QNAN_flag_float 0x7fc00000
#define SNAN_double ((double)__builtin_nans (""))
#define SNAN_flag_double 0x7ff4000000000000
#define QNAN_double ((double)__builtin_nan (""))
#define QNAN_flag_double 0x7ff8000000000000
#define SNAN__Float16 ((_Float16)__builtin_nansf16 (""))
#define SNAN_flag__Float16 0x7d00
#define QNAN__Float16 ((_Float16)__builtin_nanf16 (""))
#define QNAN_flag__Float16 0x7e00
#define SNAN___bf16 ((__bf16)__builtin_nansf16b (""))
#define SNAN_flag___bf16 0x7fa0
#define QNAN___bf16 ((__bf16)__builtin_nanf (""))
#define QNAN_flag___bf16 0x7fc0
#define ISNAN(x) (x != x)
#define ABS_float(x) fabsf (x)
#define ABS_double(x) fabs (x)
#define ABS__Float16(x) __builtin_fabsf16 (x)
#define ABS___bf16(x) __builtin_fabsf (x)

#define Union_Data(typef, typei) \
typedef union \
{ \
  typef f; \
  typei i; \
} union_##typef;

Union_Data(float, int)
Union_Data(double, long long)
Union_Data(__bf16, short)
Union_Data(_Float16, short)

#define IS_SNAN(union_x, type) ((union_x.i & SNAN_flag_##type) == union_snan.i)

#define IS_QNAN(union_x, type) ((union_x.i & QNAN_flag_##type) == union_qnan.i)

#define CHECK_EXP_MINMAX(UNION_TYPE, VALUE_TYPE, INT_TYPE) \
static int \
__attribute__((noinline, unused)) \
check_minmax_##UNION_TYPE (UNION_TYPE u, const VALUE_TYPE *v) \
{ \
  int i; \
  int err = 0; \
  for (i = 0; i < ARRAY_SIZE (u.a); i++) \
  { \
    union_##VALUE_TYPE union_x, union_y; \
    union_x.f = u.a[i]; \
    union_y.f = v[i]; \
    if (union_x.i != union_y.i) \
    { \
	    err++; \
	    PRINTF ("%i: " "%f" " != " "%f" "\n", \
		          i, v[i], u.a[i]); \
    } \
  } \
  return err; \
}

#if defined (AVX10_512BIT)
CHECK_EXP_MINMAX (union512, float, int)
CHECK_EXP_MINMAX (union512d, double, long int)
CHECK_EXP_MINMAX (union512bf16_bf, __bf16, short int)
CHECK_EXP_MINMAX (union512h, _Float16, short int)
#endif
CHECK_EXP_MINMAX (union256, float, int)
CHECK_EXP_MINMAX (union256d, double, long int)
CHECK_EXP_MINMAX (union128, float, int)
CHECK_EXP_MINMAX (union128d, double, long int)
CHECK_EXP_MINMAX (union256bf16_bf, __bf16, short int)
CHECK_EXP_MINMAX (union128bf16_bf, __bf16, short int)
CHECK_EXP_MINMAX (union256h, _Float16, short int)
CHECK_EXP_MINMAX (union128h, _Float16, short int)

#define UNION_CHECK_MINMAX(SIZE, NAME) EVAL(check_minmax_union, SIZE, NAME)

#define CMP(res, x, y, type, value, op1, np, op2, zero, num, mag) \
{ \
  union_##type union_a, union_b; \
  union_a.f = x; \
  union_b.f = y; \
  union_##type union_snan, union_qnan; \
  union_snan.f = SNAN_##type; \
  union_qnan.f = QNAN_##type; \
  bool flag = false; \
  if(num) \
  { \
    if(ISNAN(x) && ISNAN(y)) \
    { \
      if(IS_SNAN(union_a,type) || (IS_QNAN(union_a,type) && IS_QNAN(union_b,type))) \
      { \
        union_a.i |= value; \
        res = union_a.f; \
        flag = true; \
      } \
      else \
      { \
        union_b.i |= value; \
        res = union_b.f; \
        flag = true; \
      } \
    } \
    else if(ISNAN(x)) \
    { \
      res = y; \
      flag = true; \
    } \
    else if(ISNAN(y)) \
    { \
      res = x; \
      flag = true; \
    } \
  } \
  else \
  { \
    if(IS_SNAN(union_a,type) || (IS_QNAN(union_a,type) && !IS_SNAN(union_b,type))) \
    { \
      union_a.i |= value; \
      res = union_a.f; \
      flag = true; \
    } \
    else if(ISNAN(y)) \
    { \
      union_b.i |= value; \
      res = union_b.f; \
      flag = true; \
    } \
  } \
  if(!flag) \
  { \
    if(!mag) \
    { \
      if((x == zero && y == - zero) || (x == - zero && y == zero)) \
        res =  np zero; \
      else if(x op1 y) \
        res = x; \
      else \
        res = y; \
    } \
    else \
    { \
      if(ABS_##type(x) op2 ABS_##type(y)) \
        res = x; \
      else if(ABS_##type(y) op2 ABS_##type(x)) \
        res = y; \
      else \
      { \
	if((x == zero && y == - zero) || (x == - zero && y == zero)) \
	  res =  np zero; \
	else if(x op1 y) \
	  res = x; \
	else \
	  res = y; \
      } \
    } \
  } \
}

#define MINMAX(type, value, zero) \
type \
minmax_##type (type * a, type * b, int imm) \
{ \
  int op_select = imm & 0x03; \
  int sign_control = (imm & 0x0C) >> 2; \
  int nan_prop_select = (imm & 0x10) >> 4; \
  type tmp; \
  if(nan_prop_select == 0) \
    if(op_select == 0) \
      CMP(tmp, *a, *b, type, value, <=, -, <, zero, false, false) \
    else if(op_select == 1) \
      CMP(tmp, *a, *b, type, value, >=, +, >, zero, false, false) \
    else if(op_select == 2) \
      CMP(tmp, *a, *b, type, value, <=, -, <, zero, false, true) \
    else \
      CMP(tmp, *a, *b, type, value, >=, +, >, zero, false, true) \
  else \
    if(op_select == 0) \
      CMP(tmp, *a, *b, type, value, <=, -, <, zero, true, false) \
    else if(op_select == 1) \
      CMP(tmp, *a, *b, type, value, >=, +, >, zero, true, false) \
    else if(op_select == 2) \
      CMP(tmp, *a, *b, type, value, <=, -, <, zero, true, true) \
    else \
      CMP(tmp, *a, *b, type, value, >=, +, >, zero, true, true) \
  if(!ISNAN(tmp)) \
    if(sign_control == 0 && !ISNAN(*a)) \
      if((tmp < 0 && *a > 0) || (tmp > 0 && *a < 0)) \
	tmp = -tmp; \
    else if(sign_control == 2) \
      if(tmp < 0) tmp = -tmp; \
    else if(sign_control == 3) \
      if(tmp > 0) tmp = -tmp; \
  return tmp; \
}


MINMAX(double, 0x7ff8000000000000, 0.0)
MINMAX(float, 0x7fc00000, 0.0f)
MINMAX(_Float16, 0x7e00, 0.0f16)
MINMAX(__bf16, 0x7fc0, 0.0bf16)

#define UNIT_TEST(R, InsnSuffix, MaskType, type) \
  sign = -1; \
  for (i = 0; i < SIZE; i++) \
  { \
    src1.a[i] = i % 2 ? SNAN_##type : 1.5 + 34.67 * i * sign; \
    src2.a[i] = i % 3 ? QNAN_##type : -22.17 * i * sign; \
    sign = sign * -1; \
  } \
  for (i = 0; i < SIZE; i++) \
    res2.a[i] = DEFAULT_VALUE; \
  res1.x = INTRINSIC(_minmax_##InsnSuffix) (src1.x, src2.x, R); \
  res2.x = INTRINSIC(_mask_minmax_##InsnSuffix) (res2.x, mask, src1.x, src2.x, R); \
  res3.x = INTRINSIC(_maskz_minmax_##InsnSuffix) (mask, src1.x, src2.x, R); \
  CALC (res_ref, src1.a, src2.a, R); \
  if (UNION_CHECK_MINMAX (AVX512F_LEN, MaskType) (res1, res_ref)) \
    abort(); \
  MASK_MERGE (MaskType) (res_ref, mask, SIZE); \
  if (UNION_CHECK_MINMAX (AVX512F_LEN, MaskType) (res2, res_ref)) \
    abort(); \
  MASK_ZERO (MaskType) (res_ref, mask, SIZE); \
  if (UNION_CHECK_MINMAX (AVX512F_LEN, MaskType) (res3, res_ref)) \
    abort();

#define SCALAR_UNIT_TEST(R, InsnSuffix, MaskType, type) \
  sign = -1; \
  for (i = 0; i < SIZE; i++) \
  { \
    src1.a[i] = i % 2 ? SNAN_##type : 1.5 + 34.67 * i * sign; \
    src2.a[i] = i % 3 ? QNAN_##type : -22.17 * i * sign; \
    sign = sign * -1; \
  } \
  for (i = 0; i < SIZE; i++) \
    res2.a[i] = DEFAULT_VALUE; \
  res1.x = _mm_minmax_##InsnSuffix (src1.x, src2.x, R); \
  res2.x = _mm_mask_minmax_##InsnSuffix (res2.x, mask, src1.x, src2.x, R); \
  res3.x = _mm_maskz_minmax_##InsnSuffix (mask, src1.x, src2.x, R); \
  CALC (res_ref, src1.a, src2.a, R); \
  if (UNION_CHECK_MINMAX (128, MaskType) (res1, res_ref)) \
    abort(); \
  MASK_MERGE (MaskType) (res_ref, mask, 1); \
  if (UNION_CHECK_MINMAX (128, MaskType) (res2, res_ref)) \
    abort(); \
  MASK_ZERO (MaskType) (res_ref, mask, 1); \
  if (UNION_CHECK_MINMAX (128, MaskType) (res3, res_ref)) \
    abort();

#endif
