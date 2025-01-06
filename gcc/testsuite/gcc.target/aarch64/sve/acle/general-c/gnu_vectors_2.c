/* { dg-options "-msve-vector-bits=256 -flax-vector-conversions" } */

#include <arm_sve.h>

typedef uint8_t gnu_uint8_t __attribute__ ((vector_size (32)));
typedef int8_t gnu_int8_t __attribute__ ((vector_size (32)));

typedef int32_t gnu128_int32_t __attribute__ ((vector_size (128)));
typedef int32_t gnu32_int32_t __attribute__ ((vector_size (32)));

void
f (svuint8_t sve_u1, svint8_t sve_s1, svbool_t sve_b1, svbool_t sve_b2,
   gnu_uint8_t gnu_u1, gnu_int8_t gnu_s1, gnu128_int32_t gnu128_s1, int n, unsigned char uc)
{
  /* Initialization.  */

  svuint8_t init_sve_u1 = 0; /* { dg-error {incompatible types when initializing type 'svuint8_t' using type 'int'} } */
  svuint8_t init_sve_u2 = {};
  svuint8_t init_sve_u3 = { sve_u1 }; /* { dg-error {incompatible types when initializing type 'unsigned char'} } */
  svuint8_t init_sve_u4 = { gnu_u1 }; /* { dg-error {incompatible types when initializing type 'unsigned char'} } */
  svuint8_t init_sve_u5 = { sve_s1 }; /* { dg-error {incompatible types when initializing type 'unsigned char'} } */
  svuint8_t init_sve_u6 = { gnu_s1 }; /* { dg-error {incompatible types when initializing type 'unsigned char'} } */
  svuint8_t init_sve_u7 = { 0 };
  svuint8_t init_sve_u8 = { sve_u1, sve_u1 }; /* { dg-error {incompatible types when initializing type 'unsigned char'} } */
  svuint8_t init_sve_u9 = { gnu_u1, gnu_u1 }; /* { dg-error {incompatible types when initializing type 'unsigned char'} } */

  /* Boolean inits.  */
  svbool_t init_sve_b1 = 0; /* { dg-error {incompatible types when initializing type 'svbool_t' using type 'int'} } */
  svbool_t init_sve_b2 = {};
  svbool_t init_sve_b3 = { sve_b1 }; /* { dg-error {incompatible types when initializing type 'signed char:1'} } */
  svbool_t init_sve_b4 = { gnu_u1 }; /* { dg-error {incompatible types when initializing type 'signed char:1'} } */
  svbool_t init_sve_b5 = { sve_s1 }; /* { dg-error {incompatible types when initializing type 'signed char:1'} } */
  svbool_t init_sve_b6 = { gnu_s1 }; /* { dg-error {incompatible types when initializing type 'signed char:1'} } */
  svbool_t init_sve_b7 = { 0 };

  gnu_uint8_t init_gnu_u1 = 0; /* { dg-error {incompatible types when initializing type 'gnu_uint8_t'[^\n]* using type 'int'} } */
  gnu_uint8_t init_gnu_u2 = {};
  gnu_uint8_t init_gnu_u3 = { sve_u1 }; /* { dg-error {incompatible types when initializing type 'unsigned char'} } */
  gnu_uint8_t init_gnu_u4 = { gnu_u1 }; /* { dg-error {incompatible types when initializing type 'unsigned char'} } */
  gnu_uint8_t init_gnu_u5 = { sve_s1 }; /* { dg-error {incompatible types when initializing type 'unsigned char'} } */
  gnu_uint8_t init_gnu_u6 = { gnu_s1 }; /* { dg-error {incompatible types when initializing type 'unsigned char'} } */
  gnu_uint8_t init_gnu_u7 = { 0 };

  /* Boolean inits.  */
  svbool_t init_sve_b8 = { sve_u1, sve_u1 }; /* { dg-error {incompatible types when initializing type 'signed char:1'} } */
  svbool_t init_sve_b9 = { gnu_u1, gnu_u1 }; /* { dg-error {incompatible types when initializing type 'signed char:1'} } */
  gnu_uint8_t init_gnu_u8 = { sve_b1 }; /* { dg-error {incompatible types when initializing type 'unsigned char'} } */

  /* Compound literals.  */

  (svuint8_t) {};
  (svuint8_t) { 0 };
  (svuint8_t) { sve_u1 }; /* { dg-error {incompatible types when initializing type 'unsigned char'} } */
  (svuint8_t) { gnu_u1 }; /* { dg-error {incompatible types when initializing type 'unsigned char'} } */
  (svuint8_t) { sve_s1 }; /* { dg-error {incompatible types when initializing type 'unsigned char'} } */
  (svuint8_t) { gnu_s1 }; /* { dg-error {incompatible types when initializing type 'unsigned char'} } */

  (gnu_uint8_t) {};
  (gnu_uint8_t) { 0 };
  (gnu_uint8_t) { sve_u1 }; /* { dg-error {incompatible types when initializing type 'unsigned char'} } */
  (gnu_uint8_t) { gnu_u1 }; /* { dg-error {incompatible types when initializing type 'unsigned char'} } */

  /* Boolean compound literals.  */
  (svbool_t) {};
  (svbool_t) { 0 };
  (svbool_t) { sve_b1 }; /* { dg-error {incompatible types when initializing type 'signed char:1'} } */
  (svbool_t) { gnu_u1 }; /* { dg-error {incompatible types when initializing type 'signed char:1'} } */
  (svbool_t) { sve_s1 }; /* { dg-error {incompatible types when initializing type 'signed char:1'} } */
  (svbool_t) { sve_u1 }; /* { dg-error {incompatible types when initializing type 'signed char:1'} } */
  (svbool_t) { gnu_s1 }; /* { dg-error {incompatible types when initializing type 'signed char:1'} } */
  (gnu_uint8_t) { sve_b1 }; /* { dg-error {incompatible types when initializing type 'unsigned char'} } */

  /* Assignment.  */

  sve_u1 = 0; /* { dg-error {incompatible types when assigning to type 'svuint8_t' from type 'int'} } */
  sve_u1 = sve_u1;
  sve_u1 = gnu_u1;
  sve_u1 = sve_s1;
  sve_u1 = gnu_s1;

  gnu_u1 = 0; /* { dg-error {incompatible types when assigning to type 'gnu_uint8_t'[^\n]* from type 'int'} } */
  gnu_u1 = sve_u1;
  gnu_u1 = gnu_u1;
  gnu_u1 = sve_s1;
  gnu_u1 = gnu_s1;

  /* Boolean Assignments.  */

  sve_b1 = 0; /* { dg-error {incompatible types when assigning to type 'svbool_t' from type 'int'} } */
  sve_b1 = sve_b1;
  sve_b1 = sve_s1; /* { dg-error {incompatible types when assigning to type 'svbool_t' from type 'svint8_t'} } */
  sve_b1 = gnu_s1; /* { dg-error {incompatible types when assigning to type 'svbool_t' from type 'gnu_int8_t'} } */
  gnu_u1 = sve_b1; /* { dg-error {incompatible types when assigning to type 'gnu_uint8_t'[^\n]* from type 'svbool_t'} } */

  /* Casts.  */

  (void) sve_u1;
  (svuint8_t) sve_u1;
  (svuint8_t) gnu_u1;
  (svuint8_t) 0; /* { dg-error {cannot convert a value of type 'int' to vector type '__SVUint8_t' which has different size} } */
  (svuint8_t) n; /* { dg-error {cannot convert a value of type 'int' to vector type '__SVUint8_t' which has different size} } */
  (svint8_t) sve_u1;
  (svint8_t) gnu_u1;

  (void) gnu_u1;
  (gnu_uint8_t) sve_u1;
  (gnu_uint8_t) gnu_u1;
  (gnu_uint8_t) 0; /* { dg-error {cannot convert a value of type 'int' to vector type '[^']*' which has different size} } */
  (gnu_uint8_t) n; /* { dg-error {cannot convert a value of type 'int' to vector type '[^']*' which has different size} } */
  (gnu_int8_t) sve_u1;
  (gnu_int8_t) gnu_u1;

  /* Boolean casts.  */
  (void) sve_b1;
  (svbool_t) sve_b1;
  (svbool_t) gnu_u1; /* { dg-error {cannot convert a value of type 'gnu_uint8_t'} } */
  (svbool_t) sve_u1; /* { dg-error {cannot convert a value of type 'svuint8_t'} } */
  (svbool_t) 0; /* This is OK.  sizeof (svbool_t) == sizeof (int) for VL == 256.  */
  (svbool_t) n; /* This is OK.  sizeof (svbool_t) == sizeof (int) for VL == 256.  */
  (svbool_t) (short)0; /* { dg-error {cannot convert a value of type 'short int'} } */
  (svbool_t) (short)n; /* { dg-error {cannot convert a value of type 'short int'} } */
  (svint8_t) sve_b1; /* { dg-error {cannot convert a value of type 'svbool_t'} } */
  (gnu_uint8_t) sve_b1; /* { dg-error {cannot convert a value of type 'svbool_t'} } */
  (gnu_int8_t) sve_b1; /* { dg-error {cannot convert a value of type 'svbool_t'} } */

  /* Vector indexing.  */

  sve_u1[0];
  &sve_u1[0];

  gnu_u1[0];
  &gnu_u1[0];

  /* Unary operators.  */

  +sve_u1;
  -sve_u1;
  ~sve_u1;
  !sve_u1; /* { dg-error {wrong type argument to unary exclamation mark} } */
  *sve_u1; /* { dg-error {invalid type argument of unary '\*'} } */
  __real sve_u1; /* { dg-error {wrong type argument to __real} } */
  __imag sve_u1; /* { dg-error {wrong type argument to __imag} } */
  ++sve_u1;
  --sve_u1;
  sve_u1++;
  sve_u1--;

  +gnu_u1;
  -gnu_u1;
  ~gnu_u1;
  !gnu_u1; /* { dg-error {wrong type argument to unary exclamation mark} } */
  *gnu_u1; /* { dg-error {invalid type argument of unary '\*'} } */
  __real gnu_u1; /* { dg-error {wrong type argument to __real} } */
  __imag gnu_u1; /* { dg-error {wrong type argument to __imag} } */
  ++gnu_u1;
  --gnu_u1;
  gnu_u1++;
  gnu_u1--;

  /* Boolean unary ops.  */

  +sve_b1;
  -sve_b1; /* { dg-error {negation operation not permitted} } */
  ~sve_b1;
  !sve_b1; /* { dg-error {wrong type argument to unary exclamation mark} } */
  *sve_b1; /* { dg-error {invalid type argument of unary '\*'} } */
  __real sve_b1; /* { dg-error {wrong type argument to __real} } */
  __imag sve_b1; /* { dg-error {wrong type argument to __imag} } */
  ++sve_b1; /* { dg-error {not permitted} } */
  --sve_b1; /* { dg-error {not permitted} } */
  sve_b1++; /* { dg-error {not permitted} } */
  sve_b1--; /* { dg-error {not permitted} } */

  /* Vector-vector binary arithmetic.  */

  sve_u1 + sve_u1;
  sve_u1 - sve_u1;
  sve_u1 * sve_u1;
  sve_u1 / sve_u1;
  sve_u1 % sve_u1;
  sve_u1 & sve_u1;
  sve_u1 | sve_u1;
  sve_u1 ^ sve_u1;
  sve_u1 == sve_u1;
  sve_u1 != sve_u1;
  sve_u1 <= sve_u1;
  sve_u1 < sve_u1;
  sve_u1 > sve_u1;
  sve_u1 >= sve_u1;
  sve_u1 << sve_u1;
  sve_u1 >> sve_u1;
  sve_u1 && sve_u1; /* { dg-error {used vector type where scalar is required} } */
  sve_u1 || sve_u1; /* { dg-error {used vector type where scalar is required} } */

  /* Boolean vector-vector binary arithmetic.  */

  sve_b1 + sve_b1; /* { dg-error {not permitted} } */
  sve_b1 - sve_b1; /* { dg-error {not permitted} } */
  sve_b1 * sve_b1; /* { dg-error {not permitted} } */
  sve_b1 / sve_b1; /* { dg-error {not permitted} } */
  sve_b1 % sve_b1; /* { dg-error {invalid operands to binary} } */
  sve_b1 & sve_b1;
  sve_b1 | sve_b1;
  sve_b1 ^ sve_b1;
  sve_b1 == sve_b1;
  sve_b1 != sve_b1;
  sve_b1 <= sve_b1; /* { dg-error {only == and != operations permitted} } */
  sve_b1 < sve_b1;  /* { dg-error {only == and != operations permitted} } */
  sve_b1 > sve_b1;  /* { dg-error {only == and != operations permitted} } */
  sve_b1 >= sve_b1; /* { dg-error {only == and != operations permitted} } */
  sve_b1 << sve_b1; /* { dg-error {not permitted} } */
  sve_b1 >> sve_b1; /* { dg-error {not permitted} } */
  sve_b1 && sve_b1; /* { dg-error {used vector type where scalar is required} } */
  sve_b1 || sve_b1; /* { dg-error {used vector type where scalar is required} } */

  sve_u1 + gnu_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 - gnu_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 * gnu_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 / gnu_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 % gnu_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 & gnu_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 | gnu_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 ^ gnu_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 == gnu_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 != gnu_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 <= gnu_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 < gnu_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 > gnu_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 >= gnu_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 << gnu_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 >> gnu_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 && gnu_u1; /* { dg-error {used vector type where scalar is required} } */
  sve_u1 || gnu_u1; /* { dg-error {used vector type where scalar is required} } */

  gnu_u1 + sve_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 - sve_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 * sve_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 / sve_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 % sve_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 & sve_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 | sve_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 ^ sve_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 == sve_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 != sve_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 <= sve_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 < sve_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 > sve_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 >= sve_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 << sve_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 >> sve_u1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 && sve_u1; /* { dg-error {used vector type where scalar is required} } */
  gnu_u1 || sve_u1; /* { dg-error {used vector type where scalar is required} } */

  gnu_u1 + gnu_u1;
  gnu_u1 - gnu_u1;
  gnu_u1 * gnu_u1;
  gnu_u1 / gnu_u1;
  gnu_u1 % gnu_u1;
  gnu_u1 & gnu_u1;
  gnu_u1 | gnu_u1;
  gnu_u1 ^ gnu_u1;
  gnu_u1 == gnu_u1;
  gnu_u1 != gnu_u1;
  gnu_u1 <= gnu_u1;
  gnu_u1 < gnu_u1;
  gnu_u1 > gnu_u1;
  gnu_u1 >= gnu_u1;
  gnu_u1 << gnu_u1;
  gnu_u1 >> gnu_u1;
  gnu_u1 && gnu_u1; /* { dg-error {used vector type where scalar is required} } */
  gnu_u1 || gnu_u1; /* { dg-error {used vector type where scalar is required} } */

  /* Vector-scalar binary arithmetic.  */

  sve_u1 + 2;
  sve_u1 - 2;
  sve_u1 * 2;
  sve_u1 / 2;
  sve_u1 % 2;
  sve_u1 & 2;
  sve_u1 | 2;
  sve_u1 ^ 2;
  sve_u1 == 2;
  sve_u1 != 2;
  sve_u1 <= 2;
  sve_u1 < 2;
  sve_u1 > 2;
  sve_u1 >= 2;
  sve_u1 << 2;
  sve_u1 >> 2;
  sve_u1 && 2; /* { dg-error {used vector type where scalar is required} } */
  sve_u1 || 2; /* { dg-error {used vector type where scalar is required} } */

  sve_u1 + uc;
  sve_u1 - uc;
  sve_u1 * uc;
  sve_u1 / uc;
  sve_u1 % uc;
  sve_u1 & uc;
  sve_u1 | uc;
  sve_u1 ^ uc;
  sve_u1 == uc;
  sve_u1 != uc;
  sve_u1 <= uc;
  sve_u1 < uc;
  sve_u1 > uc;
  sve_u1 >= uc;
  sve_u1 << uc;
  sve_u1 >> uc;
  sve_u1 && uc; /* { dg-error {used vector type where scalar is required} } */
  sve_u1 || uc; /* { dg-error {used vector type where scalar is required} } */

  gnu_u1 + 2;
  gnu_u1 - 2;
  gnu_u1 * 2;
  gnu_u1 / 2;
  gnu_u1 % 2;
  gnu_u1 & 2;
  gnu_u1 | 2;
  gnu_u1 ^ 2;
  gnu_u1 == 2;
  gnu_u1 != 2;
  gnu_u1 <= 2;
  gnu_u1 < 2;
  gnu_u1 > 2;
  gnu_u1 >= 2;
  gnu_u1 << 2;
  gnu_u1 >> 2;
  gnu_u1 && 2; /* { dg-error {used vector type where scalar is required} } */
  gnu_u1 || 2; /* { dg-error {used vector type where scalar is required} } */

  gnu_u1 + uc;
  gnu_u1 - uc;
  gnu_u1 * uc;
  gnu_u1 / uc;
  gnu_u1 % uc;
  gnu_u1 & uc;
  gnu_u1 | uc;
  gnu_u1 ^ uc;
  gnu_u1 == uc;
  gnu_u1 != uc;
  gnu_u1 <= uc;
  gnu_u1 < uc;
  gnu_u1 > uc;
  gnu_u1 >= uc;
  gnu_u1 << uc;
  gnu_u1 >> uc;
  gnu_u1 && uc; /* { dg-error {used vector type where scalar is required} } */
  gnu_u1 || uc; /* { dg-error {used vector type where scalar is required} } */

  /* Scalar-vector binary arithmetic.  */

  3 + sve_u1;
  3 - sve_u1;
  3 * sve_u1;
  3 / sve_u1;
  3 % sve_u1;
  3 & sve_u1;
  3 | sve_u1;
  3 ^ sve_u1;
  3 == sve_u1;
  3 != sve_u1;
  3 <= sve_u1;
  3 < sve_u1;
  3 > sve_u1;
  3 >= sve_u1;
  3 << sve_u1;
  3 >> sve_u1;
  3 && sve_u1; /* { dg-error {invalid operands to binary \&\&} } */
  3 || sve_u1; /* { dg-error {invalid operands to binary \|\|} } */

  3 + gnu_u1;
  3 - gnu_u1;
  3 * gnu_u1;
  3 / gnu_u1;
  3 % gnu_u1;
  3 & gnu_u1;
  3 | gnu_u1;
  3 ^ gnu_u1;
  3 == gnu_u1;
  3 != gnu_u1;
  3 <= gnu_u1;
  3 < gnu_u1;
  3 > gnu_u1;
  3 >= gnu_u1;
  3 << gnu_u1;
  3 >> gnu_u1;
  3 && gnu_u1; /* { dg-error {invalid operands to binary \&\&} } */
  3 || gnu_u1; /* { dg-error {invalid operands to binary \|\|} } */

  /* Mismatched types.  */

  sve_u1 + sve_s1;
  sve_u1 - sve_s1;
  sve_u1 * sve_s1;
  sve_u1 / sve_s1;
  sve_u1 % sve_s1;
  sve_u1 & sve_s1;
  sve_u1 | sve_s1;
  sve_u1 ^ sve_s1;
  sve_u1 == sve_s1;
  sve_u1 != sve_s1;
  sve_u1 <= sve_s1;
  sve_u1 < sve_s1;
  sve_u1 > sve_s1;
  sve_u1 >= sve_s1;
  sve_u1 << sve_s1;
  sve_u1 >> sve_s1;
  sve_u1 && sve_s1; /* { dg-error {used vector type where scalar is required} } */
  sve_u1 || sve_s1; /* { dg-error {used vector type where scalar is required} } */

  sve_u1 + gnu_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 - gnu_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 * gnu_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 / gnu_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 % gnu_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 & gnu_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 | gnu_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 ^ gnu_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 == gnu_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 != gnu_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 <= gnu_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 < gnu_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 > gnu_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 >= gnu_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 << gnu_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 >> gnu_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  sve_u1 && gnu_s1; /* { dg-error {used vector type where scalar is required} } */
  sve_u1 || gnu_s1; /* { dg-error {used vector type where scalar is required} } */

  gnu_u1 + sve_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 - sve_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 * sve_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 / sve_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 % sve_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 & sve_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 | sve_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 ^ sve_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 == sve_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 != sve_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 <= sve_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 < sve_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 > sve_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 >= sve_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 << sve_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 >> sve_s1; /* { dg-error {cannot combine GNU and SVE vectors in a binary operation} } */
  gnu_u1 && sve_s1; /* { dg-error {used vector type where scalar is required} } */
  gnu_u1 || sve_s1; /* { dg-error {used vector type where scalar is required} } */

  gnu_u1 + gnu_s1;
  gnu_u1 - gnu_s1;
  gnu_u1 * gnu_s1;
  gnu_u1 / gnu_s1;
  gnu_u1 % gnu_s1;
  gnu_u1 & gnu_s1;
  gnu_u1 | gnu_s1;
  gnu_u1 ^ gnu_s1;
  gnu_u1 == gnu_s1;
  gnu_u1 != gnu_s1;
  gnu_u1 <= gnu_s1;
  gnu_u1 < gnu_s1;
  gnu_u1 > gnu_s1;
  gnu_u1 >= gnu_s1;
  gnu_u1 << gnu_s1;
  gnu_u1 >> gnu_s1;
  gnu_u1 && gnu_s1; /* { dg-error {used vector type where scalar is required} } */
  gnu_u1 || gnu_s1; /* { dg-error {used vector type where scalar is required} } */

  /* Conditional expressions.  */

  uc ? sve_u1 : sve_u1;
  uc ? gnu_u1 : sve_u1; /* { dg-error {type mismatch in conditional expression} } */
  uc ? sve_u1 : gnu_u1; /* { dg-error {type mismatch in conditional expression} } */
  uc ? gnu_u1 : gnu_u1;

  sve_u1 ? sve_u1 : sve_u1; /* { dg-error {used vector type where scalar is required} } */
  sve_u1 ? gnu_u1 : sve_u1; /* { dg-error {used vector type where scalar is required} } */
  sve_u1 ? sve_u1 : gnu_u1; /* { dg-error {used vector type where scalar is required} } */
  sve_u1 ? gnu_u1 : gnu_u1; /* { dg-error {used vector type where scalar is required} } */

  gnu_u1 ? sve_u1 : sve_u1; /* { dg-error {used vector type where scalar is required} } */
  gnu_u1 ? gnu_u1 : sve_u1; /* { dg-error {used vector type where scalar is required} } */
  gnu_u1 ? sve_u1 : gnu_u1; /* { dg-error {used vector type where scalar is required} } */
  gnu_u1 ? gnu_u1 : gnu_u1; /* { dg-error {used vector type where scalar is required} } */

  /* Boolean conditional expressions.  */

  uc ? sve_b1 : sve_b2;

  sve_b1 ? sve_u1 : sve_u1; /* { dg-error {used vector type where scalar is required} } */
  sve_b1 ? gnu_u1 : sve_u1; /* { dg-error {used vector type where scalar is required} } */
  sve_b1 ? sve_u1 : gnu_u1; /* { dg-error {used vector type where scalar is required} } */
  sve_b1 ? gnu_u1 : gnu_u1; /* { dg-error {used vector type where scalar is required} } */

  sve_u1 ? sve_b1 : sve_b2; /* { dg-error {used vector type where scalar is required} } */
  gnu_u1 ? sve_b1 : sve_b2; /* { dg-error {used vector type where scalar is required} } */
  gnu_u1 ? gnu_u1 : sve_b1; /* { dg-error {used vector type where scalar is required} } */
  gnu_u1 ? sve_b1 : gnu_u1; /* { dg-error {used vector type where scalar is required} } */

  /* Vector built-ins.  */

  __builtin_shuffle (sve_u1, sve_u1, sve_u1);
  __builtin_shuffle (sve_u1, gnu_u1, gnu_u1); /* { dg-error {'__builtin_shuffle' argument vectors must be of the same type} } */
  __builtin_shuffle (gnu_u1, sve_u1, gnu_u1); /* { dg-error {'__builtin_shuffle' argument vectors must be of the same type} } */
  __builtin_shuffle (gnu_u1, gnu_u1, sve_u1);
  __builtin_shuffle (gnu_u1, gnu_u1, gnu_u1);

  __builtin_convertvector (sve_u1, svuint8_t);
  __builtin_convertvector (gnu_u1, svuint8_t);
  __builtin_convertvector (sve_u1, gnu_uint8_t);
  __builtin_convertvector (gnu_u1, gnu_uint8_t);

  /* Boolean vector built-ins.  */

  __builtin_shuffle (sve_b1, sve_b1, sve_s1);
  __builtin_shuffle (sve_b1, sve_b1, sve_u1);
  __builtin_shuffle (sve_b1, sve_b1, gnu_s1);
  __builtin_shuffle (sve_b1, sve_b1, gnu_u1);

  __builtin_shuffle (sve_b1, gnu_u1, gnu_u1); /* { dg-error {'__builtin_shuffle' argument vectors must be of the same type} } */
  __builtin_shuffle (gnu_u1, sve_b1, gnu_u1); /* { dg-error {'__builtin_shuffle' argument vectors must be of the same type} } */

  __builtin_convertvector (sve_b1, svint8_t);
  __builtin_convertvector (sve_b1, svuint8_t);
  __builtin_convertvector (sve_b1, gnu_int8_t);
  __builtin_convertvector (sve_b1, gnu_uint8_t);

  __builtin_convertvector (sve_s1, svbool_t);
  __builtin_convertvector (gnu_s1, svbool_t);
  __builtin_convertvector (sve_u1, svbool_t);
  __builtin_convertvector (gnu_u1, svbool_t);

  __builtin_convertvector (sve_b1, svint32_t); /* { dg-error {'__builtin_convertvector' number of elements of the first argument vector and the second argument vector type should be the same} } */
  __builtin_convertvector (sve_b1, svuint32_t); /* { dg-error {'__builtin_convertvector' number of elements of the first argument vector and the second argument vector type should be the same} } */
  __builtin_convertvector (sve_b1, gnu32_int32_t); /* { dg-error {'__builtin_convertvector' number of elements of the first argument vector and the second argument vector type should be the same} } */

  __builtin_convertvector (sve_b1, gnu128_int32_t);
  __builtin_convertvector (gnu128_s1, svbool_t);
}
