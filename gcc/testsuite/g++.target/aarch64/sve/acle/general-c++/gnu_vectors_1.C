// { dg-options "-msve-vector-bits=256 -std=gnu++2a" }

#include <arm_sve.h>

typedef uint8_t gnu_uint8_t __attribute__ ((vector_size (32)));
typedef int8_t gnu_int8_t __attribute__ ((vector_size (32)));

typedef int32_t gnu128_int32_t __attribute__ ((vector_size (128)));
typedef int32_t gnu32_int32_t __attribute__ ((vector_size (32)));

void
f (svuint8_t sve_u1, svint8_t sve_s1, svbool_t sve_b1, svbool_t sve_b2,
   gnu_uint8_t gnu_u1, gnu_int8_t gnu_s1, gnu128_int32_t gnu128_s1,
   int n, unsigned char uc)
{
  // Initialization

  svuint8_t init_sve_u1 = 0; // { dg-error {cannot convert 'int' to 'svuint8_t' in initialization} }
  svuint8_t init_sve_u2 = {};
  svuint8_t init_sve_u3 = { sve_u1 };
  svuint8_t init_sve_u4 = { gnu_u1 }; // { dg-error {cannot convert 'gnu_uint8_t'[^\n]* to 'unsigned char' in initialization} }
  svuint8_t init_sve_u5 = { sve_s1 }; // { dg-error {cannot convert 'svint8_t' to 'unsigned char' in initialization} }
  svuint8_t init_sve_u6 = { gnu_s1 }; // { dg-error {cannot convert 'gnu_int8_t'[^\n]* to 'unsigned char' in initialization} }
  svuint8_t init_sve_u7 = { 0 };
  svuint8_t init_sve_u8 = { sve_u1, sve_u1 }; // { dg-error {cannot convert 'svuint8_t' to 'unsigned char' in initialization} }
  svuint8_t init_sve_u9 = { gnu_u1, gnu_u1 }; // { dg-error {cannot convert 'gnu_uint8_t'[^\n]* to 'unsigned char' in initialization} }
  svuint8_t init_sve_u10 {};
  svuint8_t init_sve_u11 { sve_u1 };
  svuint8_t init_sve_u12 { gnu_u1 }; // { dg-error {cannot convert 'gnu_uint8_t'[^\n]* to 'unsigned char' in initialization} }
  svuint8_t init_sve_u13 { sve_s1 }; // { dg-error {cannot convert 'svint8_t' to 'unsigned char' in initialization} }
  svuint8_t init_sve_u14 { gnu_s1 }; // { dg-error {cannot convert 'gnu_int8_t'[^\n]* to 'unsigned char' in initialization} }
  svuint8_t init_sve_u15 { 0 };
  svuint8_t init_sve_u16 { sve_u1, sve_u1 }; // { dg-error {cannot convert 'svuint8_t' to 'unsigned char' in initialization} }
  svuint8_t init_sve_u17 { gnu_u1, gnu_u1 }; // { dg-error {cannot convert 'gnu_uint8_t'[^\n]* to 'unsigned char' in initialization} }
  svuint8_t init_sve_u18 (0); // { dg-error {cannot convert 'int' to 'svuint8_t' in initialization} }
  svuint8_t init_sve_u19 (sve_u1);
  svuint8_t init_sve_u20 (gnu_u1);
  svuint8_t init_sve_u21 (sve_s1); // { dg-error {cannot convert 'svint8_t' to 'svuint8_t' in initialization} }
  svuint8_t init_sve_u22 (gnu_s1); // { dg-error {cannot convert 'gnu_int8_t'[^\n]* to 'svuint8_t' in initialization} }

  gnu_uint8_t init_gnu_u1 = 0; // { dg-error {cannot convert 'int' to 'gnu_uint8_t'[^\n]* in initialization} }
  gnu_uint8_t init_gnu_u2 = {};
  gnu_uint8_t init_gnu_u3 = { sve_u1 }; // { dg-error {cannot convert 'svuint8_t' to 'unsigned char' in initialization} }
  gnu_uint8_t init_gnu_u4 = { gnu_u1 };
  gnu_uint8_t init_gnu_u5 = { sve_s1 }; // { dg-error {cannot convert 'svint8_t' to 'unsigned char' in initialization} }
  gnu_uint8_t init_gnu_u6 = { gnu_s1 }; // { dg-error {cannot convert 'gnu_int8_t'[^\n]* to 'unsigned char' in initialization} }
  gnu_uint8_t init_gnu_u7 = { 0 };
  gnu_uint8_t init_gnu_u8 = { sve_u1, sve_u1 }; // { dg-error {cannot convert 'svuint8_t' to 'unsigned char' in initialization} }
  gnu_uint8_t init_gnu_u9 = { gnu_u1, gnu_u1 }; // { dg-error {cannot convert 'gnu_uint8_t'[^\n]* to 'unsigned char' in initialization} }
  gnu_uint8_t init_gnu_u10 { sve_u1 }; // { dg-error {cannot convert 'svuint8_t' to 'unsigned char' in initialization} }
  gnu_uint8_t init_gnu_u11 { gnu_u1 };
  gnu_uint8_t init_gnu_u12 { sve_s1 }; // { dg-error {cannot convert 'svint8_t' to 'unsigned char' in initialization} }
  gnu_uint8_t init_gnu_u13 { gnu_s1 }; // { dg-error {cannot convert 'gnu_int8_t'[^\n]* to 'unsigned char' in initialization} }
  gnu_uint8_t init_gnu_u14 { 0 };
  gnu_uint8_t init_gnu_u15 { sve_u1, sve_u1 }; // { dg-error {cannot convert 'svuint8_t' to 'unsigned char' in initialization} }
  gnu_uint8_t init_gnu_u16 { gnu_u1, gnu_u1 }; // { dg-error {cannot convert 'gnu_uint8_t'[^\n]* to 'unsigned char' in initialization} }
  gnu_uint8_t init_gnu_u17 (0); // { dg-error {cannot convert 'int' to 'gnu_uint8_t'[^\n]* in initialization} }
  gnu_uint8_t init_gnu_u18 (sve_u1);
  gnu_uint8_t init_gnu_u19 (gnu_u1);
  gnu_uint8_t init_gnu_u20 (sve_s1); // { dg-error {cannot convert 'svint8_t' to 'gnu_uint8_t'[^\n]* in initialization} }
  gnu_uint8_t init_gnu_u21 (gnu_s1); // { dg-error {cannot convert 'gnu_int8_t'[^\n]* to 'gnu_uint8_t'[^\n]* in initialization} }

  // Boolean inits.
  svbool_t init_sve_b1 = 0; // { dg-error {cannot convert 'int' to 'svbool_t'} }
  svbool_t init_sve_b2 = {};
  svbool_t init_sve_b3 = { sve_b1 };
  svbool_t init_sve_b4 = { gnu_u1 }; // { dg-error {cannot convert 'gnu_uint8_t'[^\n]* to 'signed char:1'} }
  svbool_t init_sve_b5 = { sve_s1 }; // { dg-error {cannot convert 'svint8_t' to 'signed char:1'} }
  svbool_t init_sve_b6 = { gnu_s1 }; // { dg-error {cannot convert 'gnu_int8_t'[^\n]* to 'signed char:1'} }
  svbool_t init_sve_b7 = { 0 };

  svbool_t init_sve_b8 = { sve_u1, sve_u1 }; // { dg-error {cannot convert 'svuint8_t' to 'signed char:1'} }
  svbool_t init_sve_b9 = { gnu_u1, gnu_u1 }; // { dg-error {cannot convert 'gnu_uint8_t'[^\n]* to 'signed char:1'} }
  init_gnu_u8 = { sve_b1 }; // { dg-error {cannot convert '<brace-enclosed initializer list>' to 'gnu_uint8_t'[^\n]*} }
  init_gnu_u8 = { sve_b1, sve_b1 }; // { dg-error {cannot convert '<brace-enclosed initializer list>' to 'gnu_uint8_t'[^\n]*} }

  // Compound literals

  (svuint8_t) {};
  (svuint8_t) { 0 };
  (svuint8_t) { sve_u1 };
  (svuint8_t) { gnu_u1 }; // { dg-error {cannot convert 'gnu_uint8_t'[^\n]* to 'unsigned char' in initialization} }
  (svuint8_t) { sve_s1 }; // { dg-error {cannot convert 'svint8_t' to 'unsigned char' in initialization} }
  (svuint8_t) { gnu_s1 }; // { dg-error {cannot convert 'gnu_int8_t'[^\n]* to 'unsigned char' in initialization} }
  (svuint8_t) { sve_u1, sve_u1 }; // { dg-error {cannot convert 'svuint8_t' to 'unsigned char' in initialization} }
  (svuint8_t) { gnu_u1, gnu_u1 }; // { dg-error {cannot convert 'gnu_uint8_t'[^\n]* to 'unsigned char' in initialization} }

  (gnu_uint8_t) {};
  (gnu_uint8_t) { 0 };
  (gnu_uint8_t) { sve_u1 }; // { dg-error {cannot convert 'svuint8_t' to 'unsigned char' in initialization} }
  (gnu_uint8_t) { gnu_u1 };
  (gnu_uint8_t) { sve_s1 }; // { dg-error {cannot convert 'svint8_t' to 'unsigned char' in initialization} }
  (gnu_uint8_t) { gnu_s1 }; // { dg-error {cannot convert 'gnu_int8_t'[^\n]* to 'unsigned char' in initialization} }
  (gnu_uint8_t) { sve_u1, sve_u1 }; // { dg-error {cannot convert 'svuint8_t' to 'unsigned char' in initialization} }
  (gnu_uint8_t) { gnu_u1, gnu_u1 }; // { dg-error {cannot convert 'gnu_uint8_t'[^\n]* to 'unsigned char' in initialization} }

  // Boolean compound literals.

  (svbool_t) {};
  (svbool_t) { 0 };
  (svbool_t) { sve_b1 };
  (svbool_t) { gnu_u1 }; // { dg-error {cannot convert 'gnu_uint8_t'[^\n]* to 'signed char:1'} }
  (svbool_t) { sve_s1 }; // { dg-error {cannot convert 'svint8_t' to 'signed char:1'} }
  (svbool_t) { sve_u1 }; // { dg-error {cannot convert 'svuint8_t' to 'signed char:1'} }
  (svbool_t) { gnu_s1 }; // { dg-error {cannot convert 'gnu_int8_t'[^\n]* to 'signed char:1'} }
  (gnu_uint8_t) { sve_b1 }; // { dg-error {cannot convert 'svbool_t' to 'unsigned char'} }

  // Assignment

  sve_u1 = 0; // { dg-error {cannot convert 'int' to 'svuint8_t' in assignment} }
  sve_u1 = sve_u1;
  sve_u1 = gnu_u1;
  sve_u1 = sve_s1; // { dg-error {cannot convert 'svint8_t' to 'svuint8_t' in assignment} }
  sve_u1 = gnu_s1; // { dg-error {cannot convert 'gnu_int8_t'[^\n]* to 'svuint8_t' in assignment} }

  gnu_u1 = 0; // { dg-error {cannot convert 'int' to 'gnu_uint8_t'[^\n]* in assignment} }
  gnu_u1 = sve_u1;
  gnu_u1 = gnu_u1;
  gnu_u1 = sve_s1; // { dg-error {cannot convert 'svint8_t' to 'gnu_uint8_t'[^\n]* in assignment} }
  gnu_u1 = gnu_s1; // { dg-error {cannot convert 'gnu_int8_t'[^\n]* to 'gnu_uint8_t'[^\n]* in assignment} }

  // Boolean Assignments.

  sve_b1 = 0; // { dg-error {cannot convert 'int' to 'svbool_t'} }
  sve_b1 = sve_b1;
  sve_b1 = sve_s1; // { dg-error {cannot convert 'svint8_t' to 'svbool_t'} }
  sve_b1 = gnu_s1; // { dg-error {cannot convert 'gnu_int8_t'[^\n]* to 'svbool_t'} }
  gnu_u1 = sve_b1; // { dg-error {cannot convert 'svbool_t' to 'gnu_uint8_t'} }

  // Casts

  (void) sve_u1;
  (int) sve_u1; // { dg-error {cannot convert a vector of type 'svuint8_t' to type 'int'} }
  (bool) sve_u1;  // { dg-error {cannot convert a vector of type 'svuint8_t' to type 'bool'} }
  (svuint8_t) 0; // { dg-error {cannot convert a value of type 'int' to vector type 'svuint8_t'} }
  (svuint8_t) n; // { dg-error {cannot convert a value of type 'int' to vector type 'svuint8_t'} }
  (svuint8_t) sve_u1;
  (svuint8_t) gnu_u1;
  (svuint8_t) sve_s1;
  (svuint8_t) gnu_s1;

  (void) gnu_u1;
  (int) gnu_u1; // { dg-error {cannot convert a vector of type 'gnu_uint8_t'[^\n]* to type 'int'} }
  (bool) gnu_u1; // { dg-error {cannot convert a vector of type 'gnu_uint8_t'[^\n]* to type 'bool'} }
  (gnu_uint8_t) 0; // { dg-error {cannot convert a value of type 'int' to vector type 'gnu_uint8_t'} }
  (gnu_uint8_t) n; // { dg-error {cannot convert a value of type 'int' to vector type 'gnu_uint8_t'} }
  (gnu_uint8_t) sve_u1;
  (gnu_uint8_t) gnu_u1;
  (gnu_uint8_t) sve_s1;
  (gnu_uint8_t) gnu_s1;

  // Boolean casts.
  (void) sve_b1;
  (svbool_t) sve_b1;
  (svbool_t) gnu_u1; // { dg-error {cannot convert a value of type 'gnu_uint8_t'[^\n]* to vector type 'svbool_t' which has different size} }
  (svbool_t) sve_u1; // { dg-error {cannot convert a value of type 'svuint8_t' to vector type 'svbool_t' which has different size} }
  (svbool_t) 0;
  (svbool_t) n;
  (svint8_t) sve_b1; // { dg-error {cannot convert a value of type 'svbool_t' to vector type 'svint8_t' which has different size} }
  (gnu_uint8_t) sve_b1; // { dg-error {cannot convert a value of type 'svbool_t' to vector type 'gnu_uint8_t'[^\n]* which has different size} }
  (gnu_int8_t) sve_b1; // { dg-error {cannot convert a value of type 'svbool_t' to vector type 'gnu_int8_t'[^\n]* which has different size} }

  // Vector indexing.

  sve_u1[0];
  &sve_u1[0];

  gnu_u1[0];
  &gnu_u1[0];

  // Unary vector arithmetic.

  +sve_u1;
  -sve_u1;
  ~sve_u1;
  !sve_u1;

  *sve_u1; // { dg-error {invalid type argument of unary '\*'} }
  __real sve_u1; // { dg-error {wrong type argument to __real} }
  __imag sve_u1; // { dg-error {wrong type argument to __imag} }
  ++sve_u1;
  --sve_u1;
  sve_u1++;
  sve_u1--;

  +gnu_u1;
  -gnu_u1;
  ~gnu_u1;
  !gnu_u1;
  *gnu_u1; // { dg-error {invalid type argument of unary '\*'} }
  __real gnu_u1; // { dg-error {wrong type argument to __real} }
  __imag gnu_u1; // { dg-error {wrong type argument to __imag} }
  ++gnu_u1;
  --gnu_u1;
  gnu_u1++;
  gnu_u1--;

  // Boolean unary ops.

  +sve_b1;
  -sve_b1; // { dg-error {negation operation not permitted} }
  ~sve_b1;
  !sve_b1;
  *sve_b1; // { dg-error {invalid type argument of unary '\*'} }
  __real sve_b1; // { dg-error {wrong type argument to __real} }
  __imag sve_b1; // { dg-error {wrong type argument to __imag} }
  ++sve_b1; // { dg-error {not permitted} }
  --sve_b1; // { dg-error {not permitted} }
  sve_b1++; // { dg-error {not permitted} }
  sve_b1--; // { dg-error {not permitted} }

  // Vector-vector binary arithmetic.

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
  sve_u1 <=> sve_u1; // { dg-message {three-way comparison of vectors} }
  sve_u1 << sve_u1;
  sve_u1 >> sve_u1;
  sve_u1 && sve_u1;
  sve_u1 || sve_u1;

  // Boolean vector-vector binary arithmetic.

  sve_b1 + sve_b1; // { dg-error {not permitted} }
  sve_b1 - sve_b1; // { dg-error {not permitted} }
  sve_b1 * sve_b1; // { dg-error {not permitted} }
  sve_b1 / sve_b1; // { dg-error {not permitted} }
  sve_b1 % sve_b1; // { dg-error {invalid operands} }
  sve_b1 & sve_b1;
  sve_b1 | sve_b1;
  sve_b1 ^ sve_b1;
  sve_b1 == sve_b1;
  sve_b1 != sve_b1;
  sve_b1 <= sve_b1; // { dg-error {only == and != operations permitted} }
  sve_b1 < sve_b1;  // { dg-error {only == and != operations permitted} }
  sve_b1 > sve_b1;  // { dg-error {only == and != operations permitted} }
  sve_b1 >= sve_b1; // { dg-error {only == and != operations permitted} }
  sve_b1 << sve_b1; // { dg-error {not permitted} }
  sve_b1 >> sve_b1; // { dg-error {not permitted} }
  sve_b1 && sve_b1;
  sve_b1 || sve_b1;

  sve_u1 + gnu_u1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 - gnu_u1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 * gnu_u1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 / gnu_u1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 % gnu_u1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 & gnu_u1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 | gnu_u1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 ^ gnu_u1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 == gnu_u1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 != gnu_u1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 <= gnu_u1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 < gnu_u1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 > gnu_u1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 >= gnu_u1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 <=> gnu_u1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 << gnu_u1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 >> gnu_u1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 && gnu_u1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 || gnu_u1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }

  gnu_u1 + sve_u1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 - sve_u1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 * sve_u1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 / sve_u1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 % sve_u1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 & sve_u1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 | sve_u1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 ^ sve_u1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 == sve_u1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 != sve_u1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 <= sve_u1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 < sve_u1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 > sve_u1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 >= sve_u1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 <=> sve_u1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 << sve_u1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 >> sve_u1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 && sve_u1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 || sve_u1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }

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
  // This is a target-independent sorry.  There's no ACLE reason why it
  // needs to be kept.
  gnu_u1 <=> gnu_u1; // { dg-message {three-way comparison of vectors} }
  gnu_u1 << gnu_u1;
  gnu_u1 >> gnu_u1;
  gnu_u1 && gnu_u1;
  gnu_u1 || gnu_u1;

  // Vector-scalar binary arithmetic.

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
  sve_u1 <=> 2; // { dg-error {invalid operands of types 'svuint8_t' and 'int' to binary 'operator<=>'} }
  sve_u1 << 2;
  sve_u1 >> 2;
  sve_u1 && 2;
  sve_u1 || 2;

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
  sve_u1 <=> uc; // { dg-error {invalid operands of types 'svuint8_t' and 'unsigned char' to binary 'operator<=>'} }
  sve_u1 << uc;
  sve_u1 >> uc;
  sve_u1 && uc;
  sve_u1 || uc;

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
  gnu_u1 <=> 2; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'int' to binary 'operator<=>'} }
  gnu_u1 << 2;
  gnu_u1 >> 2;
  gnu_u1 && 2;
  gnu_u1 || 2;

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
  gnu_u1 <=> uc; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'unsigned char' to binary 'operator<=>'} }
  gnu_u1 << uc;
  gnu_u1 >> uc;
  gnu_u1 && uc;
  gnu_u1 || uc;

  // Scalar-vector binary 'operatorarithmetic.

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
  3 <=> sve_u1; // { dg-error {invalid operands of types 'int' and 'svuint8_t' to binary 'operator<=>'} }
  3 < sve_u1;
  3 > sve_u1;
  3 >= sve_u1;
  3 << sve_u1;
  3 >> sve_u1;
  3 && sve_u1;
  3 || sve_u1;

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
  3 <=> gnu_u1; // { dg-error {invalid operands of types 'int' and 'gnu_uint8_t'[^\n]* to binary 'operator<=>'} }
  3 < gnu_u1;
  3 > gnu_u1;
  3 >= gnu_u1;
  3 << gnu_u1;
  3 >> gnu_u1;
  3 && gnu_u1;
  3 || gnu_u1;

  // Mismatched types.

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
  sve_u1 <=> sve_s1; // { dg-message {three-way comparison of vectors} }
  sve_u1 << sve_s1;
  sve_u1 >> sve_s1;

  sve_u1 + gnu_s1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 - gnu_s1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 * gnu_s1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 / gnu_s1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 % gnu_s1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 & gnu_s1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 | gnu_s1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 ^ gnu_s1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 == gnu_s1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 != gnu_s1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 <= gnu_s1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 < gnu_s1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 > gnu_s1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 >= gnu_s1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 <=> gnu_s1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 << gnu_s1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  sve_u1 >> gnu_s1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }

  gnu_u1 + sve_s1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 - sve_s1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 * sve_s1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 / sve_s1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 % sve_s1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 & sve_s1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 | sve_s1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 ^ sve_s1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 == sve_s1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 != sve_s1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 <= sve_s1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 < sve_s1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 > sve_s1; // { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 >= sve_s1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 <=> sve_s1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 << sve_s1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }
  gnu_u1 >> sve_s1;// { dg-error {cannot combine GNU and SVE vectors in a binary operation} }

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
  // This is a target-independent sorry.  There's no ACLE reason why it
  // needs to be kept.
  gnu_u1 <=> gnu_s1; // { dg-message {three-way comparison of vectors} }
  gnu_u1 << gnu_s1;
  gnu_u1 >> gnu_s1;

  // Conditional expressions.

  uc ? sve_u1 : sve_u1;
  uc ? gnu_u1 : sve_u1; // { dg-error {operands to '\?:' have different types 'gnu_uint8_t'[^\n]* and 'svuint8_t'} }
  uc ? sve_u1 : gnu_u1; // { dg-error {operands to '\?:' have different types 'svuint8_t' and 'gnu_uint8_t'} }
  uc ? gnu_u1 : gnu_u1;

  sve_u1 ? sve_u1 : sve_u1;
  sve_u1 ? gnu_u1 : sve_u1; // { dg-error {incompatible vector types in conditional expression} }
  sve_u1 ? sve_u1 : gnu_u1; // { dg-error {incompatible vector types in conditional expression} }
  sve_u1 ? gnu_u1 : gnu_u1;
  sve_u1 ? sve_u1 : uc;
  sve_u1 ? uc : sve_u1;
  sve_u1 ? gnu_u1 : uc;
  sve_u1 ? uc : gnu_u1;

  gnu_u1 ? sve_u1 : sve_u1;
  gnu_u1 ? gnu_u1 : sve_u1; // { dg-error {incompatible vector types in conditional expression} }
  gnu_u1 ? sve_u1 : gnu_u1; // { dg-error {incompatible vector types in conditional expression} }
  gnu_u1 ? gnu_u1 : gnu_u1;
  gnu_u1 ? sve_u1 : uc;
  gnu_u1 ? uc : sve_u1;
  gnu_u1 ? gnu_u1 : uc;
  gnu_u1 ? uc : gnu_u1;

  // Boolean conditional expressions.

  uc ? sve_b1 : sve_b2;
  sve_b1 ? sve_b1 : sve_b2;

  sve_b1 ? sve_u1 : sve_u1; // { dg-error {incompatible vector types in conditional expression} }
  sve_b1 ? sve_s1 : sve_s1; // { dg-error {incompatible vector types in conditional expression} }
  sve_b1 ? gnu_u1 : sve_u1; // { dg-error {incompatible vector types in conditional expression} }
  sve_b1 ? sve_u1 : gnu_u1; // { dg-error {incompatible vector types in conditional expression} }
  sve_b1 ? gnu_u1 : gnu_u1; // { dg-error {incompatible vector types in conditional expression} }
  sve_b1 ? gnu_s1 : gnu_s1; // { dg-error {incompatible vector types in conditional expression} }

  sve_u1 ? sve_b1 : sve_b2; // { dg-error {incompatible vector types in conditional expression} }
  sve_s1 ? sve_b1 : sve_b2; // { dg-error {incompatible vector types in conditional expression} }
  gnu_u1 ? sve_b1 : sve_b2; // { dg-error {incompatible vector types in conditional expression} }
  gnu_s1 ? sve_b1 : sve_b2; // { dg-error {incompatible vector types in conditional expression} }
  gnu_u1 ? gnu_u1 : sve_b1; // { dg-error {incompatible vector types in conditional expression} }
  gnu_u1 ? sve_b1 : gnu_u1; // { dg-error {incompatible vector types in conditional expression} }

  // Vector built-ins.

  __builtin_shuffle (sve_u1, sve_u1, sve_u1);
  __builtin_shuffle (sve_u1, gnu_u1, gnu_u1); // { dg-error {'__builtin_shuffle' argument vectors must be of the same type} }
  __builtin_shuffle (gnu_u1, sve_u1, gnu_u1); // { dg-error {'__builtin_shuffle' argument vectors must be of the same type} }
  __builtin_shuffle (gnu_u1, gnu_u1, sve_u1);
  __builtin_shuffle (gnu_u1, gnu_u1, gnu_u1);

  __builtin_convertvector (sve_u1, svuint8_t);
  __builtin_convertvector (gnu_u1, svuint8_t);
  __builtin_convertvector (sve_u1, gnu_uint8_t);
  __builtin_convertvector (gnu_u1, gnu_uint8_t);

  // Boolean vector built-ins.

  __builtin_shuffle (sve_b1, sve_b1, sve_s1);
  __builtin_shuffle (sve_b1, sve_b1, sve_u1);
  __builtin_shuffle (sve_b1, sve_b1, gnu_s1);
  __builtin_shuffle (sve_b1, sve_b1, gnu_u1);

  __builtin_shuffle (sve_b1, gnu_u1, gnu_u1); // { dg-error {'__builtin_shuffle' argument vectors must be of the same type} }
  __builtin_shuffle (gnu_u1, sve_b1, gnu_u1); // { dg-error {'__builtin_shuffle' argument vectors must be of the same type} }

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

  // Type queries.

  static_assert(__is_literal_type(svuint8_t));
  static_assert(__is_literal_type(gnu_uint8_t));

  // Pointers.

  svuint8_t *sve_ptr1 = &sve_u1;
  svuint8_t *sve_ptr2 = &gnu_u1; // { dg-error {invalid conversion} }
  svuint8_t *sve_ptr3 = &sve_s1; // { dg-error {cannot convert 'svint8_t\*' to 'svuint8_t\*' in initialization} }
  svuint8_t *sve_ptr4 = &gnu_s1; // { dg-error {cannot convert 'gnu_int8_t\*'[^\n]* to 'svuint8_t\*' in initialization} }

  gnu_uint8_t *gnu_ptr1 = &sve_u1; // { dg-error {invalid conversion} }
  gnu_uint8_t *gnu_ptr2 = &gnu_u1;
  gnu_uint8_t *gnu_ptr3 = &sve_s1; // { dg-error {cannot convert 'svint8_t\*' to 'gnu_uint8_t\*'} }
  gnu_uint8_t *gnu_ptr4 = &gnu_s1; // { dg-error {cannot convert 'gnu_int8_t\*'[^\n]* to 'gnu_uint8_t\*'} }

  // References.

  svuint8_t &sve_ref1 = sve_u1;
  svuint8_t &sve_ref2 = gnu_u1; // { dg-error {cannot bind non-const lvalue reference} }
  svuint8_t &sve_ref3 = sve_s1; // { dg-error {invalid initialization of reference of type 'svuint8_t\&' from expression of type 'svint8_t'} }
  svuint8_t &sve_ref4 = gnu_s1; // { dg-error {invalid initialization of reference of type 'svuint8_t\&' from expression of type 'gnu_int8_t'} }

  gnu_uint8_t &gnu_ref1 = sve_u1; // { dg-error {cannot bind non-const lvalue reference} }
  gnu_uint8_t &gnu_ref2 = gnu_u1;
  gnu_uint8_t &gnu_ref3 = sve_s1; // { dg-error {invalid initialization of reference of type 'gnu_uint8_t\&} }
  gnu_uint8_t &gnu_ref4 = gnu_s1; // { dg-error {invalid initialization of reference of type 'gnu_uint8_t\&} }
}

constexpr svuint8_t const1 (svuint8_t x) { return x; }
constexpr gnu_uint8_t const2 (gnu_uint8_t x) { return x; }
