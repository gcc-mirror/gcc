// { dg-options "-msve-vector-bits=256 -std=gnu++2a -flax-vector-conversions" }

#include <arm_sve.h>

typedef uint8_t gnu_uint8_t __attribute__ ((vector_size (32)));
typedef int8_t gnu_int8_t __attribute__ ((vector_size (32)));

void
f (svuint8_t sve_u1, svint8_t sve_s1,
   gnu_uint8_t gnu_u1, gnu_int8_t gnu_s1, int n, unsigned char uc)
{
  // Initialization

  svuint8_t init_sve_u1 = 0; // { dg-error {cannot convert 'int' to 'svuint8_t' in initialization} }
  svuint8_t init_sve_u2 = {};
  svuint8_t init_sve_u3 = { sve_u1 };
  svuint8_t init_sve_u4 = { gnu_u1 };
  svuint8_t init_sve_u5 = { sve_s1 };
  svuint8_t init_sve_u6 = { gnu_s1 };
  svuint8_t init_sve_u7 = { 0 }; // { dg-error {cannot convert 'int' to 'svuint8_t' in initialization} }
  svuint8_t init_sve_u8 = { sve_u1, sve_u1 }; // { dg-error {too many initializers for 'svuint8_t'} }
  svuint8_t init_sve_u9 = { gnu_u1, gnu_u1 }; // { dg-error {too many initializers for 'svuint8_t'} }
  svuint8_t init_sve_u10 {};
  svuint8_t init_sve_u11 { sve_u1 };
  svuint8_t init_sve_u12 { gnu_u1 };
  svuint8_t init_sve_u13 { sve_s1 };
  svuint8_t init_sve_u14 { gnu_s1 };
  svuint8_t init_sve_u15 { 0 }; // { dg-error {cannot convert 'int' to 'svuint8_t' in initialization} }
  svuint8_t init_sve_u16 { sve_u1, sve_u1 }; // { dg-error {too many initializers for 'svuint8_t'} }
  svuint8_t init_sve_u17 { gnu_u1, gnu_u1 }; // { dg-error {too many initializers for 'svuint8_t'} }
  svuint8_t init_sve_u18 (0); // { dg-error {cannot convert 'int' to 'svuint8_t' in initialization} }
  svuint8_t init_sve_u19 (sve_u1);
  svuint8_t init_sve_u20 (gnu_u1);
  svuint8_t init_sve_u21 (sve_s1);
  svuint8_t init_sve_u22 (gnu_s1);

  gnu_uint8_t init_gnu_u1 = 0; // { dg-error {cannot convert 'int' to 'gnu_uint8_t'[^\n]* in initialization} }
  gnu_uint8_t init_gnu_u2 = {};
  gnu_uint8_t init_gnu_u3 = { sve_u1 };
  gnu_uint8_t init_gnu_u4 = { gnu_u1 };
  gnu_uint8_t init_gnu_u5 = { sve_s1 }; // { dg-error {cannot convert 'svint8_t' to 'unsigned char' in initialization} }
  gnu_uint8_t init_gnu_u6 = { gnu_s1 }; // { dg-error {cannot convert 'gnu_int8_t'[^\n]* to 'unsigned char' in initialization} }
  gnu_uint8_t init_gnu_u7 = { 0 };
  gnu_uint8_t init_gnu_u8 = { sve_u1, sve_u1 }; // { dg-error {cannot convert 'svuint8_t' to 'unsigned char' in initialization} }
  gnu_uint8_t init_gnu_u9 = { gnu_u1, gnu_u1 }; // { dg-error {cannot convert 'gnu_uint8_t'[^\n]* to 'unsigned char' in initialization} }
  gnu_uint8_t init_gnu_u10 { sve_u1 };
  gnu_uint8_t init_gnu_u11 { gnu_u1 };
  gnu_uint8_t init_gnu_u12 { sve_s1 }; // { dg-error {cannot convert 'svint8_t' to 'unsigned char' in initialization} }
  gnu_uint8_t init_gnu_u13 { gnu_s1 }; // { dg-error {cannot convert 'gnu_int8_t'[^\n]* to 'unsigned char' in initialization} }
  gnu_uint8_t init_gnu_u14 { 0 };
  gnu_uint8_t init_gnu_u15 { sve_u1, sve_u1 }; // { dg-error {cannot convert 'svuint8_t' to 'unsigned char' in initialization} }
  gnu_uint8_t init_gnu_u16 { gnu_u1, gnu_u1 }; // { dg-error {cannot convert 'gnu_uint8_t'[^\n]* to 'unsigned char' in initialization} }
  gnu_uint8_t init_gnu_u17 (0); // { dg-error {cannot convert 'int' to 'gnu_uint8_t'[^\n]* in initialization} }
  gnu_uint8_t init_gnu_u18 (sve_u1);
  gnu_uint8_t init_gnu_u19 (gnu_u1);
  gnu_uint8_t init_gnu_u20 (sve_s1);
  gnu_uint8_t init_gnu_u21 (gnu_s1);

  // Compound literals

  (svuint8_t) {};
  (svuint8_t) { 0 }; // { dg-error {cannot convert 'int' to 'svuint8_t' in initialization} }
  (svuint8_t) { sve_u1 };
  (svuint8_t) { gnu_u1 };
  (svuint8_t) { sve_s1 };
  (svuint8_t) { gnu_s1 };
  (svuint8_t) { sve_u1, sve_u1 }; // { dg-error {too many initializers for 'svuint8_t'} }
  (svuint8_t) { gnu_u1, gnu_u1 }; // { dg-error {too many initializers for 'svuint8_t'} }

  (gnu_uint8_t) {};
  (gnu_uint8_t) { 0 };
  (gnu_uint8_t) { sve_u1 };
  (gnu_uint8_t) { gnu_u1 };
  (gnu_uint8_t) { sve_s1 }; // { dg-error {cannot convert 'svint8_t' to 'unsigned char' in initialization} }
  (gnu_uint8_t) { gnu_s1 }; // { dg-error {cannot convert 'gnu_int8_t'[^\n]* to 'unsigned char' in initialization} }
  (gnu_uint8_t) { sve_u1, sve_u1 }; // { dg-error {cannot convert 'svuint8_t' to 'unsigned char' in initialization} }
  (gnu_uint8_t) { gnu_u1, gnu_u1 }; // { dg-error {cannot convert 'gnu_uint8_t'[^\n]* to 'unsigned char' in initialization} }

  // Assignment

  sve_u1 = 0; // { dg-error {cannot convert 'int' to 'svuint8_t' in assignment} }
  sve_u1 = sve_u1;
  sve_u1 = gnu_u1;
  sve_u1 = sve_s1;
  sve_u1 = gnu_s1;

  gnu_u1 = 0; // { dg-error {cannot convert 'int' to 'gnu_uint8_t'[^\n]* in assignment} }
  gnu_u1 = sve_u1;
  gnu_u1 = gnu_u1;
  gnu_u1 = sve_s1;
  gnu_u1 = gnu_s1;

  // Casts

  (void) sve_u1;
  (int) sve_u1; // { dg-error {invalid cast from type 'svuint8_t' to type 'int'} }
  (bool) sve_u1; // { dg-error {invalid cast from type 'svuint8_t' to type 'bool'} }
  (svuint8_t) 0; // { dg-error {invalid cast from type 'int' to type 'svuint8_t'} }
  (svuint8_t) n; // { dg-error {invalid cast from type 'int' to type 'svuint8_t'} }
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

  // Vector indexing.

  sve_u1[0]; // { dg-error {subscripted value is neither array nor pointer} }
  &sve_u1[0]; // { dg-error {subscripted value is neither array nor pointer} }

  gnu_u1[0];
  &gnu_u1[0];

  // Unary vector arithmetic.

  +sve_u1; // { dg-error {wrong type argument to unary plus} }
  -sve_u1; // { dg-error {wrong type argument to unary minus} }
  ~sve_u1; // { dg-error {wrong type argument to bit-complement} }
  !sve_u1; // { dg-error {could not convert 'sve_u1' from 'svuint8_t' to 'bool'} }
        // { dg-error {in argument to unary !} "" { target *-*-* } .-1 }
  *sve_u1; // { dg-error {invalid type argument of unary '\*'} }
  __real sve_u1; // { dg-error {wrong type argument to __real} }
  __imag sve_u1; // { dg-error {wrong type argument to __imag} }
  ++sve_u1; // { dg-error {no pre-increment operator for type} }
  --sve_u1; // { dg-error {no pre-decrement operator for type} }
  sve_u1++; // { dg-error {no post-increment operator for type} }
  sve_u1--; // { dg-error {no post-decrement operator for type} }

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

  // Vector-vector binary arithmetic.

  sve_u1 + sve_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'svuint8_t' to binary 'operator\+'} }
  sve_u1 - sve_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'svuint8_t' to binary 'operator-'} }
  sve_u1 * sve_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'svuint8_t' to binary 'operator\*'} }
  sve_u1 / sve_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'svuint8_t' to binary 'operator/'} }
  sve_u1 % sve_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'svuint8_t' to binary 'operator%'} }
  sve_u1 & sve_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'svuint8_t' to binary 'operator\&'} }
  sve_u1 | sve_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'svuint8_t' to binary 'operator\|'} }
  sve_u1 ^ sve_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'svuint8_t' to binary 'operator\^'} }
  sve_u1 == sve_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'svuint8_t' to binary 'operator=='} }
  sve_u1 != sve_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'svuint8_t' to binary 'operator!='} }
  sve_u1 <= sve_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'svuint8_t' to binary 'operator<='} }
  sve_u1 < sve_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'svuint8_t' to binary 'operator<'} }
  sve_u1 > sve_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'svuint8_t' to binary 'operator>'} }
  sve_u1 >= sve_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'svuint8_t' to binary 'operator>='} }
  sve_u1 <=> sve_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'svuint8_t' to binary 'operator<=>'} }
  sve_u1 << sve_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'svuint8_t' to binary 'operator<<'} }
  sve_u1 >> sve_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'svuint8_t' to binary 'operator>>'} }
  sve_u1 && sve_u1; // { dg-error {could not convert 'sve_u1' from 'svuint8_t' to 'bool'} }
  sve_u1 || sve_u1; // { dg-error {could not convert 'sve_u1' from 'svuint8_t' to 'bool'} }

  sve_u1 + gnu_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_uint8_t'[^\n]* to binary 'operator\+'} }
  sve_u1 - gnu_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_uint8_t'[^\n]* to binary 'operator-'} }
  sve_u1 * gnu_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_uint8_t'[^\n]* to binary 'operator\*'} }
  sve_u1 / gnu_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_uint8_t'[^\n]* to binary 'operator/'} }
  sve_u1 % gnu_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_uint8_t'[^\n]* to binary 'operator%'} }
  sve_u1 & gnu_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_uint8_t'[^\n]* to binary 'operator\&'} }
  sve_u1 | gnu_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_uint8_t'[^\n]* to binary 'operator\|'} }
  sve_u1 ^ gnu_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_uint8_t'[^\n]* to binary 'operator\^'} }
  sve_u1 == gnu_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_uint8_t'[^\n]* to binary 'operator=='} }
  sve_u1 != gnu_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_uint8_t'[^\n]* to binary 'operator!='} }
  sve_u1 <= gnu_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_uint8_t'[^\n]* to binary 'operator<='} }
  sve_u1 < gnu_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_uint8_t'[^\n]* to binary 'operator<'} }
  sve_u1 > gnu_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_uint8_t'[^\n]* to binary 'operator>'} }
  sve_u1 >= gnu_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_uint8_t'[^\n]* to binary 'operator>='} }
  sve_u1 <=> gnu_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_uint8_t'[^\n]* to binary 'operator<=>'} }
  sve_u1 << gnu_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_uint8_t'[^\n]* to binary 'operator<<'} }
  sve_u1 >> gnu_u1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_uint8_t'[^\n]* to binary 'operator>>'} }
  sve_u1 && gnu_u1; // { dg-error {could not convert 'sve_u1' from 'svuint8_t' to 'bool'} }
  sve_u1 || gnu_u1; // { dg-error {could not convert 'sve_u1' from 'svuint8_t' to 'bool'} }

  gnu_u1 + sve_u1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svuint8_t' to binary 'operator\+'} }
  gnu_u1 - sve_u1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svuint8_t' to binary 'operator-'} }
  gnu_u1 * sve_u1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svuint8_t' to binary 'operator\*'} }
  gnu_u1 / sve_u1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svuint8_t' to binary 'operator/'} }
  gnu_u1 % sve_u1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svuint8_t' to binary 'operator%'} }
  gnu_u1 & sve_u1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svuint8_t' to binary 'operator\&'} }
  gnu_u1 | sve_u1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svuint8_t' to binary 'operator\|'} }
  gnu_u1 ^ sve_u1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svuint8_t' to binary 'operator\^'} }
  gnu_u1 == sve_u1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svuint8_t' to binary 'operator=='} }
  gnu_u1 != sve_u1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svuint8_t' to binary 'operator!='} }
  gnu_u1 <= sve_u1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svuint8_t' to binary 'operator<='} }
  gnu_u1 < sve_u1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svuint8_t' to binary 'operator<'} }
  gnu_u1 > sve_u1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svuint8_t' to binary 'operator>'} }
  gnu_u1 >= sve_u1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svuint8_t' to binary 'operator>='} }
  gnu_u1 <=> sve_u1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svuint8_t' to binary 'operator<=>'} }
  gnu_u1 << sve_u1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svuint8_t' to binary 'operator<<'} }
  gnu_u1 >> sve_u1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svuint8_t' to binary 'operator>>'} }
  gnu_u1 && sve_u1; // { dg-error {could not convert 'sve_u1' from 'svuint8_t' to 'bool'} }
  gnu_u1 || sve_u1; // { dg-error {could not convert 'sve_u1' from 'svuint8_t' to 'bool'} }

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

  sve_u1 + 2; // { dg-error {invalid operands of types 'svuint8_t' and 'int' to binary 'operator\+'} }
  sve_u1 - 2; // { dg-error {invalid operands of types 'svuint8_t' and 'int' to binary 'operator-'} }
  sve_u1 * 2; // { dg-error {invalid operands of types 'svuint8_t' and 'int' to binary 'operator\*'} }
  sve_u1 / 2; // { dg-error {invalid operands of types 'svuint8_t' and 'int' to binary 'operator/'} }
  sve_u1 % 2; // { dg-error {invalid operands of types 'svuint8_t' and 'int' to binary 'operator%'} }
  sve_u1 & 2; // { dg-error {invalid operands of types 'svuint8_t' and 'int' to binary 'operator\&'} }
  sve_u1 | 2; // { dg-error {invalid operands of types 'svuint8_t' and 'int' to binary 'operator\|'} }
  sve_u1 ^ 2; // { dg-error {invalid operands of types 'svuint8_t' and 'int' to binary 'operator\^'} }
  sve_u1 == 2; // { dg-error {invalid operands of types 'svuint8_t' and 'int' to binary 'operator=='} }
  sve_u1 != 2; // { dg-error {invalid operands of types 'svuint8_t' and 'int' to binary 'operator!='} }
  sve_u1 <= 2; // { dg-error {invalid operands of types 'svuint8_t' and 'int' to binary 'operator<='} }
  sve_u1 < 2; // { dg-error {invalid operands of types 'svuint8_t' and 'int' to binary 'operator<'} }
  sve_u1 > 2; // { dg-error {invalid operands of types 'svuint8_t' and 'int' to binary 'operator>'} }
  sve_u1 >= 2; // { dg-error {invalid operands of types 'svuint8_t' and 'int' to binary 'operator>='} }
  sve_u1 <=> 2; // { dg-error {invalid operands of types 'svuint8_t' and 'int' to binary 'operator<=>'} }
  sve_u1 << 2; // { dg-error {invalid operands of types 'svuint8_t' and 'int' to binary 'operator<<'} }
  sve_u1 >> 2; // { dg-error {invalid operands of types 'svuint8_t' and 'int' to binary 'operator>>'} }
  sve_u1 && 2; // { dg-error {could not convert 'sve_u1' from 'svuint8_t' to 'bool'} }
  sve_u1 || 2; // { dg-error {could not convert 'sve_u1' from 'svuint8_t' to 'bool'} }

  sve_u1 + uc; // { dg-error {invalid operands of types 'svuint8_t' and 'unsigned char' to binary 'operator\+'} }
  sve_u1 - uc; // { dg-error {invalid operands of types 'svuint8_t' and 'unsigned char' to binary 'operator-'} }
  sve_u1 * uc; // { dg-error {invalid operands of types 'svuint8_t' and 'unsigned char' to binary 'operator\*'} }
  sve_u1 / uc; // { dg-error {invalid operands of types 'svuint8_t' and 'unsigned char' to binary 'operator/'} }
  sve_u1 % uc; // { dg-error {invalid operands of types 'svuint8_t' and 'unsigned char' to binary 'operator%'} }
  sve_u1 & uc; // { dg-error {invalid operands of types 'svuint8_t' and 'unsigned char' to binary 'operator\&'} }
  sve_u1 | uc; // { dg-error {invalid operands of types 'svuint8_t' and 'unsigned char' to binary 'operator\|'} }
  sve_u1 ^ uc; // { dg-error {invalid operands of types 'svuint8_t' and 'unsigned char' to binary 'operator\^'} }
  sve_u1 == uc; // { dg-error {invalid operands of types 'svuint8_t' and 'unsigned char' to binary 'operator=='} }
  sve_u1 != uc; // { dg-error {invalid operands of types 'svuint8_t' and 'unsigned char' to binary 'operator!='} }
  sve_u1 <= uc; // { dg-error {invalid operands of types 'svuint8_t' and 'unsigned char' to binary 'operator<='} }
  sve_u1 < uc; // { dg-error {invalid operands of types 'svuint8_t' and 'unsigned char' to binary 'operator<'} }
  sve_u1 > uc; // { dg-error {invalid operands of types 'svuint8_t' and 'unsigned char' to binary 'operator>'} }
  sve_u1 >= uc; // { dg-error {invalid operands of types 'svuint8_t' and 'unsigned char' to binary 'operator>='} }
  sve_u1 <=> uc; // { dg-error {invalid operands of types 'svuint8_t' and 'unsigned char' to binary 'operator<=>'} }
  sve_u1 << uc; // { dg-error {invalid operands of types 'svuint8_t' and 'unsigned char' to binary 'operator<<'} }
  sve_u1 >> uc; // { dg-error {invalid operands of types 'svuint8_t' and 'unsigned char' to binary 'operator>>'} }
  sve_u1 && uc; // { dg-error {could not convert 'sve_u1' from 'svuint8_t' to 'bool'} }
  sve_u1 || uc; // { dg-error {could not convert 'sve_u1' from 'svuint8_t' to 'bool'} }

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

  3 + sve_u1; // { dg-error {invalid operands of types 'int' and 'svuint8_t' to binary 'operator\+'} }
  3 - sve_u1; // { dg-error {invalid operands of types 'int' and 'svuint8_t' to binary 'operator-'} }
  3 * sve_u1; // { dg-error {invalid operands of types 'int' and 'svuint8_t' to binary 'operator\*'} }
  3 / sve_u1; // { dg-error {invalid operands of types 'int' and 'svuint8_t' to binary 'operator/'} }
  3 % sve_u1; // { dg-error {invalid operands of types 'int' and 'svuint8_t' to binary 'operator%'} }
  3 & sve_u1; // { dg-error {invalid operands of types 'int' and 'svuint8_t' to binary 'operator\&'} }
  3 | sve_u1; // { dg-error {invalid operands of types 'int' and 'svuint8_t' to binary 'operator\|'} }
  3 ^ sve_u1; // { dg-error {invalid operands of types 'int' and 'svuint8_t' to binary 'operator\^'} }
  3 == sve_u1; // { dg-error {invalid operands of types 'int' and 'svuint8_t' to binary 'operator=='} }
  3 != sve_u1; // { dg-error {invalid operands of types 'int' and 'svuint8_t' to binary 'operator!='} }
  3 <= sve_u1; // { dg-error {invalid operands of types 'int' and 'svuint8_t' to binary 'operator<='} }
  3 <=> sve_u1; // { dg-error {invalid operands of types 'int' and 'svuint8_t' to binary 'operator<=>'} }
  3 < sve_u1; // { dg-error {invalid operands of types 'int' and 'svuint8_t' to binary 'operator<'} }
  3 > sve_u1; // { dg-error {invalid operands of types 'int' and 'svuint8_t' to binary 'operator>'} }
  3 >= sve_u1; // { dg-error {invalid operands of types 'int' and 'svuint8_t' to binary 'operator>='} }
  3 << sve_u1; // { dg-error {invalid operands of types 'int' and 'svuint8_t' to binary 'operator<<'} }
  3 >> sve_u1; // { dg-error {invalid operands of types 'int' and 'svuint8_t' to binary 'operator>>'} }
  3 && sve_u1; // { dg-error {could not convert 'sve_u1' from 'svuint8_t' to 'bool'} }
  3 || sve_u1; // { dg-error {could not convert 'sve_u1' from 'svuint8_t' to 'bool'} }

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

  sve_u1 + sve_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'svint8_t' to binary 'operator\+'} }
  sve_u1 - sve_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'svint8_t' to binary 'operator-'} }
  sve_u1 * sve_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'svint8_t' to binary 'operator\*'} }
  sve_u1 / sve_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'svint8_t' to binary 'operator/'} }
  sve_u1 % sve_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'svint8_t' to binary 'operator%'} }
  sve_u1 & sve_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'svint8_t' to binary 'operator\&'} }
  sve_u1 | sve_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'svint8_t' to binary 'operator\|'} }
  sve_u1 ^ sve_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'svint8_t' to binary 'operator\^'} }
  sve_u1 == sve_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'svint8_t' to binary 'operator=='} }
  sve_u1 != sve_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'svint8_t' to binary 'operator!='} }
  sve_u1 <= sve_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'svint8_t' to binary 'operator<='} }
  sve_u1 < sve_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'svint8_t' to binary 'operator<'} }
  sve_u1 > sve_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'svint8_t' to binary 'operator>'} }
  sve_u1 >= sve_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'svint8_t' to binary 'operator>='} }
  sve_u1 <=> sve_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'svint8_t' to binary 'operator<=>'} }
  sve_u1 << sve_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'svint8_t' to binary 'operator<<'} }
  sve_u1 >> sve_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'svint8_t' to binary 'operator>>'} }

  sve_u1 + gnu_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_int8_t'[^\n]* to binary 'operator\+'} }
  sve_u1 - gnu_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_int8_t'[^\n]* to binary 'operator-'} }
  sve_u1 * gnu_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_int8_t'[^\n]* to binary 'operator\*'} }
  sve_u1 / gnu_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_int8_t'[^\n]* to binary 'operator/'} }
  sve_u1 % gnu_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_int8_t'[^\n]* to binary 'operator%'} }
  sve_u1 & gnu_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_int8_t'[^\n]* to binary 'operator\&'} }
  sve_u1 | gnu_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_int8_t'[^\n]* to binary 'operator\|'} }
  sve_u1 ^ gnu_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_int8_t'[^\n]* to binary 'operator\^'} }
  sve_u1 == gnu_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_int8_t'[^\n]* to binary 'operator=='} }
  sve_u1 != gnu_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_int8_t'[^\n]* to binary 'operator!='} }
  sve_u1 <= gnu_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_int8_t'[^\n]* to binary 'operator<='} }
  sve_u1 < gnu_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_int8_t'[^\n]* to binary 'operator<'} }
  sve_u1 > gnu_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_int8_t'[^\n]* to binary 'operator>'} }
  sve_u1 >= gnu_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_int8_t'[^\n]* to binary 'operator>='} }
  sve_u1 <=> gnu_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_int8_t'[^\n]* to binary 'operator<=>'} }
  sve_u1 << gnu_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_int8_t'[^\n]* to binary 'operator<<'} }
  sve_u1 >> gnu_s1; // { dg-error {invalid operands of types 'svuint8_t' and 'gnu_int8_t'[^\n]* to binary 'operator>>'} }

  gnu_u1 + sve_s1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svint8_t' to binary 'operator\+'} }
  gnu_u1 - sve_s1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svint8_t' to binary 'operator-'} }
  gnu_u1 * sve_s1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svint8_t' to binary 'operator\*'} }
  gnu_u1 / sve_s1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svint8_t' to binary 'operator/'} }
  gnu_u1 % sve_s1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svint8_t' to binary 'operator%'} }
  gnu_u1 & sve_s1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svint8_t' to binary 'operator\&'} }
  gnu_u1 | sve_s1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svint8_t' to binary 'operator\|'} }
  gnu_u1 ^ sve_s1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svint8_t' to binary 'operator\^'} }
  gnu_u1 == sve_s1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svint8_t' to binary 'operator=='} }
  gnu_u1 != sve_s1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svint8_t' to binary 'operator!='} }
  gnu_u1 <= sve_s1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svint8_t' to binary 'operator<='} }
  gnu_u1 < sve_s1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svint8_t' to binary 'operator<'} }
  gnu_u1 > sve_s1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svint8_t' to binary 'operator>'} }
  gnu_u1 >= sve_s1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svint8_t' to binary 'operator>='} }
  gnu_u1 <=> sve_s1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svint8_t' to binary 'operator<=>'} }
  gnu_u1 << sve_s1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svint8_t' to binary 'operator<<'} }
  gnu_u1 >> sve_s1; // { dg-error {invalid operands of types 'gnu_uint8_t'[^\n]* and 'svint8_t' to binary 'operator>>'} }

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
  uc ? gnu_u1 : sve_u1; // { dg-error {operands to '\?:' have different types 'gnu_uint8_t'[^\n]* and 'svuint8_t'} "" { xfail *-*-* } }
  uc ? sve_u1 : gnu_u1; // { dg-error {operands to '\?:' have different types 'svuint8_t' and 'gnu_uint8_t'} "" { xfail *-*-* } }
  uc ? gnu_u1 : gnu_u1;

  sve_u1 ? sve_u1 : sve_u1; // { dg-error {could not convert 'sve_u1' from 'svuint8_t' to 'bool'} }
  sve_u1 ? gnu_u1 : sve_u1; // { dg-error {could not convert 'sve_u1' from 'svuint8_t' to 'bool'} }
  sve_u1 ? sve_u1 : gnu_u1; // { dg-error {could not convert 'sve_u1' from 'svuint8_t' to 'bool'} }
  sve_u1 ? gnu_u1 : gnu_u1; // { dg-error {could not convert 'sve_u1' from 'svuint8_t' to 'bool'} }
  sve_u1 ? sve_u1 : uc; // { dg-error {could not convert 'sve_u1' from 'svuint8_t' to 'bool'} }
  sve_u1 ? uc : sve_u1; // { dg-error {could not convert 'sve_u1' from 'svuint8_t' to 'bool'} }
  sve_u1 ? gnu_u1 : uc; // { dg-error {could not convert 'sve_u1' from 'svuint8_t' to 'bool'} }
  sve_u1 ? uc : gnu_u1; // { dg-error {could not convert 'sve_u1' from 'svuint8_t' to 'bool'} }

  gnu_u1 ? sve_u1 : sve_u1; // { dg-error {incompatible vector types in conditional expression} }
  gnu_u1 ? gnu_u1 : sve_u1; // { dg-error {incompatible vector types in conditional expression} }
  gnu_u1 ? sve_u1 : gnu_u1; // { dg-error {incompatible vector types in conditional expression} }
  gnu_u1 ? gnu_u1 : gnu_u1;
  gnu_u1 ? sve_u1 : uc; // { dg-error {incompatible vector types in conditional expression} }
  gnu_u1 ? uc : sve_u1; // { dg-error {incompatible vector types in conditional expression} }
  gnu_u1 ? gnu_u1 : uc;
  gnu_u1 ? uc : gnu_u1;

  // Vector built-ins.

  __builtin_shuffle (sve_u1, sve_u1, sve_u1); // { dg-error {'__builtin_shuffle' last argument must be an integer vector} }
  __builtin_shuffle (sve_u1, gnu_u1, gnu_u1); // { dg-error {'__builtin_shuffle' arguments must be vectors} }
  __builtin_shuffle (gnu_u1, sve_u1, gnu_u1); // { dg-error {'__builtin_shuffle' arguments must be vectors} }
  __builtin_shuffle (gnu_u1, gnu_u1, sve_u1); // { dg-error {'__builtin_shuffle' last argument must be an integer vector} }
  __builtin_shuffle (gnu_u1, gnu_u1, gnu_u1);

  __builtin_convertvector (sve_u1, svuint8_t); // { dg-error {'__builtin_convertvector' first argument must be an integer or floating vector} }
  __builtin_convertvector (gnu_u1, svuint8_t); // { dg-error {'__builtin_convertvector' second argument must be an integer or floating vector type} }
  __builtin_convertvector (sve_u1, gnu_uint8_t); // { dg-error {'__builtin_convertvector' first argument must be an integer or floating vector} }
  __builtin_convertvector (gnu_u1, gnu_uint8_t);

  // Type queries.

  static_assert(__is_literal_type(svuint8_t));
  static_assert(__is_literal_type(gnu_uint8_t));

  svuint8_t *sve_ptr1 = &sve_u1;
  svuint8_t *sve_ptr2 = &gnu_u1;
  svuint8_t *sve_ptr3 = &sve_s1; // { dg-error {invalid conversion from 'svint8_t\*' to 'svuint8_t\*'} }
  svuint8_t *sve_ptr4 = &gnu_s1; // { dg-error {invalid conversion from 'gnu_int8_t\*'[^\n]* to 'svuint8_t\*'} }

  gnu_uint8_t *gnu_ptr1 = &sve_u1;
  gnu_uint8_t *gnu_ptr2 = &gnu_u1;
  gnu_uint8_t *gnu_ptr3 = &sve_s1; // { dg-error {invalid conversion from 'svint8_t\*' to 'gnu_uint8_t\*'} }
  gnu_uint8_t *gnu_ptr4 = &gnu_s1; // { dg-error {invalid conversion from 'gnu_int8_t\*'[^\n]* to 'gnu_uint8_t\*'} }
}

constexpr svuint8_t const1 (svuint8_t x) { return x; }
constexpr gnu_uint8_t const2 (gnu_uint8_t x) { return x; }
