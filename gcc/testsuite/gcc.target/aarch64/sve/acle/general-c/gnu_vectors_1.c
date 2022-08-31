/* { dg-options "-msve-vector-bits=256" } */

#include <arm_sve.h>

typedef uint8_t gnu_uint8_t __attribute__ ((vector_size (32)));
typedef int8_t gnu_int8_t __attribute__ ((vector_size (32)));

void
f (svuint8_t sve_u1, svint8_t sve_s1,
   gnu_uint8_t gnu_u1, gnu_int8_t gnu_s1, int n, unsigned char uc)
{
  /* Initialization.  */

  svuint8_t init_sve_u1 = 0; /* { dg-error {incompatible types when initializing type 'svuint8_t' using type 'int'} } */
  svuint8_t init_sve_u2 = {};
  svuint8_t init_sve_u3 = { sve_u1 };
  svuint8_t init_sve_u4 = { gnu_u1 };
  svuint8_t init_sve_u5 = { sve_s1 }; /* { dg-error {incompatible types when initializing type 'svuint8_t' using type 'svint8_t'} } */
  svuint8_t init_sve_u6 = { gnu_s1 }; /* { dg-error {incompatible types when initializing type 'svuint8_t'} } */
  svuint8_t init_sve_u7 = { 0 }; /* { dg-error {incompatible types when initializing type 'svuint8_t' using type 'int'} } */
  svuint8_t init_sve_u8 = { sve_u1, sve_u1 }; /* { dg-warning {excess elements in scalar initializer} } */
  svuint8_t init_sve_u9 = { gnu_u1, gnu_u1 }; /* { dg-warning {excess elements in scalar initializer} } */

  gnu_uint8_t init_gnu_u1 = 0; /* { dg-error {incompatible types when initializing type 'gnu_uint8_t'[^\n]* using type 'int'} } */
  gnu_uint8_t init_gnu_u2 = {};
  gnu_uint8_t init_gnu_u3 = { sve_u1 }; /* { dg-error {incompatible types when initializing type 'unsigned char'} } */
  gnu_uint8_t init_gnu_u4 = { gnu_u1 }; /* { dg-error {incompatible types when initializing type 'unsigned char'} } */
  gnu_uint8_t init_gnu_u5 = { sve_s1 }; /* { dg-error {incompatible types when initializing type 'unsigned char'} } */
  gnu_uint8_t init_gnu_u6 = { gnu_s1 }; /* { dg-error {incompatible types when initializing type 'unsigned char'} } */
  gnu_uint8_t init_gnu_u7 = { 0 };

  /* Compound literals.  */

  (svuint8_t) {};
  (svuint8_t) { 0 }; /* { dg-error {incompatible types when initializing type 'svuint8_t' using type 'int'} } */
  (svuint8_t) { sve_u1 };
  (svuint8_t) { gnu_u1 };
  (svuint8_t) { sve_s1 }; /* { dg-error {incompatible types when initializing type 'svuint8_t' using type 'svint8_t'} } */
  (svuint8_t) { gnu_s1 }; /* { dg-error {incompatible types when initializing type 'svuint8_t'} } */

  (gnu_uint8_t) {};
  (gnu_uint8_t) { 0 };
  (gnu_uint8_t) { sve_u1 }; /* { dg-error {incompatible types when initializing type 'unsigned char'} } */
  (gnu_uint8_t) { gnu_u1 }; /* { dg-error {incompatible types when initializing type 'unsigned char'} } */

  /* Assignment.  */

  sve_u1 = 0; /* { dg-error {incompatible types when assigning to type 'svuint8_t' from type 'int'} } */
  sve_u1 = sve_u1;
  sve_u1 = gnu_u1;
  sve_u1 = sve_s1; /* { dg-error {incompatible types when assigning to type 'svuint8_t' from type 'svint8_t'} } */
  sve_u1 = gnu_s1; /* { dg-error {incompatible types when assigning to type 'svuint8_t' from type 'gnu_int8_t'} } */

  gnu_u1 = 0; /* { dg-error {incompatible types when assigning to type 'gnu_uint8_t'[^\n]* from type 'int'} } */
  gnu_u1 = sve_u1;
  gnu_u1 = gnu_u1;
  gnu_u1 = sve_s1; /* { dg-error {incompatible types when assigning to type 'gnu_uint8_t'[^\n]* from type 'svint8_t'} } */
  gnu_u1 = gnu_s1; /* { dg-error {incompatible types when assigning to type 'gnu_uint8_t'[^\n]* from type 'gnu_int8_t'} } */

  /* Casts.  */

  (void) sve_u1;
  (svuint8_t) sve_u1;
  (svuint8_t) gnu_u1;
  (svuint8_t) 0; /* { dg-error {conversion to non-scalar type requested} } */
  (svuint8_t) n; /* { dg-error {conversion to non-scalar type requested} } */
  (svint8_t) sve_u1; /* { dg-error {conversion to non-scalar type requested} } */
  (svint8_t) gnu_u1;

  (void) gnu_u1;
  (gnu_uint8_t) sve_u1;
  (gnu_uint8_t) gnu_u1;
  (gnu_uint8_t) 0; /* { dg-error {cannot convert a value of type 'int' to vector type '[^']*' which has different size} } */
  (gnu_uint8_t) n; /* { dg-error {cannot convert a value of type 'int' to vector type '[^']*' which has different size} } */
  (gnu_int8_t) sve_u1;
  (gnu_int8_t) gnu_u1;

  /* Vector indexing.  */

  sve_u1[0]; /* { dg-error {subscripted value is neither array nor pointer} } */
  &sve_u1[0]; /* { dg-error {subscripted value is neither array nor pointer} } */

  gnu_u1[0];
  &gnu_u1[0];

  /* Unary operators.  */

  +sve_u1; /* { dg-error {wrong type argument to unary plus} } */
  -sve_u1; /* { dg-error {wrong type argument to unary minus} } */
  ~sve_u1; /* { dg-error {wrong type argument to bit-complement} } */
  !sve_u1; /* { dg-error {wrong type argument to unary exclamation mark} } */
  *sve_u1; /* { dg-error {invalid type argument of unary '\*'} } */
  __real sve_u1; /* { dg-error {wrong type argument to __real} } */
  __imag sve_u1; /* { dg-error {wrong type argument to __imag} } */
  ++sve_u1; /* { dg-error {wrong type argument to increment} } */
  --sve_u1; /* { dg-error {wrong type argument to decrement} } */
  sve_u1++; /* { dg-error {wrong type argument to increment} } */
  sve_u1--; /* { dg-error {wrong type argument to decrement} } */

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

  /* Vector-vector binary arithmetic.  */

  sve_u1 + sve_u1; /* { dg-error {invalid operands to binary \+} } */
  sve_u1 - sve_u1; /* { dg-error {invalid operands to binary -} } */
  sve_u1 * sve_u1; /* { dg-error {invalid operands to binary \*} } */
  sve_u1 / sve_u1; /* { dg-error {invalid operands to binary /} } */
  sve_u1 % sve_u1; /* { dg-error {invalid operands to binary %} } */
  sve_u1 & sve_u1; /* { dg-error {invalid operands to binary \&} } */
  sve_u1 | sve_u1; /* { dg-error {invalid operands to binary \|} } */
  sve_u1 ^ sve_u1; /* { dg-error {invalid operands to binary \^} } */
  sve_u1 == sve_u1; /* { dg-error {invalid operands to binary ==} } */
  sve_u1 != sve_u1; /* { dg-error {invalid operands to binary !=} } */
  sve_u1 <= sve_u1; /* { dg-error {invalid operands to binary <=} } */
  sve_u1 < sve_u1; /* { dg-error {invalid operands to binary <} } */
  sve_u1 > sve_u1; /* { dg-error {invalid operands to binary >} } */
  sve_u1 >= sve_u1; /* { dg-error {invalid operands to binary >=} } */
  sve_u1 << sve_u1; /* { dg-error {invalid operands to binary <<} } */
  sve_u1 >> sve_u1; /* { dg-error {invalid operands to binary >>} } */
  sve_u1 && sve_u1; /* { dg-error {used vector type where scalar is required} } */
  sve_u1 || sve_u1; /* { dg-error {used vector type where scalar is required} } */

  sve_u1 + gnu_u1; /* { dg-error {invalid operands to binary \+} } */
  sve_u1 - gnu_u1; /* { dg-error {invalid operands to binary -} } */
  sve_u1 * gnu_u1; /* { dg-error {invalid operands to binary \*} } */
  sve_u1 / gnu_u1; /* { dg-error {invalid operands to binary /} } */
  sve_u1 % gnu_u1; /* { dg-error {invalid operands to binary %} } */
  sve_u1 & gnu_u1; /* { dg-error {invalid operands to binary \&} } */
  sve_u1 | gnu_u1; /* { dg-error {invalid operands to binary \|} } */
  sve_u1 ^ gnu_u1; /* { dg-error {invalid operands to binary \^} } */
  sve_u1 == gnu_u1; /* { dg-error {invalid operands to binary ==} } */
  sve_u1 != gnu_u1; /* { dg-error {invalid operands to binary !=} } */
  sve_u1 <= gnu_u1; /* { dg-error {invalid operands to binary <=} } */
  sve_u1 < gnu_u1; /* { dg-error {invalid operands to binary <} } */
  sve_u1 > gnu_u1; /* { dg-error {invalid operands to binary >} } */
  sve_u1 >= gnu_u1; /* { dg-error {invalid operands to binary >=} } */
  sve_u1 << gnu_u1; /* { dg-error {invalid operands to binary <<} } */
  sve_u1 >> gnu_u1; /* { dg-error {invalid operands to binary >>} } */
  sve_u1 && gnu_u1; /* { dg-error {used vector type where scalar is required} } */
  sve_u1 || gnu_u1; /* { dg-error {used vector type where scalar is required} } */

  gnu_u1 + sve_u1; /* { dg-error {invalid operands to binary \+} } */
  gnu_u1 - sve_u1; /* { dg-error {invalid operands to binary -} } */
  gnu_u1 * sve_u1; /* { dg-error {invalid operands to binary \*} } */
  gnu_u1 / sve_u1; /* { dg-error {invalid operands to binary /} } */
  gnu_u1 % sve_u1; /* { dg-error {invalid operands to binary %} } */
  gnu_u1 & sve_u1; /* { dg-error {invalid operands to binary \&} } */
  gnu_u1 | sve_u1; /* { dg-error {invalid operands to binary \|} } */
  gnu_u1 ^ sve_u1; /* { dg-error {invalid operands to binary \^} } */
  gnu_u1 == sve_u1; /* { dg-error {invalid operands to binary ==} } */
  gnu_u1 != sve_u1; /* { dg-error {invalid operands to binary !=} } */
  gnu_u1 <= sve_u1; /* { dg-error {invalid operands to binary <=} } */
  gnu_u1 < sve_u1; /* { dg-error {invalid operands to binary <} } */
  gnu_u1 > sve_u1; /* { dg-error {invalid operands to binary >} } */
  gnu_u1 >= sve_u1; /* { dg-error {invalid operands to binary >=} } */
  gnu_u1 << sve_u1; /* { dg-error {invalid operands to binary <<} } */
  gnu_u1 >> sve_u1; /* { dg-error {invalid operands to binary >>} } */
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

  sve_u1 + 2; /* { dg-error {invalid operands to binary \+} } */
  sve_u1 - 2; /* { dg-error {invalid operands to binary -} } */
  sve_u1 * 2; /* { dg-error {invalid operands to binary \*} } */
  sve_u1 / 2; /* { dg-error {invalid operands to binary /} } */
  sve_u1 % 2; /* { dg-error {invalid operands to binary %} } */
  sve_u1 & 2; /* { dg-error {invalid operands to binary \&} } */
  sve_u1 | 2; /* { dg-error {invalid operands to binary \|} } */
  sve_u1 ^ 2; /* { dg-error {invalid operands to binary \^} } */
  sve_u1 == 2; /* { dg-error {invalid operands to binary ==} } */
  sve_u1 != 2; /* { dg-error {invalid operands to binary !=} } */
  sve_u1 <= 2; /* { dg-error {invalid operands to binary <=} } */
  sve_u1 < 2; /* { dg-error {invalid operands to binary <} } */
  sve_u1 > 2; /* { dg-error {invalid operands to binary >} } */
  sve_u1 >= 2; /* { dg-error {invalid operands to binary >=} } */
  sve_u1 << 2; /* { dg-error {invalid operands to binary <<} } */
  sve_u1 >> 2; /* { dg-error {invalid operands to binary >>} } */
  sve_u1 && 2; /* { dg-error {used vector type where scalar is required} } */
  sve_u1 || 2; /* { dg-error {used vector type where scalar is required} } */

  sve_u1 + uc; /* { dg-error {invalid operands to binary \+} } */
  sve_u1 - uc; /* { dg-error {invalid operands to binary -} } */
  sve_u1 * uc; /* { dg-error {invalid operands to binary \*} } */
  sve_u1 / uc; /* { dg-error {invalid operands to binary /} } */
  sve_u1 % uc; /* { dg-error {invalid operands to binary %} } */
  sve_u1 & uc; /* { dg-error {invalid operands to binary \&} } */
  sve_u1 | uc; /* { dg-error {invalid operands to binary \|} } */
  sve_u1 ^ uc; /* { dg-error {invalid operands to binary \^} } */
  sve_u1 == uc; /* { dg-error {invalid operands to binary ==} } */
  sve_u1 != uc; /* { dg-error {invalid operands to binary !=} } */
  sve_u1 <= uc; /* { dg-error {invalid operands to binary <=} } */
  sve_u1 < uc; /* { dg-error {invalid operands to binary <} } */
  sve_u1 > uc; /* { dg-error {invalid operands to binary >} } */
  sve_u1 >= uc; /* { dg-error {invalid operands to binary >=} } */
  sve_u1 << uc; /* { dg-error {invalid operands to binary <<} } */
  sve_u1 >> uc; /* { dg-error {invalid operands to binary >>} } */
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

  3 + sve_u1; /* { dg-error {invalid operands to binary \+} } */
  3 - sve_u1; /* { dg-error {invalid operands to binary -} } */
  3 * sve_u1; /* { dg-error {invalid operands to binary \*} } */
  3 / sve_u1; /* { dg-error {invalid operands to binary /} } */
  3 % sve_u1; /* { dg-error {invalid operands to binary %} } */
  3 & sve_u1; /* { dg-error {invalid operands to binary \&} } */
  3 | sve_u1; /* { dg-error {invalid operands to binary \|} } */
  3 ^ sve_u1; /* { dg-error {invalid operands to binary \^} } */
  3 == sve_u1; /* { dg-error {invalid operands to binary ==} } */
  3 != sve_u1; /* { dg-error {invalid operands to binary !=} } */
  3 <= sve_u1; /* { dg-error {invalid operands to binary <=} } */
  3 < sve_u1; /* { dg-error {invalid operands to binary <} } */
  3 > sve_u1; /* { dg-error {invalid operands to binary >} } */
  3 >= sve_u1; /* { dg-error {invalid operands to binary >=} } */
  3 << sve_u1; /* { dg-error {invalid operands to binary <<} } */
  3 >> sve_u1; /* { dg-error {invalid operands to binary >>} } */
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

  sve_u1 + sve_s1; /* { dg-error {invalid operands to binary \+} } */
  sve_u1 - sve_s1; /* { dg-error {invalid operands to binary -} } */
  sve_u1 * sve_s1; /* { dg-error {invalid operands to binary \*} } */
  sve_u1 / sve_s1; /* { dg-error {invalid operands to binary /} } */
  sve_u1 % sve_s1; /* { dg-error {invalid operands to binary %} } */
  sve_u1 & sve_s1; /* { dg-error {invalid operands to binary \&} } */
  sve_u1 | sve_s1; /* { dg-error {invalid operands to binary \|} } */
  sve_u1 ^ sve_s1; /* { dg-error {invalid operands to binary \^} } */
  sve_u1 == sve_s1; /* { dg-error {invalid operands to binary ==} } */
  sve_u1 != sve_s1; /* { dg-error {invalid operands to binary !=} } */
  sve_u1 <= sve_s1; /* { dg-error {invalid operands to binary <=} } */
  sve_u1 < sve_s1; /* { dg-error {invalid operands to binary <} } */
  sve_u1 > sve_s1; /* { dg-error {invalid operands to binary >} } */
  sve_u1 >= sve_s1; /* { dg-error {invalid operands to binary >=} } */
  sve_u1 << sve_s1; /* { dg-error {invalid operands to binary <<} } */
  sve_u1 >> sve_s1; /* { dg-error {invalid operands to binary >>} } */
  sve_u1 && sve_s1; /* { dg-error {used vector type where scalar is required} } */
  sve_u1 || sve_s1; /* { dg-error {used vector type where scalar is required} } */

  sve_u1 + gnu_s1; /* { dg-error {invalid operands to binary \+} } */
  sve_u1 - gnu_s1; /* { dg-error {invalid operands to binary -} } */
  sve_u1 * gnu_s1; /* { dg-error {invalid operands to binary \*} } */
  sve_u1 / gnu_s1; /* { dg-error {invalid operands to binary /} } */
  sve_u1 % gnu_s1; /* { dg-error {invalid operands to binary %} } */
  sve_u1 & gnu_s1; /* { dg-error {invalid operands to binary \&} } */
  sve_u1 | gnu_s1; /* { dg-error {invalid operands to binary \|} } */
  sve_u1 ^ gnu_s1; /* { dg-error {invalid operands to binary \^} } */
  sve_u1 == gnu_s1; /* { dg-error {invalid operands to binary ==} } */
  sve_u1 != gnu_s1; /* { dg-error {invalid operands to binary !=} } */
  sve_u1 <= gnu_s1; /* { dg-error {invalid operands to binary <=} } */
  sve_u1 < gnu_s1; /* { dg-error {invalid operands to binary <} } */
  sve_u1 > gnu_s1; /* { dg-error {invalid operands to binary >} } */
  sve_u1 >= gnu_s1; /* { dg-error {invalid operands to binary >=} } */
  sve_u1 << gnu_s1; /* { dg-error {invalid operands to binary <<} } */
  sve_u1 >> gnu_s1; /* { dg-error {invalid operands to binary >>} } */
  sve_u1 && gnu_s1; /* { dg-error {used vector type where scalar is required} } */
  sve_u1 || gnu_s1; /* { dg-error {used vector type where scalar is required} } */

  gnu_u1 + sve_s1; /* { dg-error {invalid operands to binary \+} } */
  gnu_u1 - sve_s1; /* { dg-error {invalid operands to binary -} } */
  gnu_u1 * sve_s1; /* { dg-error {invalid operands to binary \*} } */
  gnu_u1 / sve_s1; /* { dg-error {invalid operands to binary /} } */
  gnu_u1 % sve_s1; /* { dg-error {invalid operands to binary %} } */
  gnu_u1 & sve_s1; /* { dg-error {invalid operands to binary \&} } */
  gnu_u1 | sve_s1; /* { dg-error {invalid operands to binary \|} } */
  gnu_u1 ^ sve_s1; /* { dg-error {invalid operands to binary \^} } */
  gnu_u1 == sve_s1; /* { dg-error {invalid operands to binary ==} } */
  gnu_u1 != sve_s1; /* { dg-error {invalid operands to binary !=} } */
  gnu_u1 <= sve_s1; /* { dg-error {invalid operands to binary <=} } */
  gnu_u1 < sve_s1; /* { dg-error {invalid operands to binary <} } */
  gnu_u1 > sve_s1; /* { dg-error {invalid operands to binary >} } */
  gnu_u1 >= sve_s1; /* { dg-error {invalid operands to binary >=} } */
  gnu_u1 << sve_s1; /* { dg-error {invalid operands to binary <<} } */
  gnu_u1 >> sve_s1; /* { dg-error {invalid operands to binary >>} } */
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

  /* Vector built-ins.  */

  __builtin_shuffle (sve_u1, sve_u1, sve_u1); /* { dg-error {'__builtin_shuffle' last argument must be an integer vector} } */
  __builtin_shuffle (sve_u1, gnu_u1, gnu_u1); /* { dg-error {'__builtin_shuffle' arguments must be vectors} } */
  __builtin_shuffle (gnu_u1, sve_u1, gnu_u1); /* { dg-error {'__builtin_shuffle' arguments must be vectors} } */
  __builtin_shuffle (gnu_u1, gnu_u1, sve_u1); /* { dg-error {'__builtin_shuffle' last argument must be an integer vector} } */
  __builtin_shuffle (gnu_u1, gnu_u1, gnu_u1);

  __builtin_convertvector (sve_u1, svuint8_t); /* { dg-error {'__builtin_convertvector' first argument must be an integer or floating vector} } */
  __builtin_convertvector (gnu_u1, svuint8_t); /* { dg-error {'__builtin_convertvector' second argument must be an integer or floating vector type} } */
  __builtin_convertvector (sve_u1, gnu_uint8_t); /* { dg-error {'__builtin_convertvector' first argument must be an integer or floating vector} } */
  __builtin_convertvector (gnu_u1, gnu_uint8_t);
}
