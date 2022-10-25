/* { dg-do compile } */
/* { dg-options "-mavx512fp16 -O2" } */

#include <immintrin.h>

typedef __bf16 __v16bf __attribute__ ((__vector_size__ (32)));
typedef __bf16 __m256bf16 __attribute__ ((__vector_size__ (32), __may_alias__));

__bf16 glob_bfloat;
__m256bf16 glob_bfloat_vec;

__m256 is_a_float_vec;

__m256h *float_ptr;
__m256h is_a_float16_vec;

__v8si is_an_int_vec;
__m256i is_a_long_int_pair;
__v16hi is_a_short_vec;

int is_an_int;
short is_a_short_int;
float is_a_float;
float is_a_float16;
double is_a_double;

__m256bf16 footest (__m256bf16 vector0)
{
  /* Initialisation  */

  __m256bf16 vector1_1;
  __m256bf16 vector1_2 = glob_bfloat_vec;
  __m256bf16 vector1_3 = is_a_float_vec; /* { dg-error {incompatible types when initializing type '__m256bf16' {aka '__vector\(16\) __bf16'} using type '__m256'} } */
  __m256bf16 vector1_4 = is_an_int_vec;  /* { dg-error {incompatible types when initializing type '__m256bf16' {aka '__vector\(16\) __bf16'} using type '__v8si'} } */
  __m256bf16 vector1_5 = is_a_float16_vec; /* { dg-error {incompatible types when initializing type '__m256bf16' {aka '__vector\(16\) __bf16'} using type '__m256h'} } */
  __m256bf16 vector1_7 = is_a_long_int_pair; /* { dg-error {incompatible types when initializing type '__m256bf16' {aka '__vector\(16\) __bf16'} using type '__m256i'} } */
  __m256bf16 vector1_8 = is_a_short_vec; /* { dg-error {incompatible types when initializing type '__m256bf16' {aka '__vector\(16\) __bf16'} using type '__v16hi'} } */

  __v8si initi_1_1 = glob_bfloat_vec;   /* { dg-error {incompatible types when initializing type '__v8si' using type '__m256bf16' {aka '__vector\(16\) __bf16'}} } */
  __m256 initi_1_2 = glob_bfloat_vec; /* { dg-error {incompatible types when initializing type '__m256' using type '__m256bf16' {aka '__vector\(16\) __bf16'}} } */
  __m256h initi_1_3 = glob_bfloat_vec; /* { dg-error {incompatible types when initializing type '__m256h' using type '__m256bf16' {aka '__vector\(16\) __bf16'}} } */
  __m256i initi_1_5 = glob_bfloat_vec;  /* { dg-error {incompatible types when initializing type '__m256i' using type '__m256bf16' {aka '__vector\(16\) __bf16'}} } */
  __v16hi initi_1_6 = glob_bfloat_vec;  /* { dg-error {incompatible types when initializing type '__v16hi' using type '__m256bf16' {aka '__vector\(16\) __bf16'}} } */

  __m256bf16 vector2_1 = {};
  __m256bf16 vector2_2 = { glob_bfloat };
  __m256bf16 vector2_3 = { glob_bfloat, glob_bfloat, glob_bfloat, glob_bfloat };
  __m256bf16 vector2_4 = { 0 };
  __m256bf16 vector2_5 = { 0.1 };
  __m256bf16 vector2_6 = { is_a_float16 };
  __m256bf16 vector2_7 = { is_a_float };
  __m256bf16 vector2_8 = { is_an_int };
  __m256bf16 vector2_9 = { is_a_short_int };
  __m256bf16 vector2_10 = { 0.0, 0, is_a_short_int, is_a_float };

  __v8si initi_2_1 = { glob_bfloat };
  __m256 initi_2_2 = { glob_bfloat };
  __m256h initi_2_3 = { glob_bfloat };
  __m256i initi_2_5 = { glob_bfloat };
  __v16hi initi_2_6 = { glob_bfloat };

  /* Assignments to/from vectors.  */

  glob_bfloat_vec = glob_bfloat_vec;
  glob_bfloat_vec = 0;   /* { dg-error {incompatible types when assigning to type '__m256bf16' {aka '__vector\(16\) __bf16'} from type 'int'} } */
  glob_bfloat_vec = 0.1; /* { dg-error {incompatible types when assigning to type '__m256bf16' {aka '__vector\(16\) __bf16'} from type 'double'} } */
  glob_bfloat_vec = is_a_float_vec; /* { dg-error {incompatible types when assigning to type '__m256bf16' {aka '__vector\(16\) __bf16'} from type '__m256'} } */
  glob_bfloat_vec = is_an_int_vec; /* { dg-error {incompatible types when assigning to type '__m256bf16' {aka '__vector\(16\) __bf16'} from type '__v8si'} } */
  glob_bfloat_vec = is_a_float16_vec; /* { dg-error {incompatible types when assigning to type '__m256bf16' {aka '__vector\(16\) __bf16'} from type '__m256h'} } */
  glob_bfloat_vec = is_a_long_int_pair; /* { dg-error {incompatible types when assigning to type '__m256bf16' {aka '__vector\(16\) __bf16'} from type '__m256i'} } */
  glob_bfloat_vec = is_a_short_vec; /* { dg-error {incompatible types when assigning to type '__m256bf16' {aka '__vector\(16\) __bf16'} from type '__v16hi'} } */

  is_an_int_vec = glob_bfloat_vec; /* { dg-error {incompatible types when assigning to type '__v8si' from type '__m256bf16' {aka '__vector\(16\) __bf16'}} } */
  is_a_float_vec = glob_bfloat_vec; /* { dg-error {incompatible types when assigning to type '__m256' from type '__m256bf16' {aka '__vector\(16\) __bf16'}} } */
  is_a_float16_vec = glob_bfloat_vec; /* { dg-error {incompatible types when assigning to type '__m256h' from type '__m256bf16' {aka '__vector\(16\) __bf16'}} } */
  is_a_long_int_pair = glob_bfloat_vec; /* { dg-error {incompatible types when assigning to type '__m256i' from type '__m256bf16' {aka '__vector\(16\) __bf16'}} } */
  is_a_short_vec = glob_bfloat_vec;/* { dg-error {incompatible types when assigning to type '__v16hi' from type '__m256bf16' {aka '__vector\(16\) __bf16'}} } */

  /* Assignments to/from elements.  */

  vector2_3[0] = glob_bfloat;
  vector2_3[0] = is_an_int;
  vector2_3[0] = is_a_short_int;
  vector2_3[0] = is_a_float;
  vector2_3[0] = is_a_float16;
  vector2_3[0] = 0;
  vector2_3[0] = 0.1;

  glob_bfloat = vector2_3[0];
  is_an_int = vector2_3[0];
  is_a_short_int = vector2_3[0];
  is_a_float = vector2_3[0];
  is_a_float16 = vector2_3[0];

  /* Compound literals.  */

  (__m256bf16) {};

  (__m256bf16) { 0 };
  (__m256bf16) { 0.1 };
  (__m256bf16) { is_a_float_vec }; /* { dg-error {incompatible types when initializing type '__bf16' using type '__m256'} } */
  (__m256bf16) { is_an_int_vec }; /* { dg-error {incompatible types when initializing type '__bf16' using type '__v8si'} } */
  (__m256bf16) { is_a_long_int_pair }; /* { dg-error {incompatible types when initializing type '__bf16' using type '__m256i'} } */
  (__m256bf16) { is_a_float16_vec }; /* { dg-error {incompatible types when initializing type '__bf16' using type '__m256h'} } */
  (__m256bf16) { is_a_short_vec }; /* { dg-error {incompatible types when initializing type '__bf16' using type '__v16hi'} } */

  (__m256bf16) { glob_bfloat_vec }; /* { dg-error {incompatible types when initializing type '__bf16' using type '__m256bf16' {aka '__vector\(16\) __bf16'}} } */
  (__v8si) { glob_bfloat_vec }; /* { dg-error {incompatible types when initializing type 'int' using type '__m256bf16' {aka '__vector\(16\) __bf16'}} } */
  (__m256) { glob_bfloat_vec }; /* { dg-error {incompatible types when initializing type 'float' using type '__m256bf16' {aka '__vector\(16\) __bf16'}} } */
  (__m256i) { glob_bfloat_vec }; /* { dg-error {incompatible types when initializing type 'long long int' using type '__m256bf16' {aka '__vector\(16\) __bf16'}} } */
  (__m256h) { glob_bfloat_vec }; /* { dg-error {incompatible types when initializing type '_Float16' using type '__m256bf16' {aka '__vector\(16\) __bf16'}} } */
  (__v16hi) { glob_bfloat_vec }; /* { dg-error {incompatible types when initializing type 'short int' using type '__m256bf16' {aka '__vector\(16\) __bf16'}} } */

  /* Casting.  */

  (void) glob_bfloat_vec;
  (__m256bf16) glob_bfloat_vec;

  (__bf16) glob_bfloat_vec; /* { dg-error {aggregate value used where a floating-point was expected} } */
  (short) glob_bfloat_vec; /* { dg-error {cannot convert a vector of type '__m256bf16' {aka '__vector\(16\) __bf16'} to type 'short int' which has different size} } */
  (int) glob_bfloat_vec; /* { dg-error {cannot convert a vector of type '__m256bf16' {aka '__vector\(16\) __bf16'} to type 'int' which has different size} } */
  (_Float16) glob_bfloat_vec; /* { dg-error {aggregate value used where a floating-point was expected} } */
  (float) glob_bfloat_vec; /* { dg-error {aggregate value used where a floating-point was expected} } */
  (double) glob_bfloat_vec; /* { dg-error {aggregate value used where a floating-point was expected} } */

  (__v8si) glob_bfloat_vec;
  (__m256) glob_bfloat_vec;
  (__m256h) glob_bfloat_vec;
  (__m256i) glob_bfloat_vec;
  (__v16hi) glob_bfloat_vec;

  (__m256bf16) is_an_int_vec;
  (__m256bf16) is_a_float_vec;
  (__m256bf16) is_a_float16_vec;
  (__m256bf16) is_a_long_int_pair;
  (__m256bf16) is_a_short_vec;

  /* Arrays and Structs.  */

  typedef __m256bf16 array_type[2];
  extern __m256bf16 extern_array[];

  __m256bf16 array[2];
  __m256bf16 zero_length_array[0];
  __m256bf16 empty_init_array[] = {};
  typedef __m256bf16 some_other_type[is_an_int];

  struct struct1 {
    __m256bf16 a;
  };

  union union1 {
    __m256bf16 a;
  };

  /* Addressing and dereferencing.  */

  __m256bf16 *bfloat_ptr = &vector0;
  vector0 = *bfloat_ptr;

  /* Pointer assignment.  */

  __m256bf16 *bfloat_ptr2 = bfloat_ptr;
  __m256bf16 *bfloat_ptr3 = array;

  /* Pointer arithmetic.  */

  ++bfloat_ptr;
  --bfloat_ptr;
  bfloat_ptr++;
  bfloat_ptr--;
  bfloat_ptr += 1;
  bfloat_ptr -= 1;
  bfloat_ptr - bfloat_ptr2;
  bfloat_ptr = &bfloat_ptr3[0];
  bfloat_ptr = &bfloat_ptr3[1];

  /* Simple comparison.  */
  vector0 > glob_bfloat_vec;
  glob_bfloat_vec == vector0;
  vector0 > is_a_float_vec; /* { dg-error {comparing vectors with different element types} } */
  is_a_float_vec == vector0; /* { dg-error {comparing vectors with different element types} } */
  vector0 > 0;
  0 == vector0;
  vector0 > 0.1; /* { dg-error {conversion of scalar 'double' to vector '__m256bf16'} } */
  0.1 == vector0; /* { dg-error {conversion of scalar 'double' to vector '__m256bf16'} } */
  vector0 > is_an_int_vec; /* { dg-error {comparing vectors with different element types} } */
  is_an_int_vec == vector0; /* { dg-error {comparing vectors with different element types} } */

  /* Pointer comparison.  */

  bfloat_ptr == &vector0;
  bfloat_ptr != &vector0;
  bfloat_ptr < &vector0;
  bfloat_ptr <= &vector0;
  bfloat_ptr > &vector0;
  bfloat_ptr >= &vector0;
  bfloat_ptr == bfloat_ptr2;
  bfloat_ptr != bfloat_ptr2;
  bfloat_ptr < bfloat_ptr2;
  bfloat_ptr <= bfloat_ptr2;
  bfloat_ptr > bfloat_ptr2;
  bfloat_ptr >= bfloat_ptr2;

  /* Conditional expressions.  */

  0 ? vector0 : vector0;
  0 ? vector0 : is_a_float_vec; /* { dg-error {type mismatch in conditional expression} } */
  0 ? is_a_float_vec : vector0; /* { dg-error {type mismatch in conditional expression} } */
  0 ? vector0 : is_a_float16_vec; /* { dg-error {type mismatch in conditional expression} } */
  0 ? is_a_float16_vec : vector0; /* { dg-error {type mismatch in conditional expression} } */
  0 ? vector0 : 0; /* { dg-error {type mismatch in conditional expression} } */
  0 ? 0 : vector0; /* { dg-error {type mismatch in conditional expression} } */
  0 ? 0.1 : vector0; /* { dg-error {type mismatch in conditional expression} } */
  0 ? vector0 : 0.1; /* { dg-error {type mismatch in conditional expression} } */
  0 ? bfloat_ptr : bfloat_ptr2;
  0 ? bfloat_ptr : float_ptr; /* { dg-warning {pointer type mismatch in conditional expression} } */
  0 ? float_ptr : bfloat_ptr; /* { dg-warning {pointer type mismatch in conditional expression} } */

  vector0 ? vector0 : vector0; /* { dg-error {used vector type where scalar is required} } */
  vector0 ? is_a_float16_vec : vector0; /* { dg-error {used vector type where scalar is required} } */
  vector0 ? vector0 : is_a_float16_vec; /* { dg-error {used vector type where scalar is required} } */
  vector0 ? is_a_float16_vec : is_a_float16_vec; /* { dg-error {used vector type where scalar is required} } */

  /* Unary operators.  */

  +vector0;
  -vector0;
  ~vector0; /* { dg-error {wrong type argument to bit-complement} } */
  !vector0; /* { dg-error {wrong type argument to unary exclamation mark} } */
  *vector0; /* { dg-error {invalid type argument of unary '\*'} } */
  __real vector0; /* { dg-error {wrong type argument to __real} } */
  __imag vector0; /* { dg-error {wrong type argument to __imag} } */
  ++vector0;
  --vector0;
  vector0++;
  vector0--;

  /* Binary arithmetic operations.  */

  vector0 = glob_bfloat_vec + *bfloat_ptr;
  vector0 = glob_bfloat_vec + 0.1; /* { dg-error {conversion of scalar 'double' to vector '__m256bf16'} } */
  vector0 = glob_bfloat_vec + 0;
  vector0 = glob_bfloat_vec + is_a_float_vec; /* { dg-error {invalid operands to binary \+} } */

  return vector0;
}

