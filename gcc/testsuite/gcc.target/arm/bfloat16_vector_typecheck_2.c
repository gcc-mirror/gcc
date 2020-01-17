/* { dg-do assemble { target { arm*-*-* } } } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-require-effective-target arm_v8_2a_fp16_neon_ok } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-additional-options "-march=armv8.6-a+bf16+fp16 -Wno-pedantic -O3 --save-temps" }  */

#include <arm_neon.h>

bfloat16_t glob_bfloat;
bfloat16x8_t glob_bfloat_vec;

float32x4_t is_a_float_vec;

float16x8_t *float_ptr;
float16x8_t is_a_float16_vec;

int32x4_t is_an_int_vec;
int64x2_t is_a_long_int_pair;
int16x8_t is_a_short_vec;

int is_an_int;
short is_a_short_int;
float is_a_float;
float is_a_float16;
double is_a_double;

bfloat16x8_t foo3 (void) { return (bfloat16x8_t) 0x12345678123456781234567812345678; }
 /* { dg-error {integer constant is too large for its type} "" {target *-*-*} 27 } */
 /* { dg-error {cannot convert a value of type 'long long int' to vector type '__simd128_bfloat16_t' which has different size} "" {target *-*-*} 27 } */

bfloat16x8_t footest (bfloat16x8_t vector0)
{
  /* Initialisation  */

  bfloat16x8_t vector1_1;
  bfloat16x8_t vector1_2 = glob_bfloat_vec;
  bfloat16x8_t vector1_3 = is_a_float_vec; /* { dg-error {incompatible types when initializing type 'bfloat16x8_t' using type 'float32x4_t'} } */
  bfloat16x8_t vector1_4 = is_an_int_vec;  /* { dg-error {incompatible types when initializing type 'bfloat16x8_t' using type 'int32x4_t'} } */
  bfloat16x8_t vector1_5 = is_a_float16_vec; /* { dg-error {incompatible types when initializing type 'bfloat16x8_t' using type 'float16x8_t'} } */
  bfloat16x8_t vector1_7 = is_a_long_int_pair; /* { dg-error {incompatible types when initializing type 'bfloat16x8_t' using type 'int64x2_t'} } */
  bfloat16x8_t vector1_8 = is_a_short_vec; /* { dg-error {incompatible types when initializing type 'bfloat16x8_t' using type 'int16x8_t'} } */

  int32x4_t initi_1_1 = glob_bfloat_vec;   /* { dg-error {incompatible types when initializing type 'int32x4_t' using type 'bfloat16x8_t'} } */
  float32x4_t initi_1_2 = glob_bfloat_vec; /* { dg-error {incompatible types when initializing type 'float32x4_t' using type 'bfloat16x8_t'} } */
  float16x8_t initi_1_3 = glob_bfloat_vec; /* { dg-error {incompatible types when initializing type 'float16x8_t' using type 'bfloat16x8_t'} } */
  int64x2_t initi_1_5 = glob_bfloat_vec;  /* { dg-error {incompatible types when initializing type 'int64x2_t' using type 'bfloat16x8_t'} } */
  int16x8_t initi_1_6 = glob_bfloat_vec;  /* { dg-error {incompatible types when initializing type 'int16x8_t' using type 'bfloat16x8_t'} } */

  bfloat16x8_t vector2_1 = {};
  bfloat16x8_t vector2_2 = { glob_bfloat };
  bfloat16x8_t vector2_3 = { glob_bfloat, glob_bfloat, glob_bfloat, glob_bfloat };
  bfloat16x8_t vector2_4 = { 0 }; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  bfloat16x8_t vector2_5 = { 0.1 }; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  bfloat16x8_t vector2_6 = { is_a_float16 }; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  bfloat16x8_t vector2_7 = { is_a_float }; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  bfloat16x8_t vector2_8 = { is_an_int }; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  bfloat16x8_t vector2_9 = { is_a_short_int }; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  bfloat16x8_t vector2_10 = { 0.0, 0, is_a_short_int, is_a_float }; /* { dg-error "invalid conversion to type 'bfloat16_t'" } */

  int32x4_t initi_2_1 = { glob_bfloat };   /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  float32x4_t initi_2_2 = { glob_bfloat }; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  float16x8_t initi_2_3 = { glob_bfloat }; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  int64x2_t initi_2_5 = { glob_bfloat };   /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  int16x8_t initi_2_6 = { glob_bfloat };   /* { dg-error {invalid conversion from type 'bfloat16_t'} } */

  /* Assignments to/from vectors.  */

  glob_bfloat_vec = glob_bfloat_vec;
  glob_bfloat_vec = 0;   /* { dg-error {incompatible types when assigning to type 'bfloat16x8_t' from type 'int'} } */
  glob_bfloat_vec = 0.1; /* { dg-error {incompatible types when assigning to type 'bfloat16x8_t' from type 'double'} } */
  glob_bfloat_vec = is_a_float_vec; /* { dg-error {incompatible types when assigning to type 'bfloat16x8_t' from type 'float32x4_t'} } */
  glob_bfloat_vec = is_an_int_vec; /* { dg-error {incompatible types when assigning to type 'bfloat16x8_t' from type 'int32x4_t'} } */
  glob_bfloat_vec = is_a_float16_vec; /* { dg-error {incompatible types when assigning to type 'bfloat16x8_t' from type 'float16x8_t'} } */
  glob_bfloat_vec = is_a_long_int_pair; /* { dg-error {incompatible types when assigning to type 'bfloat16x8_t' from type 'int64x2_t'} } */
  glob_bfloat_vec = is_a_short_vec; /* { dg-error {incompatible types when assigning to type 'bfloat16x8_t' from type 'int16x8_t'} } */

  is_an_int_vec = glob_bfloat_vec; /* { dg-error {incompatible types when assigning to type 'int32x4_t' from type 'bfloat16x8_t'} } */
  is_a_float_vec = glob_bfloat_vec; /* { dg-error {incompatible types when assigning to type 'float32x4_t' from type 'bfloat16x8_t'} } */
  is_a_float16_vec = glob_bfloat_vec; /* { dg-error {incompatible types when assigning to type 'float16x8_t' from type 'bfloat16x8_t'} } */
  is_a_long_int_pair = glob_bfloat_vec; /* { dg-error {incompatible types when assigning to type 'int64x2_t' from type 'bfloat16x8_t'} } */
  is_a_short_vec = glob_bfloat_vec;/* { dg-error {incompatible types when assigning to type 'int16x8_t' from type 'bfloat16x8_t'} } */

  /* Assignments to/from elements.  */

  vector2_3[0] = glob_bfloat;
  vector2_3[0] = is_an_int; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  vector2_3[0] = is_a_short_int; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  vector2_3[0] = is_a_float; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  vector2_3[0] = is_a_float16; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  vector2_3[0] = 0; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  vector2_3[0] = 0.1; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */

  glob_bfloat = vector2_3[0];
  is_an_int = vector2_3[0]; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  is_a_short_int = vector2_3[0]; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  is_a_float = vector2_3[0]; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  is_a_float16 = vector2_3[0]; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */

  /* Compound literals.  */

  (bfloat16x8_t) {};

  (bfloat16x8_t) { 0 }; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  (bfloat16x8_t) { 0.1 }; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  (bfloat16x8_t) { is_a_float_vec }; /* { dg-error {incompatible types when initializing type '__bf16' using type 'float32x4_t'} } */
  (bfloat16x8_t) { is_an_int_vec }; /* { dg-error {incompatible types when initializing type '__bf16' using type 'int32x4_t'} } */
  (bfloat16x8_t) { is_a_long_int_pair }; /* { dg-error {incompatible types when initializing type '__bf16' using type 'int64x2_t'} } */
  (bfloat16x8_t) { is_a_float16_vec }; /* { dg-error {incompatible types when initializing type '__bf16' using type 'float16x8_t'} } */
  (bfloat16x8_t) { is_a_short_vec }; /* { dg-error {incompatible types when initializing type '__bf16' using type 'int16x8_t'} } */

  (bfloat16x8_t) { glob_bfloat_vec }; /* { dg-error {incompatible types when initializing type '__bf16' using type 'bfloat16x8_t'} } */
  (int32x4_t) { glob_bfloat_vec }; /* { dg-error {incompatible types when initializing type 'int' using type 'bfloat16x8_t'} } */
  (float32x4_t) { glob_bfloat_vec }; /* { dg-error {incompatible types when initializing type 'float' using type 'bfloat16x8_t'} } */
  (int64x2_t) { glob_bfloat_vec }; /* { dg-error {incompatible types when initializing type 'long long int' using type 'bfloat16x8_t'} } */
  (float16x8_t) { glob_bfloat_vec }; /* { dg-error {incompatible types when initializing type '__fp16' using type 'bfloat16x8_t'} } */
  (int16x8_t) { glob_bfloat_vec }; /* { dg-error {incompatible types when initializing type 'short int' using type 'bfloat16x8_t'} } */

  /* Casting.  */

  (void) glob_bfloat_vec;
  (bfloat16x8_t) glob_bfloat_vec;

  (bfloat16_t) glob_bfloat_vec; /* { dg-error {aggregate value used where a floating-point was expected} } */
  (short) glob_bfloat_vec; /* { dg-error {cannot convert a vector of type 'bfloat16x8_t' to type 'short int' which has different size} } */
  (int) glob_bfloat_vec; /* { dg-error {cannot convert a vector of type 'bfloat16x8_t' to type 'int' which has different size} } */
  (float16_t) glob_bfloat_vec; /* { dg-error {aggregate value used where a floating-point was expected} } */
  (float) glob_bfloat_vec; /* { dg-error {aggregate value used where a floating-point was expected} } */
  (double) glob_bfloat_vec; /* { dg-error {aggregate value used where a floating-point was expected} } */

  (int32x4_t) glob_bfloat_vec;
  (float32x4_t) glob_bfloat_vec;
  (float16x8_t) glob_bfloat_vec;
  (int64x2_t) glob_bfloat_vec;
  (int16x8_t) glob_bfloat_vec;

  (bfloat16x8_t) is_an_int_vec;
  (bfloat16x8_t) is_a_float_vec;
  (bfloat16x8_t) is_a_float16_vec;
  (bfloat16x8_t) is_a_long_int_pair;
  (bfloat16x8_t) is_a_short_vec;

  /* Arrays and Structs.  */

  typedef bfloat16x8_t array_type[2];
  extern bfloat16x8_t extern_array[];

  bfloat16x8_t array[2];
  bfloat16x8_t zero_length_array[0];
  bfloat16x8_t empty_init_array[] = {};
  typedef bfloat16x8_t some_other_type[is_an_int];

  struct struct1 {
    bfloat16x8_t a;
  };

  union union1 {
    bfloat16x8_t a;
  };

  /* Addressing and dereferencing.  */

  bfloat16x8_t *bfloat_ptr = &vector0;
  vector0 = *bfloat_ptr;

  /* Pointer assignment.  */

  bfloat16x8_t *bfloat_ptr2 = bfloat_ptr;
  bfloat16x8_t *bfloat_ptr3 = array;

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
  vector0 > glob_bfloat_vec; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  glob_bfloat_vec == vector0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  vector0 > is_a_float_vec; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  is_a_float_vec == vector0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  vector0 > 0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  0 == vector0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  vector0 > 0.1; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  0.1 == vector0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  vector0 > is_an_int_vec; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  is_an_int_vec == vector0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */

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
  0 ? bfloat_ptr : float_ptr; /* { dg-error {pointer type mismatch in conditional expression} } */
  0 ? float_ptr : bfloat_ptr; /* { dg-error {pointer type mismatch in conditional expression} } */

  vector0 ? vector0 : vector0; /* { dg-error {used vector type where scalar is required} } */
  vector0 ? is_a_float16_vec : vector0; /* { dg-error {used vector type where scalar is required} } */
  vector0 ? vector0 : is_a_float16_vec; /* { dg-error {used vector type where scalar is required} } */
  vector0 ? is_a_float16_vec : is_a_float16_vec; /* { dg-error {used vector type where scalar is required} } */

  /* Unary operators.  */

  +vector0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  -vector0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  ~vector0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  !vector0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  *vector0; /* { dg-error {invalid type argument of unary '\*'} } */
  __real vector0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  __imag vector0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  ++vector0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  --vector0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  vector0++; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  vector0--; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */

  /* Binary arithmetic operations.  */

  vector0 = glob_bfloat_vec + *bfloat_ptr; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  vector0 = glob_bfloat_vec + 0.1; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  vector0 = glob_bfloat_vec + 0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  vector0 = glob_bfloat_vec + is_a_float_vec; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */

  return vector0;
}

