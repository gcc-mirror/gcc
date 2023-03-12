/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon }  */
/* { dg-additional-options "-O3 --save-temps -Wno-pedantic" }  */
#include <arm_neon.h>

bfloat16_t glob_bfloat;
bfloat16x4_t glob_bfloat_vec;

float32x4_t is_a_float_vec;
float32x2_t is_a_float_pair;

float16x4_t *float_ptr;
float16x4_t is_a_float16_vec;

int32x4_t is_an_int_vec;
int32x2_t is_an_int_pair;
int16x4_t is_a_short_vec;

int is_an_int;
short is_a_short_int;
float is_a_float;
float is_a_float16;
double is_a_double;

/* Create a vector of 2 bfloat16_t.  */
typedef bfloat16_t v2bf __attribute__((vector_size(4)));
v2bf foo1 (void) { return (v2bf) 0x12345678; }
bfloat16x4_t foo2 (void) { return (bfloat16x4_t) 0x1234567812345678; }

bfloat16x4_t footest (bfloat16x4_t vector0)
{
  /* Initialisation  */

  bfloat16x4_t vector1_1;
  bfloat16x4_t vector1_2 = glob_bfloat_vec;
  bfloat16x4_t vector1_3 = is_a_float_vec; /* { dg-error {incompatible types when initializing type 'bfloat16x4_t' using type 'float32x4_t'} } */
  bfloat16x4_t vector1_4 = is_an_int_vec;  /* { dg-error {incompatible types when initializing type 'bfloat16x4_t' using type 'int32x4_t'} } */
  bfloat16x4_t vector1_5 = is_a_float16_vec; /* { dg-error {incompatible types when initializing type 'bfloat16x4_t' using type 'float16x4_t'} } */
  bfloat16x4_t vector1_6 = is_a_float_pair; /* { dg-error {incompatible types when initializing type 'bfloat16x4_t' using type 'float32x2_t'} } */
  bfloat16x4_t vector1_7 = is_an_int_pair; /* { dg-error {incompatible types when initializing type 'bfloat16x4_t' using type 'int32x2_t'} } */
  bfloat16x4_t vector1_8 = is_a_short_vec; /* { dg-error {incompatible types when initializing type 'bfloat16x4_t' using type 'int16x4_t'} } */

  int32x4_t initi_1_1 = glob_bfloat_vec;   /* { dg-error {incompatible types when initializing type 'int32x4_t' using type 'bfloat16x4_t'} } */
  float32x4_t initi_1_2 = glob_bfloat_vec; /* { dg-error {incompatible types when initializing type 'float32x4_t' using type 'bfloat16x4_t'} } */
  float16x4_t initi_1_3 = glob_bfloat_vec; /* { dg-error {incompatible types when initializing type 'float16x4_t' using type 'bfloat16x4_t'} } */
  float32x2_t initi_1_4 = glob_bfloat_vec; /* { dg-error {incompatible types when initializing type 'float32x2_t' using type 'bfloat16x4_t'} } */
  int32x2_t initi_1_5 = glob_bfloat_vec;  /* { dg-error {incompatible types when initializing type 'int32x2_t' using type 'bfloat16x4_t'} } */
  int16x4_t initi_1_6 = glob_bfloat_vec;  /* { dg-error {incompatible types when initializing type 'int16x4_t' using type 'bfloat16x4_t'} } */

  bfloat16x4_t vector2_1 = {};
  bfloat16x4_t vector2_2 = { glob_bfloat };
  bfloat16x4_t vector2_3 = { glob_bfloat, glob_bfloat, glob_bfloat, glob_bfloat };
  bfloat16x4_t vector2_4 = { 0 };
  bfloat16x4_t vector2_5 = { 0.1 };
  bfloat16x4_t vector2_6 = { is_a_float16 };
  bfloat16x4_t vector2_7 = { is_a_float };
  bfloat16x4_t vector2_8 = { is_an_int };
  bfloat16x4_t vector2_9 = { is_a_short_int };
  bfloat16x4_t vector2_10 = { 0.0, 0, is_a_short_int, is_a_float };

  int32x4_t initi_2_1 = { glob_bfloat };
  float32x4_t initi_2_2 = { glob_bfloat };
  float16x4_t initi_2_3 = { glob_bfloat };
  float32x2_t initi_2_4 = { glob_bfloat };
  int32x2_t initi_2_5 = { glob_bfloat };
  int16x4_t initi_2_6 = { glob_bfloat };

  /* Assignments to/from vectors.  */

  glob_bfloat_vec = glob_bfloat_vec;
  glob_bfloat_vec = 0;   /* { dg-error {incompatible types when assigning to type 'bfloat16x4_t' from type 'int'} } */
  glob_bfloat_vec = 0.1; /* { dg-error {incompatible types when assigning to type 'bfloat16x4_t' from type 'double'} } */
  glob_bfloat_vec = is_a_float_vec; /* { dg-error {incompatible types when assigning to type 'bfloat16x4_t' from type 'float32x4_t'} } */
  glob_bfloat_vec = is_an_int_vec; /* { dg-error {incompatible types when assigning to type 'bfloat16x4_t' from type 'int32x4_t'} } */
  glob_bfloat_vec = is_a_float16_vec; /* { dg-error {incompatible types when assigning to type 'bfloat16x4_t' from type 'float16x4_t'} } */
  glob_bfloat_vec = is_a_float_pair; /* { dg-error {incompatible types when assigning to type 'bfloat16x4_t' from type 'float32x2_t'} } */
  glob_bfloat_vec = is_an_int_pair; /* { dg-error {incompatible types when assigning to type 'bfloat16x4_t' from type 'int32x2_t'} } */
  glob_bfloat_vec = is_a_short_vec; /* { dg-error {incompatible types when assigning to type 'bfloat16x4_t' from type 'int16x4_t'} } */

  is_an_int_vec = glob_bfloat_vec; /* { dg-error {incompatible types when assigning to type 'int32x4_t' from type 'bfloat16x4_t'} } */
  is_a_float_vec = glob_bfloat_vec; /* { dg-error {incompatible types when assigning to type 'float32x4_t' from type 'bfloat16x4_t'} } */
  is_a_float16_vec = glob_bfloat_vec; /* { dg-error {incompatible types when assigning to type 'float16x4_t' from type 'bfloat16x4_t'} } */
  is_a_float_pair = glob_bfloat_vec; /* { dg-error {incompatible types when assigning to type 'float32x2_t' from type 'bfloat16x4_t'} } */
  is_an_int_pair = glob_bfloat_vec; /* { dg-error {incompatible types when assigning to type 'int32x2_t' from type 'bfloat16x4_t'} } */
  is_a_short_vec = glob_bfloat_vec;/* { dg-error {incompatible types when assigning to type 'int16x4_t' from type 'bfloat16x4_t'} } */

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

  (bfloat16x4_t) {};

  (bfloat16x4_t) { 0 };
  (bfloat16x4_t) { 0.1 };
  (bfloat16x4_t) { is_a_float_vec }; /* { dg-error {incompatible types when initializing type '__bf16' using type 'float32x4_t'} } */
  (bfloat16x4_t) { is_an_int_vec }; /* { dg-error {incompatible types when initializing type '__bf16' using type 'int32x4_t'} } */
  (bfloat16x4_t) { is_a_float_pair }; /* { dg-error {incompatible types when initializing type '__bf16' using type 'float32x2_t'} } */
  (bfloat16x4_t) { is_an_int_pair }; /* { dg-error {incompatible types when initializing type '__bf16' using type 'int32x2_t'} } */
  (bfloat16x4_t) { is_a_float16_vec }; /* { dg-error {incompatible types when initializing type '__bf16' using type 'float16x4_t'} } */
  (bfloat16x4_t) { is_a_short_vec }; /* { dg-error {incompatible types when initializing type '__bf16' using type 'int16x4_t'} } */

  (bfloat16x4_t) { glob_bfloat_vec }; /* { dg-error {incompatible types when initializing type '__bf16' using type 'bfloat16x4_t'} } */
  (int32x4_t) { glob_bfloat_vec }; /* { dg-error {incompatible types when initializing type 'int' using type 'bfloat16x4_t'} } */
  (float32x4_t) { glob_bfloat_vec }; /* { dg-error {incompatible types when initializing type 'float' using type 'bfloat16x4_t'} } */
  (int32x2_t) { glob_bfloat_vec }; /* { dg-error {incompatible types when initializing type 'int' using type 'bfloat16x4_t'} } */
  (float16x4_t) { glob_bfloat_vec }; /* { dg-error {incompatible types when initializing type '__fp16' using type 'bfloat16x4_t'} } */
  (int16x4_t) { glob_bfloat_vec }; /* { dg-error {incompatible types when initializing type 'short int' using type 'bfloat16x4_t'} } */

  /* Casting.  */

  (void) glob_bfloat_vec;
  (bfloat16x4_t) glob_bfloat_vec;

  (bfloat16_t) glob_bfloat_vec; /* { dg-error {aggregate value used where a floating-point was expected} } */
  (short) glob_bfloat_vec; /* { dg-error {cannot convert a vector of type 'bfloat16x4_t' to type 'short int' which has different size} } */
  (int) glob_bfloat_vec; /* { dg-error {cannot convert a vector of type 'bfloat16x4_t' to type 'int' which has different size} } */
  (float16_t) glob_bfloat_vec; /* { dg-error {aggregate value used where a floating-point was expected} } */
  (float) glob_bfloat_vec; /* { dg-error {aggregate value used where a floating-point was expected} } */
  (double) glob_bfloat_vec; /* { dg-error {aggregate value used where a floating-point was expected} } */

  (int32x4_t) glob_bfloat_vec; /* { dg-error {cannot convert a value of type 'bfloat16x4_t' to vector type '__Int32x4_t' which has different size} } */
  (float32x4_t) glob_bfloat_vec; /* { dg-error {cannot convert a value of type 'bfloat16x4_t' to vector type '__Float32x4_t' which has different size} } */
  (float16x4_t) glob_bfloat_vec;
  (int32x2_t) glob_bfloat_vec;
  (float32x2_t) glob_bfloat_vec;
  (int16x4_t) glob_bfloat_vec;

  (bfloat16x4_t) is_an_int_vec; /* { dg-error {cannot convert a value of type 'int32x4_t' to vector type '__Bfloat16x4_t' which has different size} } */
  (bfloat16x4_t) is_a_float_vec; /* { dg-error {cannot convert a value of type 'float32x4_t' to vector type '__Bfloat16x4_t' which has different size} } */
  (bfloat16x4_t) is_a_float16_vec;
  (bfloat16x4_t) is_an_int_pair;
  (bfloat16x4_t) is_a_float_pair;
  (bfloat16x4_t) is_a_short_vec;
  (bfloat16x4_t) is_a_double; /* { dg-error {cannot convert value to a vector} } */

  /* Arrays and Structs.  */

  typedef bfloat16x4_t array_type[2];
  extern bfloat16x4_t extern_array[];

  bfloat16x4_t array[2];
  bfloat16x4_t zero_length_array[0];
  bfloat16x4_t empty_init_array[] = {};
  typedef bfloat16x4_t some_other_type[is_an_int];

  struct struct1 {
    bfloat16x4_t a;
  };

  union union1 {
    bfloat16x4_t a;
  };

  /* Addressing and dereferencing.  */

  bfloat16x4_t *bfloat_ptr = &vector0;
  vector0 = *bfloat_ptr;

  /* Pointer assignment.  */

  bfloat16x4_t *bfloat_ptr2 = bfloat_ptr;
  bfloat16x4_t *bfloat_ptr3 = array;

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
  vector0 > 0.1; /* { dg-error {conversion of scalar 'double' to vector 'bfloat16x4_t' involves truncation} } */
  0.1 == vector0; /* { dg-error {conversion of scalar 'double' to vector 'bfloat16x4_t' involves truncation} } */
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
  0 ? bfloat_ptr : float_ptr; /* { dg-error {pointer type mismatch in conditional expression} } */
  0 ? float_ptr : bfloat_ptr; /* { dg-error {pointer type mismatch in conditional expression} } */

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
  vector0 = glob_bfloat_vec + 0.1; /* { dg-error {conversion of scalar 'double' to vector 'bfloat16x4_t' involves truncation} } */
  vector0 = glob_bfloat_vec + 0;
  vector0 = glob_bfloat_vec + is_a_float_vec; /* { dg-error {invalid operands to binary \+} } */

  return vector0;
}

