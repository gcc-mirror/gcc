/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon }  */
/* { dg-additional-options "-Wno-pedantic -O3 --save-temps" }  */

#include <arm_neon.h>

bfloat16_t glob_bfloat;

int is_an_int;
short is_a_short_int;
float is_a_float;
float is_a_float16;
double is_a_double;

float *float_ptr;

bfloat16_t foo1 (void) { return (bfloat16_t) 0x1234; } /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
bfloat16_t foo2 (void) { return (bfloat16_t) (short) 0x1234; } /* { dg-error {invalid conversion to type 'bfloat16_t'} } */

bfloat16_t footest (bfloat16_t scalar0)
{

  /* Initialisation  */

  bfloat16_t scalar1_1;
  bfloat16_t scalar1_2 = glob_bfloat;
  bfloat16_t scalar1_3 = 0;   /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  bfloat16_t scalar1_4 = 0.1; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  bfloat16_t scalar1_5 = is_a_float; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  bfloat16_t scalar1_6 = is_an_int;  /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  bfloat16_t scalar1_7 = is_a_float16; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  bfloat16_t scalar1_8 = is_a_double; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  bfloat16_t scalar1_9 = is_a_short_int; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */

  int initi_1_1 = glob_bfloat; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  float initi_1_2 = glob_bfloat; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  float16_t initi_1_3 = glob_bfloat; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  short initi_1_4 = glob_bfloat; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  double initi_1_5 = glob_bfloat; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */

  bfloat16_t scalar2_1 = {}; /* { dg-error {empty scalar initializer} } */
  bfloat16_t scalar2_2 = { glob_bfloat };
  bfloat16_t scalar2_3 = { 0 }; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  bfloat16_t scalar2_4 = { 0.1 }; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  bfloat16_t scalar2_5 = { is_a_float }; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  bfloat16_t scalar2_6 = { is_an_int }; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  bfloat16_t scalar2_7 = { is_a_float16 }; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  bfloat16_t scalar2_8 = { is_a_double }; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  bfloat16_t scalar2_9 = { is_a_short_int }; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */

  int initi_2_1 = { glob_bfloat }; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  float initi_2_2 = { glob_bfloat }; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  float16_t initi_2_3 = { glob_bfloat }; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  short initi_2_4 = { glob_bfloat }; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  double initi_2_5 = { glob_bfloat }; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */

  /* Assignments.  */

  glob_bfloat = glob_bfloat;
  glob_bfloat = 0;   /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  glob_bfloat = 0.1; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  glob_bfloat = is_a_float; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  glob_bfloat = is_an_int; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  glob_bfloat = is_a_float16; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  glob_bfloat = is_a_double; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  glob_bfloat = is_a_short_int; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */

  is_an_int = glob_bfloat; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  is_a_float = glob_bfloat; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  is_a_float16 = glob_bfloat; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  is_a_double = glob_bfloat; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  is_a_short_int = glob_bfloat; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */

  /* Casting.  */

  (void) glob_bfloat;
  (bfloat16_t) glob_bfloat;

  (int) glob_bfloat; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  (float) glob_bfloat; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  (float16_t) glob_bfloat; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  (double) glob_bfloat; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  (short) glob_bfloat; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */

  (bfloat16_t) is_an_int; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  (bfloat16_t) is_a_float; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  (bfloat16_t) is_a_float16; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  (bfloat16_t) is_a_double; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  (bfloat16_t) is_a_short_int; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */

  /* Compound literals.  */

  (bfloat16_t) {}; /* { dg-error {empty scalar initializer} } */
  (bfloat16_t) { glob_bfloat };
  (bfloat16_t) { 0 }; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  (bfloat16_t) { 0.1 }; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  (bfloat16_t) { is_a_float }; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  (bfloat16_t) { is_an_int }; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  (bfloat16_t) { is_a_float16 }; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  (bfloat16_t) { is_a_double }; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  (bfloat16_t) { is_a_short_int }; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */

  (int) { glob_bfloat }; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  (float) { glob_bfloat }; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  (float16_t) { glob_bfloat }; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  (double) { glob_bfloat }; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  (short) { glob_bfloat }; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */

  /* Arrays and Structs.  */

  typedef bfloat16_t array_type[2];
  extern bfloat16_t extern_array[];

  bfloat16_t array[2];
  bfloat16_t zero_length_array[0];
  bfloat16_t empty_init_array[] = {};
  typedef bfloat16_t some_other_type[is_an_int];

  struct struct1 {
    bfloat16_t a;
  };

  union union1 {
    bfloat16_t a;
  };

  /* Addressing and dereferencing.  */

  bfloat16_t *bfloat_ptr = &scalar0;
  scalar0 = *bfloat_ptr;

  /* Pointer assignment.  */

  bfloat16_t *bfloat_ptr2 = bfloat_ptr;
  bfloat16_t *bfloat_ptr3 = array;

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
  scalar0 > glob_bfloat; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  glob_bfloat == scalar0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  scalar0 > is_a_float; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  is_a_float == scalar0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  scalar0 > 0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  0 == scalar0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  scalar0 > 0.1; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  0.1 == scalar0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  scalar0 > is_an_int; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  is_an_int == scalar0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */

  /* Pointer comparison.  */

  bfloat_ptr == &scalar0;
  bfloat_ptr != &scalar0;
  bfloat_ptr < &scalar0;
  bfloat_ptr <= &scalar0;
  bfloat_ptr > &scalar0;
  bfloat_ptr >= &scalar0;
  bfloat_ptr == bfloat_ptr2;
  bfloat_ptr != bfloat_ptr2;
  bfloat_ptr < bfloat_ptr2;
  bfloat_ptr <= bfloat_ptr2;
  bfloat_ptr > bfloat_ptr2;
  bfloat_ptr >= bfloat_ptr2;

  /* Conditional expressions.  */

  0 ? scalar0 : scalar0;
  0 ? scalar0 : is_a_float; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  0 ? is_a_float : scalar0; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  0 ? scalar0 : 0; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  0 ? 0 : scalar0; /* { dg-error {invalid conversion to type 'bfloat16_t'} } */
  0 ? 0.1 : scalar0; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  0 ? scalar0 : 0.1; /* { dg-error {invalid conversion from type 'bfloat16_t'} } */
  0 ? bfloat_ptr : bfloat_ptr2;
  0 ? bfloat_ptr : float_ptr; /* { dg-error {pointer type mismatch in conditional expression} } */
  0 ? float_ptr : bfloat_ptr; /* { dg-error {pointer type mismatch in conditional expression} } */

  scalar0 ? scalar0 : scalar0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  scalar0 ? is_a_float : scalar0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  scalar0 ? scalar0 : is_a_float; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  scalar0 ? is_a_float : is_a_float; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */

  /* Unary operators.  */

  +scalar0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  -scalar0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  ~scalar0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  !scalar0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  *scalar0; /* { dg-error {invalid type argument of unary '\*'} } */
  __real scalar0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  __imag scalar0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  ++scalar0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  --scalar0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  scalar0++; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  scalar0--; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */

  /* Binary arithmetic operations.  */

  scalar0 = glob_bfloat + *bfloat_ptr; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  scalar0 = glob_bfloat + 0.1; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  scalar0 = glob_bfloat + 0; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */
  scalar0 = glob_bfloat + is_a_float; /* { dg-error {operation not permitted on type 'bfloat16_t'} } */

  return scalar0;
}

