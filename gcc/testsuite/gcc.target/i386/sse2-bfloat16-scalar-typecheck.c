/* { dg-do compile } */
/* { dg-options "-msse2 -O2" } */


__bf16 glob_bfloat;

int is_an_int;
short is_a_short_int;
float is_a_float;
float is_a_float16;
double is_a_double;

float *float_ptr;

__bf16 foo1 (void) { return (__bf16) 0x1234; }
__bf16 foo2 (void) { return (__bf16) (short) 0x1234; }

__bf16 footest (__bf16 scalar0)
{

  /* Initialisation  */

  __bf16 scalar1_1;
  __bf16 scalar1_2 = glob_bfloat;
  __bf16 scalar1_3 = 0;
  __bf16 scalar1_4 = 0.1;
  __bf16 scalar1_5 = is_a_float;
  __bf16 scalar1_6 = is_an_int;
  __bf16 scalar1_7 = is_a_float16;
  __bf16 scalar1_8 = is_a_double;
  __bf16 scalar1_9 = is_a_short_int;

  int initi_1_1 = glob_bfloat;
  float initi_1_2 = glob_bfloat;
  _Float16 initi_1_3 = glob_bfloat;
  short initi_1_4 = glob_bfloat;
  double initi_1_5 = glob_bfloat;

  __bf16 scalar2_1 = {};
  __bf16 scalar2_2 = { glob_bfloat };
  __bf16 scalar2_3 = { 0 };
  __bf16 scalar2_4 = { 0.1 };
  __bf16 scalar2_5 = { is_a_float };
  __bf16 scalar2_6 = { is_an_int };
  __bf16 scalar2_7 = { is_a_float16 };
  __bf16 scalar2_8 = { is_a_double };
  __bf16 scalar2_9 = { is_a_short_int };

  int initi_2_1 = { glob_bfloat };
  float initi_2_2 = { glob_bfloat };
  _Float16 initi_2_3 = { glob_bfloat };
  short initi_2_4 = { glob_bfloat };
  double initi_2_5 = { glob_bfloat };

  /* Assignments.  */

  glob_bfloat = glob_bfloat;
  glob_bfloat = 0;
  glob_bfloat = 0.1;
  glob_bfloat = is_a_float;
  glob_bfloat = is_an_int;
  glob_bfloat = is_a_float16;
  glob_bfloat = is_a_double;
  glob_bfloat = is_a_short_int;

  is_an_int = glob_bfloat;
  is_a_float = glob_bfloat;
  is_a_float16 = glob_bfloat;
  is_a_double = glob_bfloat;
  is_a_short_int = glob_bfloat;

  /* Casting.  */

  (void) glob_bfloat;
  (__bf16) glob_bfloat;

  (int) glob_bfloat;
  (float) glob_bfloat;
  (_Float16) glob_bfloat;
  (double) glob_bfloat;
  (short) glob_bfloat;

  (__bf16) is_an_int;
  (__bf16) is_a_float;
  (__bf16) is_a_float16;
  (__bf16) is_a_double;
  (__bf16) is_a_short_int;

  /* Compound literals.  */

  (__bf16) {};
  (__bf16) { glob_bfloat };
  (__bf16) { 0 };
  (__bf16) { 0.1 };
  (__bf16) { is_a_float };
  (__bf16) { is_an_int };
  (__bf16) { is_a_float16 };
  (__bf16) { is_a_double };
  (__bf16) { is_a_short_int };

  (int) { glob_bfloat };
  (float) { glob_bfloat };
  (_Float16) { glob_bfloat };
  (double) { glob_bfloat };
  (short) { glob_bfloat };

  /* Arrays and Structs.  */

  typedef __bf16 array_type[2];
  extern __bf16 extern_array[];

  __bf16 array[2];
  __bf16 zero_length_array[0];
  __bf16 empty_init_array[] = {};
  typedef __bf16 some_other_type[is_an_int];

  struct struct1 {
    __bf16 a;
  };

  union union1 {
    __bf16 a;
  };

  /* Addressing and dereferencing.  */

  __bf16 *bfloat_ptr = &scalar0;
  scalar0 = *bfloat_ptr;

  /* Pointer assignment.  */

  __bf16 *bfloat_ptr2 = bfloat_ptr;
  __bf16 *bfloat_ptr3 = array;

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
  scalar0 > glob_bfloat;
  glob_bfloat == scalar0;
  scalar0 > is_a_float;
  is_a_float == scalar0;
  scalar0 > 0;
  0 == scalar0;
  scalar0 > 0.1;
  0.1 == scalar0;
  scalar0 > is_an_int;
  is_an_int == scalar0;

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
  0 ? scalar0 : is_a_float;
  0 ? is_a_float : scalar0;
  0 ? scalar0 : 0;
  0 ? 0 : scalar0;
  0 ? 0.1 : scalar0;
  0 ? scalar0 : 0.1;
  0 ? bfloat_ptr : bfloat_ptr2;
  0 ? bfloat_ptr : float_ptr; /* { dg-warning {pointer type mismatch in conditional expression} } */
  0 ? float_ptr : bfloat_ptr; /* { dg-warning {pointer type mismatch in conditional expression} } */

  scalar0 ? scalar0 : scalar0;
  scalar0 ? is_a_float : scalar0;
  scalar0 ? scalar0 : is_a_float;
  scalar0 ? is_a_float : is_a_float;

  /* Unary operators.  */

  +scalar0;
  -scalar0;
  ~scalar0; /* { dg-error {wrong type argument to bit-complement} } */
  !scalar0;
  *scalar0; /* { dg-error {invalid type argument of unary '\*'} } */
  __real scalar0;
  __imag scalar0;
  ++scalar0;
  --scalar0;
  scalar0++;
  scalar0--;

  /* Binary arithmetic operations.  */

  scalar0 = glob_bfloat + *bfloat_ptr;
  scalar0 = glob_bfloat + 0.1;
  scalar0 = glob_bfloat + 0;
  scalar0 = glob_bfloat + is_a_float;

  return scalar0;
}

