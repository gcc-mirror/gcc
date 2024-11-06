/* Test that mfloat8_t is only usable with intrinsics, thus not convertible.  */
/* { dg-do assemble } */
/* { dg-options "-O1 -march=armv9.4-a+fp8 -Wno-narrowing" } */

#include <arm_neon.h>
#include <stdint.h>
#include <type_traits>

mfloat8_t glob_fp8;

int is_an_int;
uint8_t is_a_uint8;
int8_t is_an_int8;
short is_a_short_int;
float is_a_float;
double is_a_double;

uint8_t *uint8_ptr;

mfloat8_t
invalid_from_fp8 (uint16_t __a)
{
  mfloat8_t b = __a; /* { dg-error "invalid conversion to type 'mfloat8_t'" } */
  return b;
}

uint16_t
invalid_to_fp8 (mfloat8_t __a)
{
  uint16_t b = __a; /*{ dg-error "invalid conversion from type 'mfloat8_t'" } */
  return b;
}

mfloat8_t
foo1 (void)
{
  return (mfloat8_t)0x1234; /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
}

mfloat8_t
foo2 (void)
{
  return (mfloat8_t)(short)0x1234; /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
}

mfloat8_t
footest (mfloat8_t scalar0)
{

  /* Initialisation  */

  mfloat8_t scalar1_1;
  mfloat8_t scalar1_2 = glob_fp8;
  mfloat8_t scalar1_3
      = 0; /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  mfloat8_t scalar1_4
      = 0.1; /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  mfloat8_t scalar1_5
      = is_a_float; /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  mfloat8_t scalar1_6
      = is_an_int; /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  mfloat8_t scalar1_8
      = is_a_double; /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  mfloat8_t scalar1_9 = is_a_short_int; /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  mfloat8_t scalar1_10
      = is_a_uint8; /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  mfloat8_t scalar1_11
      = is_an_int8; /* { dg-error {invalid conversion to type 'mfloat8_t'} } */

  int initi_1_1
      = glob_fp8; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  float initi_1_2
      = glob_fp8; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  short initi_1_4
      = glob_fp8; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  double initi_1_5
      = glob_fp8; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  uint8_t initi_1_6
      = glob_fp8; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  int8_t initi_1_7
      = glob_fp8; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */

  mfloat8_t scalar2_1 = {};
  mfloat8_t scalar2_2 = { glob_fp8 };
  mfloat8_t scalar2_3
      = { 0 }; /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  mfloat8_t scalar2_4
      = { 0.1 }; /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  mfloat8_t scalar2_5 = {
    is_a_float /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  };
  mfloat8_t scalar2_6 = {
    is_an_int /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  };
  mfloat8_t scalar2_8 = {
    is_a_double /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  };
  mfloat8_t scalar2_9 = {
    is_a_short_int /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  };
  mfloat8_t scalar2_10 = {
    is_a_uint8 /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  };
  mfloat8_t scalar2_11 = {
    is_an_int8 /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  };

  int initi_2_1 = {
    glob_fp8 /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  };
  float initi_2_2 = {
    glob_fp8 /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  };
  short initi_2_4 = {
    glob_fp8 /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  };
  double initi_2_5 = {
    glob_fp8 /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  };
  uint8_t initi_2_6 = {
    glob_fp8 /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  };
  int8_t initi_2_7 = {
    glob_fp8 /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  };

  /* Assignments.  */

  glob_fp8 = glob_fp8;
  glob_fp8 = 0;	  /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  glob_fp8 = 0.1; /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  glob_fp8
      = is_a_float; /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  glob_fp8
      = is_an_int; /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  glob_fp8
      = is_a_double; /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  glob_fp8 = is_a_short_int; /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  glob_fp8
      = is_a_uint8; /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  glob_fp8
      = is_an_int8; /* { dg-error {invalid conversion to type 'mfloat8_t'} } */

  is_an_int
      = glob_fp8; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  is_a_float
      = glob_fp8; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  is_a_double
      = glob_fp8; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  is_a_short_int
      = glob_fp8; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  is_a_uint8
      = glob_fp8; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  is_an_int8
      = glob_fp8; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */

  /* Casting.  */

  (void)glob_fp8;
  (mfloat8_t) glob_fp8;

  (int)glob_fp8;   /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  (float)glob_fp8; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  (double)
      glob_fp8;	   /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  (short)glob_fp8; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  (uint8_t)
      glob_fp8; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  (int8_t)
      glob_fp8; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */

  (mfloat8_t)
      is_an_int; /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  (mfloat8_t)
      is_a_float; /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  (mfloat8_t)
      is_a_double; /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  (mfloat8_t) is_a_short_int; /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  (mfloat8_t)
      is_a_uint8; /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  (mfloat8_t)
      is_an_int8; /* { dg-error {invalid conversion to type 'mfloat8_t'} } */

  /* Compound literals.  */

  (mfloat8_t){};
  (mfloat8_t){ glob_fp8 };
  (mfloat8_t){ 0 }; /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  (mfloat8_t){
    0.1 /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  }; 
  (mfloat8_t){
    is_a_float /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  };
  (mfloat8_t){
    is_an_int /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  };
  (mfloat8_t){
    is_a_double /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  };
  (mfloat8_t){
    is_a_short_int /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  };
  (mfloat8_t){
    is_a_uint8 /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  };
  (mfloat8_t){
    is_an_int8 /* { dg-error {invalid conversion to type 'mfloat8_t'} } */
  };

  (int){
    glob_fp8 /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  };
  (float){
    glob_fp8 /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  };
  (double){
    glob_fp8 /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  };
  (short){
    glob_fp8 /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  };
  (uint8_t){
    glob_fp8 /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  };
  (int8_t){
    glob_fp8 /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  };

  /* Arrays and Structs.  */

  typedef mfloat8_t array_type[2];
  extern mfloat8_t extern_array[];

  mfloat8_t array[2];
  mfloat8_t zero_length_array[0];
  mfloat8_t empty_init_array[] = {};
  typedef mfloat8_t some_other_type[is_an_int];

  struct struct1
  {
    mfloat8_t a;
  };

  union union1
  {
    mfloat8_t a;
  };

  /* Addressing and dereferencing.  */

  mfloat8_t *fp8_ptr = &scalar0;
  scalar0 = *fp8_ptr;

  /* Pointer assignment.  */

  mfloat8_t *fp8_ptr2 = fp8_ptr;
  mfloat8_t *fp8_ptr3 = array;

  /* Pointer arithmetic.  */

  ++fp8_ptr;
  --fp8_ptr;
  fp8_ptr++;
  fp8_ptr--;
  fp8_ptr += 1;
  fp8_ptr -= 1;
  fp8_ptr - fp8_ptr2;
  fp8_ptr = &fp8_ptr3[0];
  fp8_ptr = &fp8_ptr3[1];

  /* Simple comparison.  */
  scalar0
      > glob_fp8; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  glob_fp8
      == scalar0; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  scalar0 > is_a_float; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  is_a_float
      == scalar0; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  scalar0 > 0;	  /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  0 == scalar0;	  /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  scalar0 > 0.1;  /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  0.1 == scalar0; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  scalar0
      > is_an_int; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  is_an_int
      == scalar0; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */

  /* Pointer comparison.  */

  fp8_ptr == &scalar0;
  fp8_ptr != &scalar0;
  fp8_ptr < &scalar0;
  fp8_ptr <= &scalar0;
  fp8_ptr > &scalar0;
  fp8_ptr >= &scalar0;
  fp8_ptr == fp8_ptr2;
  fp8_ptr != fp8_ptr2;
  fp8_ptr < fp8_ptr2;
  fp8_ptr <= fp8_ptr2;
  fp8_ptr > fp8_ptr2;
  fp8_ptr >= fp8_ptr2;

  /* Conditional expressions.  */

  0 ? scalar0 : scalar0;
  0 ? scalar0 /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
    : is_a_float;
  0 ? is_a_float
    : scalar0;	   /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  0 ? scalar0 : 0; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  0 ? 0 : scalar0; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  0 ? 0.1
    : scalar0; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  0 ? scalar0  /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
    : 0.1;
  0 ? fp8_ptr : fp8_ptr2;
  0 ? fp8_ptr : uint8_ptr; /* { dg-error {conditional expression between distinct pointer types} } */
  0 ? uint8_ptr : fp8_ptr; /* { dg-error {conditional expression between distinct pointer types} } */

  scalar0 /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
      ? scalar0
      : scalar0;
  scalar0 /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
      ? is_a_float
      : scalar0;
  scalar0 ? scalar0 : is_a_float;    /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  scalar0 ? is_a_float : is_a_float; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */

  /* Unary operators.  */

  +scalar0; /* { dg-error {operation not permitted on type 'mfloat8_t'} } */
  -scalar0; /* { dg-error {operation not permitted on type 'mfloat8_t'} } */
  ~scalar0; /* { dg-error {operation not permitted on type 'mfloat8_t'} } */
  !scalar0; /* { dg-error {operation not permitted on type 'mfloat8_t'} } */
  *scalar0; /* { dg-error {invalid type argument of unary} } */
  __real scalar0; /* { dg-error {operation not permitted on type 'mfloat8_t'} } */
  __imag scalar0; /* { dg-error {operation not permitted on type 'mfloat8_t'} } */
  ++scalar0; /* { dg-error {operation not permitted on type 'mfloat8_t'} } */
  --scalar0; /* { dg-error {operation not permitted on type 'mfloat8_t'} } */
  scalar0++; /* { dg-error {operation not permitted on type 'mfloat8_t'} } */
  scalar0--; /* { dg-error {operation not permitted on type 'mfloat8_t'} } */

  /* Binary arithmetic operations.  */

  scalar0 = glob_fp8 + scalar1_2; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  scalar0 = glob_fp8 + *fp8_ptr;  /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  scalar0 = glob_fp8
	    + 0.1; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  scalar0 = glob_fp8
	    + 0; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  scalar0 = glob_fp8 + is_a_float; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */

  glob_fp8 + glob_fp8; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  glob_fp8 - glob_fp8; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  glob_fp8 * glob_fp8; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  glob_fp8 / glob_fp8; /* { dg-error {invalid conversion from type 'mfloat8_t'} } */
  glob_fp8 && glob_fp8; /* { dg-error {operation not permitted on type 'mfloat8_t'} } */
  glob_fp8 || glob_fp8; /* { dg-error {operation not permitted on type 'mfloat8_t'} } */

  return scalar0;
}

/* Check that function decls for mfloat8_t and unsigned char differ */

mfloat8_t extern_fn1(void);
unsigned char extern_fn1(void); /* { dg-error {ambiguating new declaration of 'unsigned char extern_fn1\(\)'} } */

mfloat8_t extern_fn2(void);
uint8_t extern_fn2(void); /* { dg-error {ambiguating new declaration of 'uint8_t extern_fn2\(\)} } */

unsigned char extern_fn3(void);
mfloat8_t extern_fn3(void); /* { dg-error {ambiguating new declaration of 'mfloat8_t extern_fn3\(\)} } */

uint8_t extern_fn4(void);
mfloat8_t extern_fn4(void); /* { dg-error {ambiguating new declaration of 'mfloat8_t extern_fn4\(\)} } */

/* Check that the type conforms to the contract */
static_assert(!std::is_integral<__mfp8>(), "not integral");
static_assert(!std::is_signed<__mfp8>(), "not signed");
static_assert(!std::is_unsigned<__mfp8>(), "not unsigned");
