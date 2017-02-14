/* Template file for ternary scalar operator validation.

   This file is meant to be included by test files for binary scalar
   operations.  */

/* Check for required settings.  */

#ifndef INSN_NAME
#error INSN_NAME (the intrinsic to test) must be defined.
#endif

#ifndef INPUT_TYPE
#error INPUT_TYPE (basic type of an input value) must be defined.
#endif

#ifndef OUTPUT_TYPE
#error OUTPUT_TYPE (basic type of an output value) must be defined.
#endif

#ifndef OUTPUT_TYPE_SIZE
#error OUTPUT_TYPE_SIZE (size in bits of an output value) must be defined.
#endif

/* Optional settings:

   INPUT_1: Input values for the first parameter.  Must be of type INPUT_TYPE.
   INPUT_2: Input values for the second parameter.  Must be of type INPUT_TYPE.
   INPUT_3: Input values for the third parameter.  Must be of type
   INPUT_TYPE.  */

#ifndef TEST_MSG
#define TEST_MSG "unnamed test"
#endif

/* The test framework.  */

#include <stdio.h>

extern void abort ();

#define INFF __builtin_inf ()

/* Stringify a macro.  */
#define STR0(A) #A
#define STR(A) STR0 (A)

/* Macro concatenation.  */
#define CAT0(A, B) A##B
#define CAT(A, B) CAT0 (A, B)

/* Format strings for error reporting.  */
#define FMT16 "0x%04x"
#define FMT32 "0x%08x"
#define FMT CAT (FMT,OUTPUT_TYPE_SIZE)

/* Type construction: forms TS_t, where T is the base type and S the size in
   bits.  */
#define MK_TYPE0(T, S) T##S##_t
#define MK_TYPE(T, S) MK_TYPE0 (T, S)

/* Convenience types for input and output data.  */
typedef MK_TYPE (uint, OUTPUT_TYPE_SIZE) output_hex_type;

/* Conversion between typed values and their hexadecimal representation.  */
typedef union
{
  OUTPUT_TYPE value;
  output_hex_type hex;
} output_conv_type;

/* Default input values.  */

float16_t input_1_float16_t[] =
{
  0.0,
  -0.0,
  2.0,
  3.1,
  20.0,
  0.40,
  -2.3,
  1.33,
  -7.6,
  0.31,
  0.3353,
  0.5,
  1.0,
  13.13,
  -6.3,
  20.0,
  (float16_t)INFF,
  (float16_t)-INFF,
};

float16_t input_2_float16_t[] =
{
  1.0,
  1.0,
  -4.33,
  100.0,
  30.0,
  -0.02,
  0.5,
  -7.231,
  -6.3,
  20.0,
  -7.231,
  2.3,
  -7.6,
  5.1,
  0.31,
  0.33353,
  (float16_t)-INFF,
  (float16_t)INFF,
};

float16_t input_3_float16_t[] =
{
  -0.0,
  0.0,
  0.31,
  -0.31,
  1.31,
  2.1,
  -6.3,
  1.0,
  -1.5,
  5.1,
  0.3353,
  9.3,
  -9.3,
  -7.231,
  0.5,
  -0.33,
  (float16_t)INFF,
  (float16_t)INFF,
};

#ifndef INPUT_1
#define INPUT_1 CAT (input_1_,INPUT_TYPE)
#endif

#ifndef INPUT_2
#define INPUT_2 CAT (input_2_,INPUT_TYPE)
#endif

#ifndef INPUT_3
#define INPUT_3 CAT (input_3_,INPUT_TYPE)
#endif

/* Support macros and routines for the test function.  */

#define CHECK()							\
  {								\
    output_conv_type actual;					\
    output_conv_type expect;					\
								\
    expect.hex = ((output_hex_type*)EXPECTED)[index];		\
    actual.value = INSN_NAME ((INPUT_1)[index],			\
			      (INPUT_2)[index],			\
			      (INPUT_3)[index]);		\
								\
    if (actual.hex != expect.hex)				\
      {								\
	fprintf (stderr,					\
		 "ERROR in %s (%s line %d), buffer %s, "	\
		 "index %d: got "				\
		 FMT " != " FMT "\n",				\
		 TEST_MSG, __FILE__, __LINE__,			\
		 STR (EXPECTED), index,				\
		 actual.hex, expect.hex);			\
	abort ();						\
      }								\
    fprintf (stderr, "CHECKED %s %s\n",				\
	     STR (EXPECTED), TEST_MSG);				\
  }

#define FNNAME1(NAME) exec_ ## NAME
#define FNNAME(NAME) FNNAME1 (NAME)

/* The test function.  */

void
FNNAME (INSN_NAME) (void)
{
  /* Basic test: y[i] = OP (x[i]), for each INPUT[i], then compare the result
     against EXPECTED[i].  */

  const int num_tests = sizeof (INPUT_1) / sizeof (INPUT_1[0]);
  int index;

  for (index = 0; index < num_tests; index++)
    CHECK ();

#ifdef EXTRA_TESTS
  EXTRA_TESTS ();
#endif
}

int
main (void)
{
  FNNAME (INSN_NAME) ();

  return 0;
}
