/* Template file for unary scalar operator validation.

   This file is meant to be included by test files for unary scalar
   operations.  */

/* Check for required settings.  */

#ifndef INSN_NAME
#error INSN_NAME (the intrinsic to test) must be defined.
#endif

#ifndef INPUT_TYPE
#error INPUT_TYPE (basic type of an input value) must be defined.
#endif

#ifndef SCALAR_OPERANDS
#ifndef EXPECTED
#error EXPECTED (an array of expected output values) must be defined.
#endif
#endif

#ifndef OUTPUT_TYPE
#error OUTPUT_TYPE (basic type of an output value) must be defined.
#endif

#ifndef OUTPUT_TYPE_SIZE
#error OUTPUT_TYPE_SIZE (size in bits of an output value) must be defined.
#endif

/* Optional settings.  */

/* SCALAR_OPERANDS: Defined iff the intrinsic has a scalar operand.

   SCALAR_1, SCALAR_2, .., SCALAR_4: If SCALAR_OPERANDS is defined, SCALAR_<n>
   is the scalar and EXPECTED_<n> is array of expected values.

   INPUT: Input values for the first parameter.  Must be of type INPUT_TYPE.  */

/* Additional comments for the error message.  */
#ifndef COMMENT
#define COMMENT ""
#endif

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
  0.0, -0.0,
  2.0, 3.1,
  20.0, 0.40,
  -2.3, 1.33,
  -7.6, 0.31,
  0.3353, 0.5,
  1.0, 13.13,
  -6.3, 20.0,
  (float16_t)INFF, (float16_t)-INFF,
};

#ifndef INPUT
#define INPUT CAT(input_1_,INPUT_TYPE)
#endif

/* Support macros and routines for the test function.  */

#define CHECK()							\
  {								\
    output_conv_type actual;					\
    output_conv_type expect;					\
								\
    expect.hex = ((output_hex_type*)EXPECTED)[index];		\
    actual.value = INSN_NAME ((INPUT)[index]);			\
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

#define CHECK_N(SCALAR, EXPECTED)				\
  {								\
    output_conv_type actual;					\
    output_conv_type expect;					\
								\
    expect.hex							\
      = ((output_hex_type*)EXPECTED)[index];			\
    actual.value = INSN_NAME ((INPUT)[index], (SCALAR));	\
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

  const int num_tests = sizeof (INPUT) / sizeof (INPUT[0]);
  int index;

  for (index = 0; index < num_tests; index++)
    {
#if defined (SCALAR_OPERANDS)

#ifdef SCALAR_1
      CHECK_N (SCALAR_1, EXPECTED_1);
#endif
#ifdef SCALAR_2
      CHECK_N (SCALAR_2, EXPECTED_2);
#endif
#ifdef SCALAR_3
      CHECK_N (SCALAR_3, EXPECTED_3);
#endif
#ifdef SCALAR_4
      CHECK_N (SCALAR_4, EXPECTED_4);
#endif

#else /* !defined (SCALAR_OPERAND).  */
      CHECK ();
#endif
    }

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
