#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest_int1 (CFI_cdesc_t *arg_int,
			CFI_cdesc_t *arg_short,
			CFI_cdesc_t *arg_long,
			CFI_cdesc_t *arg_long_long,
			CFI_cdesc_t *arg_signed_char);

extern void ctest_int2 (CFI_cdesc_t *arg_int8,
			CFI_cdesc_t *arg_int16,
			CFI_cdesc_t *arg_int32,
			CFI_cdesc_t *arg_int64);

extern void ctest_int3 (CFI_cdesc_t *arg_least8,
			CFI_cdesc_t *arg_least16,
			CFI_cdesc_t *arg_least32,
			CFI_cdesc_t *arg_least64);

extern void ctest_int4 (CFI_cdesc_t *arg_fast8,
			CFI_cdesc_t *arg_fast16,
			CFI_cdesc_t *arg_fast32,
			CFI_cdesc_t *arg_fast64);

extern void ctest_int5 (CFI_cdesc_t *arg_size,
			CFI_cdesc_t *arg_intmax,
			CFI_cdesc_t *arg_intptr,
			CFI_cdesc_t *arg_ptrdiff);

extern void ctest_real (CFI_cdesc_t *arg_float,
			CFI_cdesc_t *arg_double);

extern void ctest_complex (CFI_cdesc_t *arg_float_complex,
			   CFI_cdesc_t *arg_double_complex);

extern void ctest_misc (CFI_cdesc_t *arg_bool,
			CFI_cdesc_t *arg_cptr,
			CFI_cdesc_t *arg_cfunptr,
			CFI_cdesc_t *arg_struct);

/* Sanity check the type info in the descriptor a.  */

static void
check (CFI_cdesc_t *a, size_t size, int typecode)
{
  dump_CFI_cdesc_t (a);
  if (a->attribute != CFI_attribute_other)
    abort ();
  if (a->base_addr == NULL)
    abort ();
  if (a->rank != 1)
    abort ();
  if (size && a->elem_len != size)
    abort ();
  if (a->type != typecode)
    abort ();
}


/* Test that the basic integer types correspond correctly.  */
void
ctest_int1 (CFI_cdesc_t *arg_int,
	    CFI_cdesc_t *arg_short,
	    CFI_cdesc_t *arg_long,
	    CFI_cdesc_t *arg_long_long,
	    CFI_cdesc_t *arg_signed_char)
{
  check (arg_int, sizeof (int), CFI_type_int);
  check (arg_short, sizeof (short), CFI_type_short);
  check (arg_long, sizeof (long), CFI_type_long);
  check (arg_long_long, sizeof (long long int), CFI_type_long_long);
  check (arg_signed_char, sizeof (signed char), CFI_type_signed_char);
}

/* Test the integer types of explicit sizes.  */
void
ctest_int2 (CFI_cdesc_t *arg_int8,
	    CFI_cdesc_t *arg_int16,
	    CFI_cdesc_t *arg_int32,
	    CFI_cdesc_t *arg_int64)
{
  check (arg_int8, sizeof (int8_t), CFI_type_int8_t);
  check (arg_int16, sizeof (int16_t), CFI_type_int16_t);
  check (arg_int32, sizeof (int32_t), CFI_type_int32_t);
  check (arg_int64, sizeof (int64_t), CFI_type_int64_t);
}

/* Check the int_least*_t types.  */

void
ctest_int3 (CFI_cdesc_t *arg_least8,
	    CFI_cdesc_t *arg_least16,
	    CFI_cdesc_t *arg_least32,
	    CFI_cdesc_t *arg_least64)
{
  check (arg_least8, sizeof (int_least8_t), CFI_type_int_least8_t);
  check (arg_least16, sizeof (int_least16_t), CFI_type_int_least16_t);
  check (arg_least32, sizeof (int_least32_t), CFI_type_int_least32_t);
  check (arg_least64, sizeof (int_least64_t), CFI_type_int_least64_t);
}

/* Check the int_fast*_t types.  */
void
ctest_int4 (CFI_cdesc_t *arg_fast8,
	    CFI_cdesc_t *arg_fast16,
	    CFI_cdesc_t *arg_fast32,
	    CFI_cdesc_t *arg_fast64)
{
  check (arg_fast8, sizeof (int_fast8_t), CFI_type_int_fast8_t);
  check (arg_fast16, sizeof (int_fast16_t), CFI_type_int_fast16_t);
  check (arg_fast32, sizeof (int_fast32_t), CFI_type_int_fast32_t);
  check (arg_fast64, sizeof (int_fast64_t), CFI_type_int_fast64_t);
}

/* Check the "purposeful" integer types.  */
void
ctest_int5 (CFI_cdesc_t *arg_size,
	    CFI_cdesc_t *arg_intmax,
	    CFI_cdesc_t *arg_intptr,
	    CFI_cdesc_t *arg_ptrdiff)
{
  check (arg_size, sizeof (size_t), CFI_type_size_t);
  check (arg_intmax, sizeof (intmax_t), CFI_type_intmax_t);
  check (arg_intptr, sizeof (intptr_t), CFI_type_intptr_t);
  check (arg_ptrdiff, sizeof (ptrdiff_t), CFI_type_ptrdiff_t);
}

/* Check the floating-point types.  */
void
ctest_real (CFI_cdesc_t *arg_float,
	    CFI_cdesc_t *arg_double)
{
  check (arg_float, sizeof (float), CFI_type_float);
  check (arg_double, sizeof (double), CFI_type_double);
}

/* Likewise for the complex types.  */
void
ctest_complex (CFI_cdesc_t *arg_float_complex,
	       CFI_cdesc_t *arg_double_complex)
{
  check (arg_float_complex, sizeof (float _Complex),
	 CFI_type_float_Complex);
  check (arg_double_complex, sizeof (double _Complex),
	 CFI_type_double_Complex);
}

/* Misc types.  */
void
ctest_misc (CFI_cdesc_t *arg_bool,
	    CFI_cdesc_t *arg_cptr,
	    CFI_cdesc_t *arg_cfunptr,
	    CFI_cdesc_t *arg_struct)
{
  struct m
  {
    int i, j;
  };

  check (arg_bool, sizeof (_Bool), CFI_type_Bool);
  check (arg_cptr, sizeof (void *), CFI_type_cptr);
  check (arg_cfunptr, sizeof (void (*)(void)), CFI_type_cfunptr);
  check (arg_struct, sizeof (struct m), CFI_type_struct);
}
