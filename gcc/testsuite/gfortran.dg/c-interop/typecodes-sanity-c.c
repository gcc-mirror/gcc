#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <ISO_Fortran_binding.h>

extern void ctest_typecodes (void);

/* Do sanity checking on the CFI_type_* macros.  In particular, make sure
   that if two type codes have the same value, they represent objects of the
   same size.  */

struct tc_info
{
  CFI_type_t typecode;
  char *name;
  size_t size;
  int extension;
};

static struct tc_info tc_table[] =
{
  /* Extension types.
     Note there is no portable C equivalent type for CFI_type_ucs4_char type
     (4-byte Unicode characters), so this is kind of hacky...  */
#if CFI_type_int128_t > 0
  { CFI_type_int128_t, "CFI_type_int128_t",
    sizeof (__int128), 1 },
  { CFI_type_int_least128_t, "CFI_type_int_least128_t",
    sizeof (__int128), 1 },
  { CFI_type_int_fast128_t, "CFI_type_int_fast128_t",
    sizeof (__int128), 1 },
#endif
#if CFI_type_ucs4_char > 0  
  { CFI_type_ucs4_char, "CFI_type_ucs4_char", 4, 1 },
#endif
#if CFI_type_float128 > 0  
  { CFI_type_float128, "CFI_type_float128",
    sizeof (_Float128), 1 },
  { CFI_type_float128_Complex, "CFI_type_float128_Complex",
    sizeof (_Float128 _Complex), 1 },
#endif
#if CFI_type_cfunptr > 0  
  { CFI_type_cfunptr, "CFI_type_cfunptr",
    sizeof (void (*)(void)), 1 },
#endif

  /* Standard types.  */
  { CFI_type_signed_char, "CFI_type_signed_char",
    sizeof (signed char), 0, },
  { CFI_type_short, "CFI_type_short",
    sizeof (short), 0 },
  { CFI_type_int, "CFI_type_int",
    sizeof (int), 0 },
  { CFI_type_long, "CFI_type_long",
    sizeof (long), 0 },
  { CFI_type_long_long, "CFI_type_long_long",
    sizeof (long long), 0 },
  { CFI_type_size_t, "CFI_type_size_t",
    sizeof (size_t), 0 },
  { CFI_type_int8_t, "CFI_type_int8_t",
    sizeof (int8_t), 0 },
  { CFI_type_int16_t, "CFI_type_int16_t",
    sizeof (int16_t), 0 },
  { CFI_type_int32_t, "CFI_type_int32_t",
    sizeof (int32_t), 0 },
  { CFI_type_int64_t, "CFI_type_int64_t",
    sizeof (int64_t), 0 },
  { CFI_type_int_least8_t, "CFI_type_int_least8_t",
    sizeof (int_least8_t), 0 },
  { CFI_type_int_least16_t, "CFI_type_int_least16_t",
    sizeof (int_least16_t), 0 },
  { CFI_type_int_least32_t, "CFI_type_int_least32_t",
    sizeof (int_least32_t), 0 },
  { CFI_type_int_least64_t, "CFI_type_int_least64_t",
    sizeof (int_least64_t), 0 },
  { CFI_type_int_fast8_t, "CFI_type_int_fast8_t",
    sizeof (int_fast8_t), 0 },
  { CFI_type_int_fast16_t, "CFI_type_int_fast16_t",
    sizeof (int_fast16_t), 0 },
  { CFI_type_int_fast32_t, "CFI_type_int_fast32_t",
    sizeof (int_fast32_t), 0 },
  { CFI_type_int_fast64_t, "CFI_type_int_fast64_t",
    sizeof (int_fast64_t), 0 },
  { CFI_type_intmax_t, "CFI_type_intmax_t",
    sizeof (intmax_t), 0 },
  { CFI_type_intptr_t, "CFI_type_intptr_t",
    sizeof (intptr_t), 0 },
  { CFI_type_ptrdiff_t, "CFI_type_ptrdiff_t",
    sizeof (ptrdiff_t), 0 },
  { CFI_type_float, "CFI_type_float",
    sizeof (float), 0 },
  { CFI_type_double, "CFI_type_double",
    sizeof (double), 0 },
  { CFI_type_long_double, "CFI_type_long_double",
    sizeof (long double), 0 },
  { CFI_type_float_Complex, "CFI_type_float_Complex",
    sizeof (float _Complex), 0 },
  { CFI_type_double_Complex, "CFI_type_double_Complex",
    sizeof (double _Complex), 0 },
  { CFI_type_long_double_Complex, "CFI_type_long_double_Complex",
    sizeof (long double _Complex), 0 },
  { CFI_type_Bool, "CFI_type_Bool",
    sizeof (_Bool), 0 },
  { CFI_type_char, "CFI_type_char",
    sizeof (char), 0 },
  { CFI_type_cptr, "CFI_type_cptr",
    sizeof (void *), 0 },
  { CFI_type_struct, "CFI_type_struct", 0, 0 },
  { CFI_type_other, "CFI_type_other", -1, 0, }
};

void
ctest_typecodes (void)
{
  int ncodes = sizeof (tc_table) / sizeof (struct tc_info);
  int i, j;
  int bad = 0;

  for (i = 0; i < ncodes; i++)
    for (j = i + 1; j < ncodes; j++)
      if (tc_table[i].typecode == tc_table[j].typecode
	  && tc_table[i].typecode > 0
	  && (tc_table[i].size != tc_table[j].size))
	{
	  fprintf (stderr,
		   "type codes have the same value %d but different sizes\n",
		   (int) tc_table[i].typecode);
	  fprintf (stderr, "  %s size %d\n",
		   tc_table[i].name, (int) tc_table[i].size);
	  fprintf (stderr, "  %s size %d\n",
		   tc_table[j].name, (int) tc_table[j].size);
	  bad = 1;
	}

  /* TS29113 Section 8.3.4: The value for CFI_type_other shall be negative
     and distinct from all other type specifiers.  If a C type is not
     interoperable with a Fortran type and kind supported by the
     Fortran processor, its macro shall evaluate to a negative value.
     Otherwise, the value for an intrinsic type shall be positive.

     In the case of GCC, we expect that all the standard intrinsic
     types are supported by both Fortran and C, so they should all be
     positive except for CFI_type_other.  Non-standard ones may have a
     value -2.  */
    
  for (i = 0; i < ncodes; i++)
    {
      if (tc_table[i].typecode == CFI_type_other)
	{
	  if (tc_table[i].typecode >= 0)
	    {
	      fprintf (stderr, "%s value %d is not negative\n",
		       tc_table[i].name, (int)tc_table[i].typecode);
	      bad = 1;
	    }
	  if (strcmp (tc_table[i].name, "CFI_type_other"))
	    {
	      fprintf (stderr, "%s has the same value %d as CFI_type_other\n",
		       tc_table[i].name, (int)CFI_type_other);
	      bad = 1;
	    }
	}
      else if (tc_table[i].typecode == -2 && tc_table[i].extension)
	/* Unsupported extension type on this target; this is OK  */
	;
      else if (tc_table[i].typecode <= 0)
	{
	  fprintf (stderr, "%s value %d is not positive\n",
		   tc_table[i].name, (int)tc_table[i].typecode);
	  bad = 1;
	}
    }

  if (bad)
    abort ();
}
