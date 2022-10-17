#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

/* Declare some source arrays.  */
struct ss {
  char c[4];
  signed char b[4];
  int i, j, k;
} s[10][5][3];

char c[10][16];

double _Complex dc[10];

CFI_index_t extents3[] = {3,5,10};
CFI_index_t extents1[] = {10};

/* External entry point.  */
extern void ctest (void);

void
ctest (void)
{
  CFI_CDESC_T(3) sdesc;
  CFI_cdesc_t *source = (CFI_cdesc_t *) &sdesc;
  CFI_CDESC_T(3) rdesc;
  CFI_cdesc_t *result = (CFI_cdesc_t *) &rdesc;
  size_t offset;

  /* Extract an array of structure elements.  */
  offset = offsetof (struct ss, j);
  check_CFI_status ("CFI_establish",
		    CFI_establish (source, (void *)s, CFI_attribute_other,
				   CFI_type_struct,
				   sizeof (struct ss), 3, extents3));
  check_CFI_status ("CFI_establish", 
		    CFI_establish (result, NULL, CFI_attribute_pointer,
				   CFI_type_int, 0, 3, NULL));
  check_CFI_status ("CFI_select_part",
		    CFI_select_part (result, source, offset, 0));
  dump_CFI_cdesc_t (source);
  dump_CFI_cdesc_t (result);

  if (result->elem_len != sizeof (int))
    abort ();
  if (result->base_addr != source->base_addr + offset)
    abort ();
  if (result->dim[0].extent != source->dim[0].extent)
    abort ();
  if (result->dim[0].sm != source->dim[0].sm)
    abort ();
  if (result->dim[1].extent != source->dim[1].extent)
    abort ();
  if (result->dim[1].sm != source->dim[1].sm)
    abort ();
  if (result->dim[2].extent != source->dim[2].extent)
    abort ();
  if (result->dim[2].sm != source->dim[2].sm)
    abort ();

  /* Check that we use the given elem_size for char but not for
     signed char, which is considered an integer type instead of a Fortran
     character type.  */
  check_CFI_status ("CFI_establish", 
		    CFI_establish (result, NULL, CFI_attribute_pointer,
				   CFI_type_char, 4, 3, NULL));
  if (result->elem_len != 4)
    abort ();
  offset = offsetof (struct ss, c);
  check_CFI_status ("CFI_select_part",
		    CFI_select_part (result, source, offset, 4));
  if (result->elem_len != 4)
    abort ();

  check_CFI_status ("CFI_establish", 
		    CFI_establish (result, NULL, CFI_attribute_pointer,
				   CFI_type_signed_char, 4, 3, NULL));
  if (result->elem_len != sizeof (signed char))
    abort ();
  offset = offsetof (struct ss, c);
  check_CFI_status ("CFI_select_part",
		    CFI_select_part (result, source, offset, 4));
  if (result->elem_len != sizeof (signed char))
    abort ();

  /* Extract an array of character substrings.  */
  offset = 2;
  check_CFI_status ("CFI_establish",
		    CFI_establish (source, (void *)c, CFI_attribute_other,
				   CFI_type_char, 16, 1, extents1));
  check_CFI_status ("CFI_establish", 
		    CFI_establish (result, NULL, CFI_attribute_pointer,
				   CFI_type_char, 8, 1, NULL));
  check_CFI_status ("CFI_select_part",
		    CFI_select_part (result, source, offset, 8));
  dump_CFI_cdesc_t (source);
  dump_CFI_cdesc_t (result);

  if (result->elem_len != 8)
    abort ();
  if (result->base_addr != source->base_addr + offset)
    abort ();
  if (result->dim[0].extent != source->dim[0].extent)
    abort ();
  if (result->dim[0].sm != source->dim[0].sm)
    abort ();

  /* Extract an array the imaginary parts of complex numbers.
     Note that the use of __imag__ to obtain the imaginary part as
     an lvalue is a GCC extension.  */
  offset = (void *)&(__imag__ dc[0]) - (void *)&(dc[0]);
  check_CFI_status ("CFI_establish",
		    CFI_establish (source, (void *)dc, CFI_attribute_other,
				   CFI_type_double_Complex,
				   0, 1, extents1));
  check_CFI_status ("CFI_establish", 
		    CFI_establish (result, NULL, CFI_attribute_pointer,
				   CFI_type_double, 0, 1, NULL));
  check_CFI_status ("CFI_select_part",
		    CFI_select_part (result, source, offset, 0));
  dump_CFI_cdesc_t (source);
  dump_CFI_cdesc_t (result);

  if (result->elem_len != sizeof (double))
    abort ();
  if (result->base_addr != source->base_addr + offset)
    abort ();
  if (result->dim[0].extent != source->dim[0].extent)
    abort ();
  if (result->dim[0].sm != source->dim[0].sm)
    abort ();
}

