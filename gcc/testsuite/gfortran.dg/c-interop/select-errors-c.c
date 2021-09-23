#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

/* Source is an array of structs.  */
struct ss {
  int i, j;
  char c[16];
  double _Complex dc;
} s[10];

CFI_index_t extents[] = {10};

/* External entry point.  */
extern void ctest (void);

void
ctest (void)
{
  int bad = 0;
  int status;
  CFI_CDESC_T(1) sdesc;
  CFI_cdesc_t *source = (CFI_cdesc_t *) &sdesc;
  CFI_CDESC_T(3) rdesc;
  CFI_cdesc_t *result = (CFI_cdesc_t *) &rdesc;

  /* Create a descriptor for the source array.  */
  check_CFI_status ("CFI_establish",
		    CFI_establish (source, (void *)s, CFI_attribute_other,
				   CFI_type_struct,
				   sizeof (struct ss), 1, extents));

  /* The attribute member of result shall have the value
     CFI_attribute_other or CFI_attribute_pointer.  */
  check_CFI_status ("CFI_establish", 
		    CFI_establish (result, NULL, CFI_attribute_allocatable,
				   CFI_type_int, 0, 1, NULL));
  status = CFI_select_part (result, source, offsetof (struct ss, j), 0);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for CFI_attribute_allocatable result\n");
      bad ++;
    }

  /* The rank member of the result C descriptor shall have the same value
     as the rank member of the C descriptor at the address specified
     by source.  */
  check_CFI_status ("CFI_establish", 
		    CFI_establish (result, NULL, CFI_attribute_pointer,
				   CFI_type_int, 0, 0, NULL));
  status = CFI_select_part (result, source, offsetof (struct ss, j), 0);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for rank mismatch (too small)\n");
      bad ++;
    }

  check_CFI_status ("CFI_establish", 
		    CFI_establish (result, NULL, CFI_attribute_pointer,
				   CFI_type_int, 0, 3, NULL));
  status = CFI_select_part (result, source, offsetof (struct ss, j), 0);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for rank mismatch (too large)\n");
      bad ++;
    }

  /* The value of displacement shall be between 0 and source->elem_len - 1
     inclusive. */
  check_CFI_status ("CFI_establish", 
		    CFI_establish (result, NULL, CFI_attribute_pointer,
				   CFI_type_int, 0, 1, NULL));
  status = CFI_select_part (result, source, -8, 0);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for negative displacement\n");
      bad ++;
    }
  status = CFI_select_part (result, source, source->elem_len, 0);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for too-large displacement\n");
      bad ++;
    }

  /* source shall be the address of a C descriptor for a nonallocatable
     nonpointer array, an allocated allocatable array, or an associated
     array pointer.  */
  check_CFI_status ("CFI_establish",
		    CFI_establish (source, NULL, CFI_attribute_allocatable,
				   CFI_type_struct,
				   sizeof (struct ss), 1, NULL));
  status = CFI_select_part (result, source, offsetof (struct ss, j), 0);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for unallocated allocatable source array\n");
      bad ++;
    }

  check_CFI_status ("CFI_establish",
		    CFI_establish (source, NULL, CFI_attribute_pointer,
				   CFI_type_struct,
				   sizeof (struct ss), 1, NULL));
  status = CFI_select_part (result, source, offsetof (struct ss, j), 0);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for unassociated pointer source array\n");
      bad ++;
    }

  if (bad)
    abort ();
}

