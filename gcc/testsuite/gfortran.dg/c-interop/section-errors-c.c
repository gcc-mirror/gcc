#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

/* For simplicity, point descriptors at a static buffer.  */
#define BUFSIZE 256
static char *buf[BUFSIZE] __attribute__ ((aligned (8)));
static CFI_index_t extents[] = {10};

/* External entry point.  The arguments are descriptors for input arrays;
   we'll construct new descriptors for the outputs of CFI_section.  */
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
  CFI_index_t lb = 2;
  CFI_index_t ub = 8;
  CFI_index_t step = 2;
  CFI_index_t zstep = 0;

  /* Use a 1-d integer source array for the first few tests.  */
  check_CFI_status ("CFI_establish",
		    CFI_establish (source, (void *)buf, CFI_attribute_other,
				   CFI_type_int, 0, 1, extents));

  /* result shall be the address of a C descriptor with rank equal
     to the rank of source minus the number of zero strides.  */
  check_CFI_status ("CFI_establish", 
		    CFI_establish (result, NULL, CFI_attribute_pointer,
				   CFI_type_int, 0, 0, NULL));
  status = CFI_section (result, source, &lb, &ub, &step);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for rank mismatch (too small)\n");
      bad ++;
    }

  check_CFI_status ("CFI_establish", 
		    CFI_establish (result, NULL, CFI_attribute_pointer,
				   CFI_type_int, 0, 1, NULL));
  status = CFI_section (result, source, &lb, &lb, &zstep);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for rank mismatch (zero stride)\n");
      bad ++;
    }

  check_CFI_status ("CFI_establish", 
		    CFI_establish (result, NULL, CFI_attribute_pointer,
				   CFI_type_int, 0, 3, NULL));
  status = CFI_section (result, source, &lb, &ub, &step);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for rank mismatch (too large)\n");
      bad ++;
    }

  /* The attribute member [of result] shall have the value
     CFI_attribute_other or CFI_attribute_pointer.  */
  check_CFI_status ("CFI_establish", 
		    CFI_establish (result, NULL, CFI_attribute_allocatable,
				   CFI_type_int, 0, 1, NULL));
  status = CFI_section (result, source, &lb, &ub, &step);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for CFI_attribute_allocatable result\n");
      bad ++;
    }

  /* source shall be the address of a C descriptor that describes a
     nonallocatable nonpointer array, an allocated allocatable array,
     or an associated array pointer.  */
  check_CFI_status ("CFI_establish",
		    CFI_establish (source, NULL, CFI_attribute_allocatable,
				   CFI_type_int, 0, 1, NULL));
  check_CFI_status ("CFI_establish", 
		    CFI_establish (result, NULL, CFI_attribute_pointer,
				   CFI_type_int, 0, 1, NULL));
  status = CFI_section (result, source, &lb, &ub, &step);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for unallocated allocatable source array\n");
      bad ++;
    }

  check_CFI_status ("CFI_establish",
		    CFI_establish (source, NULL, CFI_attribute_pointer,
				   CFI_type_int, 0, 1, NULL));
  check_CFI_status ("CFI_establish", 
		    CFI_establish (result, NULL, CFI_attribute_pointer,
				   CFI_type_int, 0, 1, NULL));
  status = CFI_section (result, source, &lb, &ub, &step);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for unassociated pointer source array\n");
      bad ++;
    }

  /* The corresponding values of the elem_len and type members shall
     be the same in the C descriptors with the addresses source
     and result.  */
  check_CFI_status ("CFI_establish",
		    CFI_establish (source, (void *)buf, CFI_attribute_other,
				   CFI_type_struct,
				   sizeof(int), 1, extents));
  check_CFI_status ("CFI_establish", 
		    CFI_establish (result, NULL, CFI_attribute_pointer,
				   CFI_type_struct,
				   2*sizeof (int), 1, NULL));
  status = CFI_section (result, source, &lb, &ub, &step);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for elem_len mismatch\n");
      bad ++;
    }

  check_CFI_status ("CFI_establish", 
		    CFI_establish (result, NULL, CFI_attribute_pointer,
				   CFI_type_int, 0, 1, NULL));
  status = CFI_section (result, source, &lb, &ub, &step);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for type mismatch\n");
      bad ++;
    }

  if (bad)
    abort ();
}

