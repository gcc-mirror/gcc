#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

static int a[10][5][3];
static CFI_index_t extents[] = {3, 5, 10};

/* External entry point.  */
extern void ctest (void);

void
ctest (void)
{
  int bad = 0;
  int status;
  CFI_CDESC_T(3) sdesc;
  CFI_cdesc_t *source = (CFI_cdesc_t *) &sdesc;
  CFI_CDESC_T(3) rdesc;
  CFI_cdesc_t *result = (CFI_cdesc_t *) &rdesc;

  /* result shall be the address of a C descriptor for a Fortran pointer.  */
  check_CFI_status ("CFI_establish",
		    CFI_establish (source, (void *)a, CFI_attribute_other,
				   CFI_type_int, 0, 3, extents));

  check_CFI_status ("CFI_establish", 
		    CFI_establish (result, NULL, CFI_attribute_allocatable,
				   CFI_type_int, 0, 3, NULL));
  status = CFI_setpointer (result, source, NULL);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for CFI_attribute_allocatable result\n");
      bad ++;
    }

  check_CFI_status ("CFI_establish", 
		    CFI_establish (result, NULL, CFI_attribute_other,
				   CFI_type_int, 0, 3, NULL));
  status = CFI_setpointer (result, source, NULL);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for CFI_attribute_other result\n");
      bad ++;
    }

  /* source shall be a null pointer or the address of a C descriptor
     for an allocated allocatable object, a data pointer object, or a
     nonallocatable nonpointer data object that is not an
     assumed-size array.  */
  check_CFI_status ("CFI_establish", 
		    CFI_establish (result, NULL, CFI_attribute_pointer,
				   CFI_type_int, 0, 3, NULL));

  check_CFI_status ("CFI_establish", 
		    CFI_establish (source, NULL, CFI_attribute_allocatable,
				   CFI_type_int, 0, 3, NULL));
  status = CFI_setpointer (result, source, NULL);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for unallocated allocatable source\n");
      bad ++;
    }

  /* CFI_establish rejects negative extents, so we can't use it to make
     an assumed-size array, so hack the descriptor by hand.  Yuck.  */
  check_CFI_status ("CFI_establish",
		    CFI_establish (source, (void *)a, CFI_attribute_other,
				   CFI_type_int, 0, 3, extents));
  source->dim[2].extent = -1;
  status = CFI_setpointer (result, source, NULL);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for assumed-size source array\n");
      bad ++;
    }
  
  /* If source is not a null pointer, the corresponding values of the
     elem_len, rank, and type members shall be the same in the C
     descriptors with the addresses source and result.  */
  check_CFI_status ("CFI_establish",
		    CFI_establish (source, (void *)a, CFI_attribute_other,
				   CFI_type_char, sizeof(int), 3, extents));
  check_CFI_status ("CFI_establish", 
		    CFI_establish (result, NULL, CFI_attribute_pointer,
				   CFI_type_char, 1, 3, NULL));
  status = CFI_setpointer (result, source, NULL);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for elem_len mismatch\n");
      bad ++;
    }

  check_CFI_status ("CFI_establish", 
		    CFI_establish (result, NULL, CFI_attribute_pointer,
				   CFI_type_char, sizeof(int), 1, NULL));
  status = CFI_setpointer (result, source, NULL);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for rank mismatch\n");
      bad ++;
    }

  check_CFI_status ("CFI_establish", 
		    CFI_establish (result, NULL, CFI_attribute_pointer,
				   CFI_type_int, 0, 3, NULL));
  status = CFI_setpointer (result, source, NULL);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for type mismatch\n");
      bad ++;
    }

  if (bad)
    abort ();
}

