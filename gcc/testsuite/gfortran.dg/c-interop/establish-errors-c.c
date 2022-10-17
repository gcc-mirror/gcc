#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

/* For simplicity, point descriptors at a static buffer.  BUFSIZE should
   be large enough for any of the standard types and we'll use DIM0 and DIM1
   for array dimensions.  */
#define BUFSIZE 64
#define DIM0 3
#define DIM1 10
#define ARRAYBUFSIZE BUFSIZE * DIM0 * DIM1
static char *buf[ARRAYBUFSIZE] __attribute__ ((aligned (8)));
static CFI_index_t extents[] = {DIM0, DIM1};

/* Magic number to use for elem_len field.  */
#define MAGIC_ELEM_LEN 20


/* External entry point.  */
extern void ctest (void);

void
ctest (void)
{
  int bad = 0;
  int status;
  CFI_CDESC_T(2) desc;
  CFI_cdesc_t *a = (CFI_cdesc_t *) &desc;

  /* If the attribute argument is CFI_attribute_allocatable,
     base_addr shall be a null pointer.  */
  status = CFI_establish (a, (void *)buf, CFI_attribute_allocatable,
			  CFI_type_int, 0, 2, extents);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for non-null pointer with CFI_attribute_allocatable\n");
      bad ++;
    }

  /* type shall have the value of one of the type codes in Table 18.4,
     or have a positive value corresponding to an interoperable C type. */
  status = CFI_establish (a, (void *)buf, CFI_attribute_other,
			  CFI_type_other - 1, 0, 2, extents);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for invalid negative type code\n");
      bad ++;
    }

  /* If the type is CFI_type_struct, CFI_type_other, or a Fortran
     character type, elem_len shall be greater than zero and equal to
     the storage size in bytes of an element of the object.  */
  status = CFI_establish (a, (void *)buf, CFI_attribute_other,
			  CFI_type_struct, 0, 2, extents);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for invalid size with CFI_type_struct\n");
      bad ++;
    }
 
  status = CFI_establish (a, (void *)buf, CFI_attribute_other,
			  CFI_type_char, 0, 2, extents);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for invalid size with CFI_type_char\n");
      bad ++;
    }

  /* Rank shall be between 0 and CFI_MAX_RANK inclusive.  */
  status = CFI_establish (a, NULL, CFI_attribute_allocatable,
			  CFI_type_int, 0, -1, extents);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for negative rank\n");
      bad ++;
    }
  status = CFI_establish (a, NULL, CFI_attribute_allocatable,
			  CFI_type_int, 0, CFI_MAX_RANK + 1, extents);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for rank > CFI_MAX_RANK\n");
      bad ++;
    }

  /* extents is ignored if the rank r is zero or if base_addr is a
     null pointer. Otherwise, it shall be the address of an array...  */
  status = CFI_establish (a, (void *)buf, CFI_attribute_other,
			  CFI_type_int, 0, 2, NULL);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for null extents\n");
      bad ++;
    }

  /* Extents shall all be nonnegative.  */
  extents[1] = -extents[1];
  status = CFI_establish (a, (void *)buf, CFI_attribute_other,
			  CFI_type_int, 0, 2, extents);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for negative extents\n");
      bad ++;
    }

  if (bad)
    abort ();
}

