#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

struct s {
  int i;
  double d;
};

static long buf[5][4][3];

/* External entry point.  */
extern void ctest (void);

void
ctest (void)
{
  int bad = 0;
  int status;
  CFI_CDESC_T(3) desc;
  CFI_cdesc_t *dv = (CFI_cdesc_t *) &desc;
  CFI_index_t ex[3], lb[3], ub[3];
  CFI_index_t sm;

  /* On entry, the base_addr member of the C descriptor shall be a null
     pointer.  */
  sm = sizeof (struct s);
  check_CFI_status ("CFI_establish",
		    CFI_establish (dv, NULL, CFI_attribute_allocatable,
				   CFI_type_struct, sm,
				   0, NULL));
  check_CFI_status ("CFI_allocate",
		    CFI_allocate (dv, NULL, NULL, 69));
  status = CFI_allocate (dv, NULL, NULL, 42);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for CFI_allocate of already-allocated object\n");
      bad ++;
    }
  check_CFI_status ("CFI_deallocate",
		    CFI_deallocate (dv));

  /* The attribute member of the C descriptor shall have a value of
     CFI_attribute_allocatable or CFI_attribute_pointer.  */
  ex[0] = 3;
  ex[1] = 4;
  ex[2] = 5;
  check_CFI_status ("CFI_establish",
		    CFI_establish (dv, NULL, CFI_attribute_other,
				   CFI_type_long, 0, 3, ex));
  lb[0] = 1;
  lb[1] = 2;
  lb[2] = 3;
  ub[0] = 10;
  ub[1] = 5;
  ub[2] = 10;
  sm = sizeof (long);
  status = CFI_allocate (dv, lb, ub, 20);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for CFI_allocate of CFI_attribute_other object\n");
      bad ++;
    }

  /* dv shall be the address of a C descriptor describing the object.
     It shall have been allocated using the same mechanism as the
     Fortran ALLOCATE statement.  */
  ex[0] = 3;
  ex[1] = 4;
  ex[2] = 5;
  check_CFI_status ("CFI_establish",
		    CFI_establish (dv, NULL, CFI_attribute_pointer,
				   CFI_type_long, 0, 3, ex));
  status = CFI_deallocate (dv);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for CFI_deallocate with null pointer\n");
      bad ++;
    }

  /* This variant is disabled.  In theory it should be possible for
     the memory allocator to easily check for pointers outside the
     heap region, but libfortran just calls free() which has no provision
     for returning an error, and there is no other standard C interface
     to check the validity of a pointer in the C heap either.  */
#if 0  
  check_CFI_status ("CFI_establish",
		    CFI_establish (dv, buf, CFI_attribute_pointer,
				   CFI_type_long, 0, 3, ex));
  status = CFI_deallocate (dv);
  if (status == CFI_SUCCESS)
    {
      fprintf (stderr,
	       "no error for CFI_deallocate with non-allocated pointer\n");
      bad ++;
    }
#endif

  if (bad)
    abort ();
}

