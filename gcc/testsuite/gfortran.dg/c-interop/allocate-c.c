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

/* External entry point.  */
extern void ctest (void);

void
ctest (void)
{
  CFI_CDESC_T(3) desc;
  CFI_cdesc_t *dv = (CFI_cdesc_t *) &desc;
  CFI_index_t ex[3], lb[3], ub[3];
  CFI_index_t sm;
  int i;

  /* Allocate and deallocate a scalar.  */
  sm = sizeof (struct s);
  check_CFI_status ("CFI_establish",
		    CFI_establish (dv, NULL, CFI_attribute_allocatable,
				   CFI_type_struct, sm,
				   0, NULL));
  check_CFI_status ("CFI_allocate",
		    CFI_allocate (dv, NULL, NULL, 69));
  dump_CFI_cdesc_t (dv);
  if (dv->base_addr == NULL)
    abort ();
  /* The elem_len argument only overrides the initial value in the
     descriptor for character types.  */
  if (dv->elem_len != sm)
    abort ();
  check_CFI_status ("CFI_deallocate",
		    CFI_deallocate (dv));
  /* The base_addr member of the C descriptor becomes a null pointer.  */
  if (dv->base_addr != NULL)
    abort ();

  /* Try an array.  We are going to test the requirement that:
       The supplied lower and upper bounds override any current 
       dimension information in the C descriptor.
     so we'll stuff different values in the descriptor to start with.  */
  ex[0] = 3;
  ex[1] = 4;
  ex[2] = 5;
  check_CFI_status ("CFI_establish",
		    CFI_establish (dv, NULL, CFI_attribute_pointer,
				   CFI_type_double, 0, 3, ex));
  lb[0] = 1;
  lb[1] = 2;
  lb[2] = 3;
  ub[0] = 10;
  ub[1] = 5;
  ub[2] = 10;
  sm = sizeof (double);
  check_CFI_status ("CFI_allocate",
		    CFI_allocate (dv, lb, ub, 20));
  dump_CFI_cdesc_t (dv);
  if (dv->base_addr == NULL)
    abort ();
  /* The element sizes passed to both CFI_establish and CFI_allocate should
     have been ignored in favor of using the constant size of the type.  */
  if (dv->elem_len != sm)
    abort ();

  /* Check extents and strides; we expect the allocated array to
     be contiguous so the stride computation should be straightforward
     no matter what the lower bound is.  */
  for (i = 0; i < 3; i++)
    {
      CFI_index_t extent = ub[i] - lb[i] + 1;
      if (dv->dim[i].lower_bound != lb[i])
	abort ();
      if (dv->dim[i].extent != extent)
	abort ();
      /* pr93524 */
      if (dv->dim[i].sm != sm)
	abort ();
      sm *= extent;
    }
  check_CFI_status ("CFI_deallocate",
		    CFI_deallocate (dv));
  if (dv->base_addr != NULL)
    abort ();

  /* Similarly for a character array, except that we expect the
     elem_len provided to CFI_allocate to prevail.  We set the elem_len
     to the same size as the array element in the previous example, so
     the bounds and strides should all be the same.  */
  ex[0] = 3;
  ex[1] = 4;
  ex[2] = 5;
  check_CFI_status ("CFI_establish",
		    CFI_establish (dv, NULL, CFI_attribute_allocatable,
				   CFI_type_char, 4, 3, ex));
  lb[0] = 1;
  lb[1] = 2;
  lb[2] = 3;
  ub[0] = 10;
  ub[1] = 5;
  ub[2] = 10;
  sm = sizeof (double);
  check_CFI_status ("CFI_allocate",
		    CFI_allocate (dv, lb, ub, sm));
  dump_CFI_cdesc_t (dv);
  if (dv->base_addr == NULL)
    abort ();
  if (dv->elem_len != sm)
    abort ();

  /* Check extents and strides; we expect the allocated array to
     be contiguous so the stride computation should be straightforward
     no matter what the lower bound is.  */
  for (i = 0; i < 3; i++)
    {
      CFI_index_t extent = ub[i] - lb[i] + 1;
      if (dv->dim[i].lower_bound != lb[i])
	abort ();
      if (dv->dim[i].extent != extent)
	abort ();
      /* pr93524 */
      if (dv->dim[i].sm != sm)
	abort ();
      sm *= extent;
    }
  check_CFI_status ("CFI_deallocate",
		    CFI_deallocate (dv));
  if (dv->base_addr != NULL)
    abort ();

  /* Signed char is not a Fortran character type.  Here we expect it to
     ignore the elem_len argument and use the size of the type.  */
  ex[0] = 3;
  ex[1] = 4;
  ex[2] = 5;
  check_CFI_status ("CFI_establish",
		    CFI_establish (dv, NULL, CFI_attribute_allocatable,
				   CFI_type_signed_char, 4, 3, ex));
  lb[0] = 1;
  lb[1] = 2;
  lb[2] = 3;
  ub[0] = 10;
  ub[1] = 5;
  ub[2] = 10;
  sm = sizeof (double);
  check_CFI_status ("CFI_allocate",
		    CFI_allocate (dv, lb, ub, sm));
  dump_CFI_cdesc_t (dv);
  if (dv->base_addr == NULL)
    abort ();
  if (dv->elem_len != sizeof (signed char))
    abort ();

  check_CFI_status ("CFI_deallocate",
		    CFI_deallocate (dv));
  if (dv->base_addr != NULL)
    abort ();

}

