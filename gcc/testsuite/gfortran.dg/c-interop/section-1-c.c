#include <stdlib.h>
#include <stdio.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

extern void ctest (CFI_cdesc_t *a, int lb, int ub, int s, CFI_cdesc_t *r);

/* Take a section of array A.  OFF is the start index of A on the Fortran
   side and the bounds LB and UB for the section to take are relative to 
   that base index.  Store the result in R, which is supposed to be a pointer
   array with lower bound 1.  */
   
void
ctest (CFI_cdesc_t *a, int lb, int ub, int s, CFI_cdesc_t *r)
{
  CFI_index_t lb_array[1], ub_array[1], s_array[1];
  CFI_index_t i, o;

  /* Dump the descriptor contents to test that we can access the fields
     correctly, etc.  */
  fprintf (stderr, "\n%s: lb=%d  ub=%d  s=%d\n",
	   (a->attribute == CFI_attribute_other) ? "non-pointer" : "pointer",
	   lb, ub, s);  
  dump_CFI_cdesc_t (a);
  dump_CFI_cdesc_t (r);

  /* Make sure we got a valid input descriptor.  */
  if (!a->base_addr)
    abort ();
  if (a->elem_len != sizeof(int))
    abort ();
  if (a->rank != 1)
    abort ();
  if (a->type != CFI_type_int)
    abort ();
  if (a->attribute == CFI_attribute_other)
    {
      if (a->dim[0].lower_bound != 0)
	abort ();
      /* Adjust the 1-based bounds.  */
      lb = lb - 1;
      ub = ub - 1;
    }
  /* For pointer arrays, the bounds use the same indexing as the lower
     bound in the array descriptor.  */

  /* Make sure we got a valid output descriptor.  */
  if (r->base_addr)
    abort ();
  if (r->elem_len != sizeof(int))
    abort ();
  if (r->rank != 1)
    abort ();
  if (r->type != CFI_type_int)
    abort ();
  if (r->attribute != CFI_attribute_pointer)
    abort ();

  /* Create an array section.  */
  lb_array[0] = lb;
  ub_array[0] = ub;
  s_array[0] = s;

  check_CFI_status ("CFI_section",
		    CFI_section (r, a, lb_array, ub_array, s_array));

  /* Check that the output descriptor is correct.  */
  dump_CFI_cdesc_t (r);
  if (!r->base_addr)
    abort ();
  if (r->elem_len != sizeof(int))
    abort ();
  if (r->rank != 1)
    abort ();
  if (r->type != CFI_type_int)
    abort ();
  if (r->attribute != CFI_attribute_pointer)
    abort ();

  /* Check the contents of the output array.  */
#if 0
  for (o = r->dim[0].lower_bound, i = lb;
       (s > 0 ? i <= ub : i >= ub);
       o++, i += s)
    {
      int *input = (int *) CFI_address (a, &i);
      int *output = (int *) CFI_address (r, &o);
      fprintf (stderr, "a(%d) = %d, r(%d) = %d\n",
	       (int)i, *input, (int)o, *output);
    }
#endif
  for (o = r->dim[0].lower_bound, i = lb;
       (s > 0 ? i <= ub : i >= ub);
       o++, i += s)
    {
      int *input = (int *) CFI_address (a, &i);
      int *output = (int *) CFI_address (r, &o);
      if (*input != *output)
	abort ();
    }

  /* Force the output array to be 1-based.  */
  lb_array[0] = 1;
  check_CFI_status ("CFI_setpointer", CFI_setpointer (r, r, lb_array));
  /* Check that the output descriptor is correct.  */
  dump_CFI_cdesc_t (r);
  if (!r->base_addr)
    abort ();
  if (r->elem_len != sizeof(int))
    abort ();
  if (r->rank != 1)
    abort ();
  if (r->type != CFI_type_int)
    abort ();
  if (r->attribute != CFI_attribute_pointer)
    abort ();
  if (r->dim[0].lower_bound != 1)
    abort ();

  /* Check the contents of the output array again.  */
  for (o = r->dim[0].lower_bound, i = lb;
       (s > 0 ? i <= ub : i >= ub);
       o++, i += s)
    {
      int *input = (int *) CFI_address (a, &i);
      int *output = (int *) CFI_address (r, &o);
      if (*input != *output)
	abort ();
    }

}



