#include <stdlib.h>
#include <stdio.h>

#include <ISO_Fortran_binding.h>
#include "dump-descriptors.h"

struct m {
  int x, y;
};

extern void ctest (CFI_cdesc_t *a, int lb0, int lb1,
		   int ub0, int ub1, int s0, int s1, CFI_cdesc_t *r);

/* Take a section of array A.  OFF is the start index of A on the Fortran
   side and the bounds LB and UB for the section to take are relative to 
   that base index.  Store the result in R, which is supposed to be a pointer
   array with lower bound 1.  */
   
void
ctest (CFI_cdesc_t *a, int lb0, int lb1,
		   int ub0, int ub1, int s0, int s1, CFI_cdesc_t *r)
{
  CFI_index_t lb_array[2], ub_array[2], s_array[2];
  int i0, i1, o0, o1;

  /* Dump the descriptor contents to test that we can access the fields
     correctly, etc.  */
  fprintf (stderr, "\n%s: lb0=%d  lb1=%d  ub0=%d  ub1=%d  s0=%d  s1=%d\n",
	   (a->attribute == CFI_attribute_other) ? "non-pointer" : "pointer",
	   lb0, lb1, ub0, ub1, s0, s1);  
  if (! (lb0 == ub0 || lb1 == ub1))
    abort ();
  dump_CFI_cdesc_t (a);
  dump_CFI_cdesc_t (r);

  /* Make sure we got a valid input descriptor.  */
  if (!a->base_addr)
    abort ();
  if (a->elem_len != sizeof(struct m))
    abort ();
  if (a->rank != 2)
    abort ();
  if (a->type != CFI_type_struct)
    abort ();
  if (a->attribute == CFI_attribute_other)
    {
      if (a->dim[0].lower_bound != 0)
	abort ();
      /* Adjust the 1-based bounds.  */
      lb0 = lb0 - 1;
      lb1 = lb1 - 1;
      ub0 = ub0 - 1;
      ub1 = ub1 - 1;
    }
  /* For pointer arrays, the bounds use the same indexing as the lower
     bound in the array descriptor.  */

  /* Make sure we got a valid output descriptor.  */
  if (r->base_addr)
    abort ();
  if (r->elem_len != sizeof(struct m))
    abort ();
  if (r->rank != 1)
    abort ();
  if (r->type != CFI_type_struct)
    abort ();
  if (r->attribute != CFI_attribute_pointer)
    abort ();

  /* Create an array section.  */
  lb_array[0] = lb0;
  lb_array[1] = lb1;
  ub_array[0] = ub0;
  ub_array[1] = ub1;
  s_array[0] = s0;
  s_array[1] = s1;

  check_CFI_status ("CFI_section",
		    CFI_section (r, a, lb_array, ub_array, s_array));

  /* Check that the output descriptor is correct.  */
  dump_CFI_cdesc_t (r);
  if (!r->base_addr)
    abort ();
  if (r->elem_len != sizeof(struct m))
    abort ();
  if (r->rank != 1)
    abort ();
  if (r->type != CFI_type_struct)
    abort ();
  if (r->attribute != CFI_attribute_pointer)
    abort ();

  /* Check the contents of the output array.  */
#if 0
  if (lb1 == ub1)
    {
      /* Output is 1-d array that varies in dimension 0.  */
      for (o0 = r->dim[0].lower_bound, i0 = lb0;
	   (s0 > 0 ? i0 <= ub0 : i0 >= ub0);
	   o0++, i0 += s0)
	{
	  CFI_index_t index[2];
	  struct m *input, *output;
	  index[0] = i0;
	  index[1] = lb1;
	  input = (struct m *) CFI_address (a, index);
	  index[0] = o0;
	  output = (struct m *) CFI_address (r, index);
	  fprintf (stderr, "a(%d,%d) = (%d,%d), r(%d) = (%d,%d)\n",
		   i0, lb1, input->x, input->y, o0, output->x, output->y);
	}
    }
  else if (lb0 == ub0)
    {
      /* Output is 1-d array that varies in dimension 1.  */
      for (o1 = r->dim[0].lower_bound, i1 = lb1;
	   (s1 > 0 ? i1 <= ub1 : i1 >= ub1);
	   o1++, i1 += s1)
	{
	  CFI_index_t index[2];
	  struct m *input, *output;
	  index[0] = lb0;
	  index[1] = i1;
	  input = (struct m *) CFI_address (a, index);
	  index[0] = o1;
	  output = (struct m *) CFI_address (r, index);
	  fprintf (stderr, "a(%d,%d) = (%d,%d), r(%d) = (%d,%d)\n",
		   lb0, i1, input->x, input->y, o1, output->x, output->y);
	}
    }
  else
    abort ();
#endif
  if (lb1 == ub1)
    {
      /* Output is 1-d array that varies in dimension 0.  */
      for (o0 = r->dim[0].lower_bound, i0 = lb0;
	   (s0 > 0 ? i0 <= ub0 : i0 >= ub0);
	   o0++, i0 += s0)
	{
	  CFI_index_t index[2];
	  struct m *input, *output;
	  index[0] = i0;
	  index[1] = lb1;
	  input = (struct m *) CFI_address (a, index);
	  index[0] = o0;
	  output = (struct m *) CFI_address (r, index);
	  if (input->x != output->x || input->y != output->y)
	    abort ();
	}
    }
  else if (lb0 == ub0)
    {
      /* Output is 1-d array that varies in dimension 1.  */
      for (o1 = r->dim[0].lower_bound, i1 = lb1;
	   (s1 > 0 ? i1 <= ub1 : i1 >= ub1);
	   o1++, i1 += s1)
	{
	  CFI_index_t index[2];
	  struct m *input, *output;
	  index[0] = lb0;
	  index[1] = i1;
	  input = (struct m *) CFI_address (a, index);
	  index[0] = o1;
	  output = (struct m *) CFI_address (r, index);
	  if (input->x != output->x || input->y != output->y)
	    abort ();
	}
    }
  else
    abort ();

  /* Force the output array to be 1-based.  */
  lb_array[0] = 1;
  lb_array[1] = 1;
  check_CFI_status ("CFI_setpointer", CFI_setpointer (r, r, lb_array));
  /* Check that the output descriptor is correct.  */
  dump_CFI_cdesc_t (r);
  if (!r->base_addr)
    abort ();
  if (r->elem_len != sizeof(struct m))
    abort ();
  if (r->rank != 1)
    abort ();
  if (r->type != CFI_type_struct)
    abort ();
  if (r->attribute != CFI_attribute_pointer)
    abort ();
  if (r->dim[0].lower_bound != 1)
    abort ();

  /* Check the contents of the output array again.  */
  if (lb1 == ub1)
    {
      /* Output is 1-d array that varies in dimension 0.  */
      for (o0 = r->dim[0].lower_bound, i0 = lb0;
	   (s0 > 0 ? i0 <= ub0 : i0 >= ub0);
	   o0++, i0 += s0)
	{
	  CFI_index_t index[2];
	  struct m *input, *output;
	  index[0] = i0;
	  index[1] = lb1;
	  input = (struct m *) CFI_address (a, index);
	  index[0] = o0;
	  output = (struct m *) CFI_address (r, index);
	  if (input->x != output->x || input->y != output->y)
	    abort ();
	}
    }
  else if (lb0 == ub0)
    {
      /* Output is 1-d array that varies in dimension 1.  */
      for (o1 = r->dim[0].lower_bound, i1 = lb1;
	   (s1 > 0 ? i1 <= ub1 : i1 >= ub1);
	   o1++, i1 += s1)
	{
	  CFI_index_t index[2];
	  struct m *input, *output;
	  index[0] = lb0;
	  index[1] = i1;
	  input = (struct m *) CFI_address (a, index);
	  index[0] = o1;
	  output = (struct m *) CFI_address (r, index);
	  if (input->x != output->x || input->y != output->y)
	    abort ();
	}
    }
  else
    abort ();
}



