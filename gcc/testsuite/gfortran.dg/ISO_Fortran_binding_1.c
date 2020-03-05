/* Test F2008 18.5: ISO_Fortran_binding.h functions.  */

#include "../../../libgfortran/ISO_Fortran_binding.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <complex.h>

/* Test the example in F2008 C.12.9: Processing assumed-shape arrays in C,
   modified to use CFI_address instead of pointer arithmetic.  */

int elemental_mult_c(CFI_cdesc_t * a_desc, CFI_cdesc_t * b_desc,
		     CFI_cdesc_t * c_desc)
{
  CFI_index_t idx[2];
  int *res_addr;
  int err = 1; /* this error code represents all errors */

  if (a_desc->rank == 0)
    {
      err = *(int*)a_desc->base_addr;
      *(int*)a_desc->base_addr = 0;
      return err;
    }

  if (a_desc->type != CFI_type_int
      || b_desc->type != CFI_type_int
      || c_desc->type != CFI_type_int)
    return err;

  /* Only support two dimensions. */
  if (a_desc->rank != 2
      || b_desc->rank != 2
      || c_desc->rank != 2)
    return err;

  if (a_desc->attribute == CFI_attribute_other)
    {
      assert (a_desc->dim[0].lower_bound == 0);
      assert (a_desc->dim[1].lower_bound == 0);
      for (idx[0] = 0; idx[0] < a_desc->dim[0].extent; idx[0]++)
	for (idx[1] = 0; idx[1] < a_desc->dim[1].extent; idx[1]++)
	  {
	    res_addr = CFI_address (a_desc, idx);
	    *res_addr = *(int*)CFI_address (b_desc, idx)
			* *(int*)CFI_address (c_desc, idx);
	  }
    }
  else
    {
      assert (a_desc->attribute == CFI_attribute_allocatable
	      || a_desc->attribute == CFI_attribute_pointer);
      for (idx[0] = a_desc->dim[0].lower_bound;
	   idx[0] < a_desc->dim[0].extent + a_desc->dim[0].lower_bound;
	   idx[0]++)
	for (idx[1] = a_desc->dim[1].lower_bound;
	     idx[1] < a_desc->dim[1].extent + a_desc->dim[1].lower_bound;
	     idx[1]++)
	  {
	    res_addr = CFI_address (a_desc, idx);
	    *res_addr = *(int*)CFI_address (b_desc, idx)
			* *(int*)CFI_address (c_desc, idx);
	  }
    }

  return 0;
}


int deallocate_c(CFI_cdesc_t * dd)
{
  return CFI_deallocate(dd);
}


int allocate_c(CFI_cdesc_t * da, CFI_index_t lower[], CFI_index_t upper[])
{
  int err = 1;
  CFI_index_t idx[2];
  int *res_addr;

  if (da->attribute == CFI_attribute_other) return err;
  if (CFI_allocate(da, lower, upper, 0)) return err;
  assert (da->dim[0].lower_bound == lower[0]);
  assert (da->dim[1].lower_bound == lower[1]);

  for (idx[0] = lower[0]; idx[0] < da->dim[0].extent + lower[0]; idx[0]++)
    for (idx[1] = lower[1]; idx[1] < da->dim[1].extent + lower[1]; idx[1]++)
      {
	res_addr = CFI_address (da, idx);
	*res_addr = (int)(idx[0] * idx[1]);
      }

  return 0;
}

int establish_c(CFI_cdesc_t * desc)
{
  typedef struct {double x; double _Complex y;} t;
  int err;
  CFI_index_t idx[1], extent[1];
  t *res_addr;
  double value = 1.0;
  double complex z_value = 0.0 + 2.0 * I;

  extent[0] = 10;
  err = CFI_establish((CFI_cdesc_t *)desc,
		      malloc ((size_t)(extent[0] * sizeof(t))),
		      CFI_attribute_pointer,
		      CFI_type_struct,
		      sizeof(t), 1, extent);
  assert (desc->dim[0].lower_bound == 0);
  for (idx[0] = 0; idx[0] < extent[0]; idx[0]++)
    {
      res_addr = (t*)CFI_address (desc, idx);
      res_addr->x = value++;
      res_addr->y = z_value * (idx[0] + 1);
    }
  return err;
}

int contiguous_c(CFI_cdesc_t * desc)
{
  return CFI_is_contiguous(desc);
}

float section_c(int *std_case, CFI_cdesc_t * source, int *low, int *str)
{
  CFI_index_t idx[CFI_MAX_RANK], lower[CFI_MAX_RANK],
		  strides[CFI_MAX_RANK], upper[CFI_MAX_RANK];
  CFI_CDESC_T(1) section;
  int ind;
  float *ret_addr;
  float ans = 0.0;

  /* Case (i) from F2018:18.5.5.7. */
  if (*std_case == 1)
    {
      lower[0] = (CFI_index_t)low[0];
      strides[0] = (CFI_index_t)str[0];
      ind = CFI_establish((CFI_cdesc_t *)&section, NULL, CFI_attribute_other,
			  CFI_type_float, 0, 1, NULL);
      if (ind) return -1.0;
      ind = CFI_section((CFI_cdesc_t *)&section, source, lower, NULL, strides);
      assert (section.dim[0].lower_bound == lower[0]);
      if (ind) return -2.0;

      /* Sum over the section  */
      for (idx[0] = lower[0]; idx[0] < section.dim[0].extent + lower[0]; idx[0]++)
        ans += *(float*)CFI_address ((CFI_cdesc_t*)&section, idx);
      return ans;
    }
  else if (*std_case == 2)
    {
      int ind;
      lower[0] = source->dim[0].lower_bound;
      upper[0] = source->dim[0].lower_bound + source->dim[0].extent - 1;
      strides[0] = str[0];
      lower[1] = upper[1] = source->dim[1].lower_bound + low[1] - 1;
      strides[1] = 0;
      ind = CFI_establish((CFI_cdesc_t *)&section, NULL, CFI_attribute_other,
			  CFI_type_float, 0, 1, NULL);
      if (ind) return -1.0;
      ind = CFI_section((CFI_cdesc_t *)&section, source,
			lower, upper, strides);
      assert (section.rank == 1);
      assert (section.dim[0].lower_bound == lower[0]);
      if (ind) return -2.0;

      /* Sum over the section  */
      for (idx[0] = lower[0]; idx[0] < section.dim[0].extent + lower[0]; idx[0]++)
        ans += *(float*)CFI_address ((CFI_cdesc_t*)&section, idx);
      return ans;
    }

  return 0.0;
}


double select_part_c (CFI_cdesc_t * source)
{
  typedef struct {
    double x; double _Complex y;
    } t;
  CFI_CDESC_T(2) component;
  CFI_cdesc_t * comp_cdesc = (CFI_cdesc_t *)&component;
  CFI_index_t extent[] = {10,10};
  CFI_index_t idx[] = {4,0};
  double ans = 0.0;
  int size;

  (void)CFI_establish(comp_cdesc, NULL, CFI_attribute_other,
		      CFI_type_double_Complex, sizeof(double _Complex),
		      2, extent);
  (void)CFI_select_part(comp_cdesc, source, offsetof(t,y), 0);
  assert (comp_cdesc->dim[0].lower_bound == 0);
  assert (comp_cdesc->dim[1].lower_bound == 0);

  /* Sum over comp_cdesc[4,:]  */
  size = comp_cdesc->dim[1].extent;
  for (idx[1] = 0; idx[1] < size; idx[1]++)
    ans += cimag (*(double _Complex*)CFI_address ((CFI_cdesc_t*)comp_cdesc,
						  idx));
  return ans;
}


int setpointer_c(CFI_cdesc_t * ptr, int lbounds[])
{
  CFI_index_t lower_bounds[] = {lbounds[0],lbounds[1]};
  int ind;
  ind = CFI_setpointer(ptr, ptr, lower_bounds);
  return ind;
}


int assumed_size_c(CFI_cdesc_t * desc)
{
  int res;

  res = CFI_is_contiguous(desc);
  if (!res)
    return 1;
  if (desc->rank)
    res = 2 * (desc->dim[desc->rank-1].extent
				!= (CFI_index_t)(long long)(-1));
  else
    res = 3;
  return res;
}
