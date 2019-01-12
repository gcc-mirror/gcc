/* Test F2018 18.5: ISO_Fortran_binding.h functions.  */

#include <ISO_Fortran_binding.h>
#include <stdio.h>
#include <stdlib.h>
#include <complex.h>

/* Test the example in F2018 C.12.9: Processing assumed-shape arrays in C,
   modified to use CFI_address instead of pointer arithmetic.  */

int address_c(CFI_cdesc_t * a_desc, const int idx[])
{
  int *res_addr;
  CFI_index_t CFI_idx[1];

  CFI_idx[0] = (CFI_index_t)idx[0];

  res_addr = CFI_address (a_desc, CFI_idx);
  if (res_addr == NULL)
    return -1;
  return *res_addr;
}


int deallocate_c(CFI_cdesc_t * dd)
{
  return CFI_deallocate(dd);
}


int allocate_c(CFI_cdesc_t * da, CFI_index_t lower[], CFI_index_t upper[])
{
  return CFI_allocate(da, lower, upper, 0);
}

int establish_c(CFI_cdesc_t * desc, int *rank, int *attr)
{
  typedef struct {double x; double _Complex y;} t;
  int err;
  CFI_index_t idx[1], extent[1];
  void *ptr;

  extent[0] = 1;
  ptr = malloc ((size_t)(extent[0] * sizeof(t)));
  err = CFI_establish((CFI_cdesc_t *)desc,
		      ptr,
		      (CFI_attribute_t)*attr,
		      CFI_type_struct,
		      sizeof(t), (CFI_rank_t)*rank, extent);
  free (ptr);
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
  int ind, size;
  float *ret_addr;
  float ans = 0.0;

  if (*std_case == 1)
    {
      lower[0] = (CFI_index_t)low[0];
      strides[0] = (CFI_index_t)str[0];
      ind = CFI_establish((CFI_cdesc_t *)&section, NULL, CFI_attribute_other,
			  CFI_type_float, 0, 1, NULL);
      if (ind) return -1.0;
      ind = CFI_section((CFI_cdesc_t *)&section, source, lower, NULL, strides);
      if (ind) return (float)ind;
    }

  return 0.0;
}


int select_part_c (CFI_cdesc_t * source)
{
  typedef struct
  {
    double x;
    double _Complex y;
  } t;
  CFI_CDESC_T(2) component;
  CFI_cdesc_t * comp_cdesc = (CFI_cdesc_t *)&component;
  CFI_index_t extent[] = {10,10};
  CFI_index_t idx[] = {4,0};
  int res;

  res = CFI_establish(comp_cdesc, NULL, CFI_attribute_other,
		      CFI_type_double_Complex, sizeof(double _Complex),
		      2, extent);
  if (res)
    return res;

  res = CFI_select_part(comp_cdesc, source, offsetof(t,y), 0);

  return res;
}


int setpointer_c(CFI_cdesc_t * ptr1, CFI_cdesc_t * ptr2, int lbounds[])
{
  CFI_index_t lower_bounds[] = {lbounds[0],lbounds[1]};
  int ind;

  ind = CFI_setpointer(ptr1, ptr2, lower_bounds);
  return ind;
}
