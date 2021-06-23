/* Test the fix for PR93524, in which CFI_allocate was computing
   sm incorrectly for dimensions > 2.  */

#include <stdlib.h>  // For size_t
#include "../../../libgfortran/ISO_Fortran_binding.h"

void my_fortran_sub_1 (CFI_cdesc_t *dv); 
void my_fortran_sub_2 (CFI_cdesc_t *dv); 

int main ()
{
  CFI_CDESC_T (3) a;
  CFI_cdesc_t *dv = (CFI_cdesc_t *) &a;
  // dv, base_addr, attribute,            type, elem_len, rank, extents
  CFI_establish (dv, NULL, CFI_attribute_allocatable, CFI_type_float, 0, 3, NULL); 

  if (dv->base_addr != NULL)
    return 1;  // shall not be allocated

  CFI_index_t lower_bounds[] = {-10, 0, 3}; 
  CFI_index_t upper_bounds[] = {10, 5, 10}; 
  size_t elem_len = 0;  // only needed for strings
  if (CFI_SUCCESS != CFI_allocate (dv, lower_bounds, upper_bounds, elem_len))
    return 2;

  if (!CFI_is_contiguous (dv))
    return 2;  // allocatables shall be contiguous,unless a strided section is used

  my_fortran_sub_1 (dv);
  my_fortran_sub_2 (dv);
  CFI_deallocate (dv);
  return 0;
}
