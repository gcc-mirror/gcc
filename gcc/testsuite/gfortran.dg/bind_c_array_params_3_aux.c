/* Used by bind_c_array_params_3.f90.  */
/* PR fortran/92284.  */

#include <assert.h>
#include <errno.h>
#include <stdio.h>

#include "../../../libgfortran/ISO_Fortran_binding.h"

void arr_set_c(CFI_cdesc_t*);

void arr_set_c(CFI_cdesc_t *arr){
  int i, stat, *auxp = NULL;
  CFI_index_t   lb[] = {1};
  CFI_index_t   ub[] = {9};
  
  assert(arr);
  assert(arr->rank==1);
  assert(!arr->base_addr);
  stat = CFI_allocate(arr, lb, ub, sizeof(int));
  assert(stat==CFI_SUCCESS);
  auxp = (int*)arr->base_addr;
  assert(auxp);
  for(i=0; i<ub[0]-lb[0]+1; i++) auxp[i]=i;
  return;
}
