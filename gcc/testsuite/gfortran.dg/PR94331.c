/* Test the fix for PR94331.  */

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>

#include <ISO_Fortran_binding.h>

bool c_vrfy (const CFI_cdesc_t *restrict);

bool check_bounds(const CFI_cdesc_t*restrict, const int, const int);

bool
c_vrfy (const CFI_cdesc_t *restrict auxp)
{
  CFI_index_t i, lb, ub, ex;
  int *ip = NULL;

  assert (auxp);
  assert (auxp->base_addr);
  lb = auxp->dim[0].lower_bound;
  ex = auxp->dim[0].extent;
  ub = ex + lb - 1;
  ip = (int*)auxp->base_addr;
  for (i=0; i<ex; i++)
    if (*ip++ != i+1)
      return false;
  for (i=lb; i<ub+1; i++)
    {
      ip = (int*)CFI_address(auxp, &i);
      if (*ip != i-lb+1)
	return false;
    }
  return true;
}

bool
check_bounds (const CFI_cdesc_t *restrict auxp, const int lb, const int ub)
{
  CFI_index_t ex = ub-lb+1;
  size_t el;
  bool is_ok = false;
  
  assert (auxp);
  el = auxp->elem_len;
  assert (auxp->rank==1);
  assert (auxp->type==CFI_type_int);
  assert (auxp->dim[0].sm>0);
  assert ((size_t)auxp->dim[0].sm==el);
  if (auxp->dim[0].extent==ex
      && auxp->dim[0].lower_bound==lb)
    {
    switch(auxp->attribute)
      {
      case CFI_attribute_pointer:
      case CFI_attribute_allocatable:
	if (!c_vrfy (auxp))
	  break;
	is_ok = true;
	break;
      case CFI_attribute_other:
	if (!c_vrfy (auxp))
	  break;
	is_ok = (lb==0);
	break;
      default:
	assert (false);
	break;
      }
    }
  return is_ok;
}

