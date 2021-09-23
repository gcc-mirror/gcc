/* Test the fix for PR94327.  */

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>

#include <ISO_Fortran_binding.h>

bool c_vrfy (const CFI_cdesc_t *restrict);

char get_attr (const CFI_cdesc_t*restrict, bool);

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

char
get_attr (const CFI_cdesc_t *restrict auxp, bool alloc)
{
  char attr;
  
  assert (auxp);
  assert (auxp->elem_len == 4);
  assert (auxp->rank == 1);
  assert (auxp->type == CFI_type_int);
  attr = '\0';
  switch (auxp->attribute)
    {
    case CFI_attribute_pointer:
      if (alloc && !c_vrfy (auxp))
	break;
      attr = 'p';
      break;
    case CFI_attribute_allocatable:
      if (alloc && !c_vrfy (auxp))
	break;
      attr = 'a';
      break;
    case CFI_attribute_other:
      assert (alloc);
      if (!c_vrfy (auxp))
	break;
      attr = 'o';
      break;
    default:
      break;
    }
  return attr;
}

