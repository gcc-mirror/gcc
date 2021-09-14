/* Test the fix for PR100911 */

#include <assert.h>
#include <stdbool.h>
#include <stdio.h>

#include <ISO_Fortran_binding.h>

#define _CFI_type_mask 0xFF
#define _CFI_type_kind_shift 8

#define _CFI_decode_type(NAME) (signed char)((NAME) & CFI_type_mask)
#define _CFI_decode_kind(NAME) (signed char)(((NAME) >> CFI_type_kind_shift) & CFI_type_mask)

#define _CFI_encode_type(TYPE, KIND) (int16_t)\
((((KIND) & CFI_type_mask) << CFI_type_kind_shift)\
 | ((TYPE) & CFI_type_mask))

#define N 11
#define M 7

#define CFI_type_Cptr CFI_type_cptr

typedef int* c_ptr;

bool c_vrfy_cptr (const CFI_cdesc_t *restrict);
 
void check_tk (const CFI_cdesc_t*restrict, const CFI_type_t, const signed char, const size_t, const size_t);

bool
c_vrfy_cptr (const CFI_cdesc_t *restrict auxp)
{
  CFI_index_t i, lb, ub, ex;
  size_t sz;
  c_ptr *ip = NULL;

  assert (auxp);
  assert (auxp->base_addr);
  assert (auxp->elem_len>0);
  lb = auxp->dim[0].lower_bound;
  ex = auxp->dim[0].extent;
  assert (ex==11);
  sz = (size_t)auxp->elem_len / sizeof (c_ptr);
  assert (sz==1);
  ub = ex + lb - 1;
  ip = (c_ptr*)auxp->base_addr;
  for (i=0; i<ex; i++, ip+=sz)
    if ((**ip) != (int)(i+1))
      return false;
  for (i=lb; i<ub+1; i++)
    {
      ip = (c_ptr*)CFI_address(auxp, &i);
      if ((**ip) != (int)(i-lb+1))
	return false;
    }
  return true;
}

void
check_tk (const CFI_cdesc_t *restrict auxp, const CFI_type_t type, const signed char kind, const size_t elem_len, const size_t nelem)
{
  signed char ityp, iknd;

  assert (auxp);
  assert (auxp->elem_len==elem_len*nelem);
  assert (auxp->rank==1);
  assert (auxp->dim[0].sm>0);
  assert ((size_t)auxp->dim[0].sm==elem_len*nelem);
  /*  */
  assert (auxp->type==type);
  ityp = _CFI_decode_type(auxp->type);
  assert (ityp == CFI_type_cptr);
  iknd = _CFI_decode_kind(auxp->type);
  assert (_CFI_decode_type(type)==ityp);
  assert (kind==iknd);
  assert (c_vrfy_cptr (auxp));
  return;
}

// Local Variables:
// mode: C
// End:
