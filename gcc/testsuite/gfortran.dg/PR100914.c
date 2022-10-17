/* Test the fix for PR100914 */

#include <assert.h>
#include <complex.h>
#include <stdbool.h>
#include <stdio.h>
#include <math.h>

#include <ISO_Fortran_binding.h>

#define _CFI_type_mask 0xFF
#define _CFI_type_kind_shift 8

#define _CFI_decode_type(NAME) (signed char)((NAME) & CFI_type_mask)
#define _CFI_decode_kind(NAME) (signed char)(((NAME) >> CFI_type_kind_shift) & CFI_type_mask)

#define _CFI_encode_type(TYPE, KIND) (int16_t)\
((((KIND) & CFI_type_mask) << CFI_type_kind_shift)\
 | ((TYPE) & CFI_type_mask))

#undef CMPLXF
#define CMPLXF(x, y) ((float complex)((float)(x) + I * (float)(y)))

#undef CMPLX
#define CMPLX(x, y) ((double complex)((double)(x) + (double complex)I * (double)(y)))

#undef CMPLXL
#define CMPLXL(x, y) ((long double complex)((long double)(x) + (long double complex)I * (long double)(y)))

#undef CMPLX
#define CMPLX(x, y) ((_Float128 _Complex )((double)(x) + (double complex)I * (double)(y)))

#define N 11
#define M 7

typedef float _Complex c_float_complex;
typedef double _Complex c_double_complex;
typedef long double _Complex c_long_double_complex;
typedef _Float128 _Complex c_float128_complex;

bool c_vrfy_c_float_complex (const CFI_cdesc_t *restrict);

bool c_vrfy_c_double_complex (const CFI_cdesc_t *restrict);

bool c_vrfy_c_long_double_complex (const CFI_cdesc_t *restrict);

bool c_vrfy_c_float128_complex (const CFI_cdesc_t *restrict);

bool c_vrfy_complex (const CFI_cdesc_t *restrict);
 
bool c_vrfy_desc (const CFI_cdesc_t*restrict, const CFI_type_t, const signed char, const size_t, const size_t);

void check_tk (const CFI_cdesc_t*restrict, const CFI_type_t, const signed char, const size_t, const size_t);
 


bool
c_vrfy_c_float_complex (const CFI_cdesc_t *restrict auxp)
{
  CFI_index_t i, lb, ub, ex;
  size_t sz;
  c_float_complex *ip = NULL;

  assert (auxp);
  assert (auxp->base_addr);
  assert (auxp->elem_len>0);
  lb = auxp->dim[0].lower_bound;
  ex = auxp->dim[0].extent;
  assert (ex==11);
  sz = (size_t)auxp->elem_len / sizeof (c_float_complex);
  assert (sz==1);
  ub = ex + lb - 1;
  ip = (c_float_complex*)auxp->base_addr;
  for (i=0; i<ex; i++, ip+=sz)
    if ((cabsf (*ip-(c_float_complex)(CMPLXF((i+1), (2*(i+1)))))>(float)0.0))
      return false;
  for (i=lb; i<ub+1; i++)
    {
      ip = (c_float_complex*)CFI_address(auxp, &i);
      if ((cabsf (*ip-(c_float_complex)(CMPLXF((i-lb+1), (2*(i-lb+1)))))>(float)0.0))
	return false;
    }
  return true;
}

bool
c_vrfy_c_double_complex (const CFI_cdesc_t *restrict auxp)
{
  CFI_index_t i, lb, ub, ex;
  size_t sz;
  c_double_complex *ip = NULL;

  assert (auxp);
  assert (auxp->base_addr);
  assert (auxp->elem_len>0);
  lb = auxp->dim[0].lower_bound;
  ex = auxp->dim[0].extent;
  assert (ex==11);
  sz = (size_t)auxp->elem_len / sizeof (c_double_complex);
  assert (sz==1);
  ub = ex + lb - 1;
  ip = (c_double_complex*)auxp->base_addr;
  for (i=0; i<ex; i++, ip+=sz)
    if ((cabs (*ip-(c_double_complex)(CMPLX((i+1), (2*(i+1)))))>(double)0.0))
      return false;
  for (i=lb; i<ub+1; i++)
    {
      ip = (c_double_complex*)CFI_address(auxp, &i);
      if ((cabs (*ip-(c_double_complex)(CMPLX((i-lb+1), (2*(i-lb+1)))))>(double)0.0))
	return false;
    }
  return true;
}

bool
c_vrfy_c_long_double_complex (const CFI_cdesc_t *restrict auxp)
{
  CFI_index_t i, lb, ub, ex;
  size_t sz;
  c_long_double_complex *ip = NULL;

  assert (auxp);
  assert (auxp->base_addr);
  assert (auxp->elem_len>0);
  lb = auxp->dim[0].lower_bound;
  ex = auxp->dim[0].extent;
  assert (ex==11);
  sz = (size_t)auxp->elem_len / sizeof (c_long_double_complex);
  assert (sz==1);
  ub = ex + lb - 1;
  ip = (c_long_double_complex*)auxp->base_addr;
  for (i=0; i<ex; i++, ip+=sz)
    if ((cabsl (*ip-(c_long_double_complex)(CMPLXL((i+1), (2*(i+1)))))>(long double)0.0))
      return false;
  for (i=lb; i<ub+1; i++)
    {
      ip = (c_long_double_complex*)CFI_address(auxp, &i);
      if ((cabsl (*ip-(c_long_double_complex)(CMPLXL((i-lb+1), (2*(i-lb+1)))))>(long double)0.0))
	return false;
    }
  return true;
}

bool
c_vrfy_c_float128_complex (const CFI_cdesc_t *restrict auxp)
{
  CFI_index_t i, lb, ub, ex;
  size_t sz;
  c_float128_complex *ip = NULL;

  assert (auxp);
  assert (auxp->base_addr);
  assert (auxp->elem_len>0);
  lb = auxp->dim[0].lower_bound;
  ex = auxp->dim[0].extent;
  assert (ex==11);
  sz = (size_t)auxp->elem_len / sizeof (c_float128_complex);
  assert (sz==1);
  ub = ex + lb - 1;
  ip = (c_float128_complex*)auxp->base_addr;
  for (i=0; i<ex; i++, ip+=sz)
    if ((cabs ((double complex)(*ip-(c_float128_complex)(CMPLX((i+1), (2*(i+1))))))>(double)0.0))
      return false;
  for (i=lb; i<ub+1; i++)
    {
      ip = (c_float128_complex*)CFI_address(auxp, &i);
      if ((cabs ((double complex)(*ip-(c_float128_complex)(CMPLX((i-lb+1), (2*(i-lb+1))))))>(double)0.0))
	return false;
    }
  return true;
}

bool
c_vrfy_complex (const CFI_cdesc_t *restrict auxp)
{
  signed char type, kind;
  
  assert (auxp);
  type = _CFI_decode_type(auxp->type);
  kind = _CFI_decode_kind(auxp->type);
  assert (type == CFI_type_Complex);
  switch (kind)
    {
    case 4:
      return c_vrfy_c_float_complex (auxp);
      break;
    case 8:
      return c_vrfy_c_double_complex (auxp);
      break;
    case 10:
      return c_vrfy_c_long_double_complex (auxp);
      break;
    case 16:
      return c_vrfy_c_float128_complex (auxp);
      break;
    default:
      assert (false);
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
  assert (ityp == CFI_type_Complex);
  iknd = _CFI_decode_kind(auxp->type);
  assert (_CFI_decode_type(type)==ityp);
  assert (kind==iknd);
  assert (c_vrfy_complex (auxp));
  return;
}

// Local Variables:
// mode: C
// End:
