/* Test the fix for PR100906 */

#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
/* #include <uchar.h> */

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

typedef char c_char;
/* typedef char32_t c_ucs4_char; */
typedef uint32_t char32_t;
typedef uint32_t c_ucs4_char;
 
bool charcmp (char *, char, size_t);

bool ucharcmp (char32_t *, char32_t, size_t);

bool c_vrfy_c_char (const CFI_cdesc_t *restrict, const size_t);

bool c_vrfy_c_ucs4_char (const CFI_cdesc_t *restrict, const size_t);

bool c_vrfy_character (const CFI_cdesc_t *restrict, const size_t);
 
void check_tk (const CFI_cdesc_t*restrict, const CFI_type_t, const signed char, const size_t, const size_t);

bool
charcmp (char *c, char v, size_t n)
{
  bool res = true;
  char b = (char)'A';
  size_t i;

  for (i=0; ((i<n)&&(res)); i++, c++)
    res = (*c == (v+b));
  return res;
}

bool
ucharcmp (char32_t *c, char32_t v, size_t n)
{
  bool res = true;
  char32_t b = (char32_t)0xFF01;
  size_t i;

  for (i=0; ((i<n)&&(res)); i++, c++)
    res = (*c == (v+b));
  return res;
}

bool
c_vrfy_c_char (const CFI_cdesc_t *restrict auxp, const size_t len)
{
  CFI_index_t i, lb, ub, ex;
  size_t sz;
  c_char *ip = NULL;

  assert (auxp);
  assert (auxp->base_addr);
  assert (auxp->elem_len>0);
  lb = auxp->dim[0].lower_bound;
  ex = auxp->dim[0].extent;
  assert (ex==N);
  sz = (size_t)auxp->elem_len / sizeof (c_char);
  assert (sz==len);
  ub = ex + lb - 1;
  ip = (c_char*)auxp->base_addr;
  for (i=0; i<ex; i++, ip+=sz)
    if (!charcmp (ip, (c_char)(i), sz))
      return false;
  for (i=lb; i<ub+1; i++)
    {
      ip = (c_char*)CFI_address(auxp, &i);
      if (!charcmp (ip, (c_char)(i-lb), sz))
	return false;
    }
  return true;
}

bool
c_vrfy_c_ucs4_char (const CFI_cdesc_t *restrict auxp, const size_t len)
{
  CFI_index_t i, lb, ub, ex;
  size_t sz;
  c_ucs4_char *ip = NULL;

  assert (auxp);
  assert (auxp->base_addr);
  assert (auxp->elem_len>0);
  lb = auxp->dim[0].lower_bound;
  ex = auxp->dim[0].extent;
  assert (ex==N);
  sz = (size_t)auxp->elem_len / sizeof (c_ucs4_char);
  assert (sz==len);
  ub = ex + lb - 1;
  ip = (c_ucs4_char*)auxp->base_addr;
  for (i=0; i<ex; i++, ip+=sz)
    if (!ucharcmp (ip, (c_ucs4_char)(i), sz))
      return false;
  for (i=lb; i<ub+1; i++)
    {
      ip = (c_ucs4_char*)CFI_address(auxp, &i);
      if (!ucharcmp (ip, (c_ucs4_char)(i-lb), sz))
	return false;
    }
  return true;
}

bool
c_vrfy_character (const CFI_cdesc_t *restrict auxp, const size_t len)
{
  signed char type, kind;
  
  assert (auxp);
  type = _CFI_decode_type(auxp->type);
  kind = _CFI_decode_kind(auxp->type);
  assert (type == CFI_type_Character);
  switch (kind)
    {
    case 1:
      return c_vrfy_c_char (auxp, len);
      break;
    case 4:
      return c_vrfy_c_ucs4_char (auxp, len);
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
  assert (ityp == CFI_type_Character);
  iknd = _CFI_decode_kind(auxp->type);
  assert (_CFI_decode_type(type)==ityp);
  assert (kind==iknd);
  assert (c_vrfy_character (auxp, nelem));
  return;
}

// Local Variables:
// mode: C
// End:
