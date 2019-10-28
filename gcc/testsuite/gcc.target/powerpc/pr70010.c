/* { dg-do compile } */
/* { dg-options "-O2 -finline-functions -Wno-psabi -mvsx" } */
/* { dg-final { scan-assembler {\mbl \.?vadd_no_vsx\M} } } */

typedef int vec_t __attribute__((vector_size(16)));

static vec_t
__attribute__((__target__("no-vsx")))
vadd_no_vsx (vec_t a, vec_t b)
{
  return a + b;
}

vec_t
__attribute__((__target__("vsx")))
call_vadd_no_vsx (vec_t x, vec_t y, vec_t z)
{
  return vadd_no_vsx (x, y) - z;
}
