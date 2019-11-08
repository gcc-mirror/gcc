/* { dg-options "-O2" } */

#include <arm_sve.h>
#include <string.h>

inline void
copy (void *dst, svbool_t src)
{
  memcpy (dst, &src, svcntd ());
}

uint64_t
f (int32_t *x, int32_t *y)
{
  union { uint64_t x; char c[8]; } u;
  svbool_t pg = svptrue_b32 ();
  copy (u.c, svcmpeq (pg, svld1 (pg, x), 0));
  copy (u.c + 4, svcmpeq (pg, svld1 (pg, y), 1));
  return u.x;
}
