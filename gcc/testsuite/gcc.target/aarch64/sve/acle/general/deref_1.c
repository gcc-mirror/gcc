/* { dg-options "-O2" } */

#include <arm_sve.h>

uint64_t
f1 (int32_t *x, int32_t *y)
{
  union { uint64_t x; char c[8]; } u;
  svbool_t pg = svptrue_b32 ();
  *(svbool_t *)&u.c[0] = svcmpeq (pg, svld1 (pg, x), 0);
  *(svbool_t *)&u.c[4] = svcmpeq (pg, svld1 (pg, y), 1);
  return u.x;
}

typedef unsigned int v4si __attribute__((vector_size(16)));

/* The aliasing is somewhat dubious here, but it must compile.  */

v4si
f2 (void)
{
  v4si res;
  *(svuint32_t *) &res = svindex_u32 (0, 1);
  return res;
}
