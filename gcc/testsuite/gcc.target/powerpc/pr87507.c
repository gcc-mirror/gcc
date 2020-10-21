/* { dg-do compile } */
/* { dg-require-effective-target int128 } */
/* { dg-options "-O2 -mdejagnu-cpu=power8" } */

typedef struct
{
  __int128_t x;
  __int128_t y;
} foo_t;

void
foo (long cond, foo_t *dst, __int128_t src)
{
  if (cond)
  {
    dst->x = src;
    dst->y = src;
  }
}

/* { dg-final { scan-assembler-times {\mstd\M} 4 } } */
/* { dg-final { scan-assembler-not {\mld\M} } } */
