/* { dg-do compile } */
/* { dg-require-effective-target mpx } */
/* { dg-options "-fcheck-pointer-bounds -mmpx -O2 -Wno-attributes" } */

static __attribute__((always_inline)) int f1 (int *p)
{
  return *p;
}

__attribute__((bnd_legacy)) int f2 (int *p)
{
  return f1 (p);
}
