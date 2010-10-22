/* { dg-do compile } */
/* { dg-options "-msse2 -ffloat-store" } */

typedef double v2df __attribute__((vector_size (16)));

v2df foo (double *d)
{
  return __builtin_ia32_loadupd (d);
}
