/* { dg-do compile } */
/* { dg-options "-msse -ffloat-store" } */

typedef float v4sf __attribute__ ((__vector_size__ (16)));

v4sf foo (v4sf a)
{
  return __builtin_ia32_movlhps (a, a);
}
