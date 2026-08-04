/* { dg-do compile } */
/* { dg-options "-O2 -march=armv9-a -msve-vector-bits=256" } */

#include <arm_sve.h>
typedef svfloat32_t sv8f __attribute__((arm_sve_vector_bits(256)));
typedef float v8f __attribute__((vector_size(32)));

/* c - (sv8f)(a * b), multiply in the GNU vector type.  */
void p (v8f *pa, v8f *pb, sv8f *pc)
{
  v8f a = *pa, b = *pb;
  v8f m = a * b;
  *pc = *pc - (sv8f)m;
}

/* Mirrored: multiply in the SVE type, addend a GNU vector.  */
void q (sv8f *pa, sv8f *pb, v8f *pc)
{
  sv8f m = *pa * *pb;
  *pc = *pc - (v8f)m;
}

/* Explicit negate of a multiplicand.  */
void r (v8f *pa, v8f *pb, sv8f *pc)
{
  v8f m = (-*pa) * *pb;
  *pc = *pc + (sv8f)m;
}
