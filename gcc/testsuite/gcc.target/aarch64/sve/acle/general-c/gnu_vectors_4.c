/* { dg-options "-msve-vector-bits=256 -flax-vector-conversions" } */

#include <arm_sve.h>

typedef uint8_t gnu_uint8_t __attribute__ ((vector_size (32)));
typedef int8_t gnu_int8_t __attribute__ ((vector_size (32)));

void
f (svuint8_t sve_u1, svint8_t sve_s1,
   gnu_uint8_t gnu_u1, gnu_int8_t gnu_s1)
{
  gnu_uint8_t arr1[] = { gnu_u1, sve_u1 };
  gnu_uint8_t arr2[] = { gnu_s1 };
  gnu_uint8_t arr3[] = { sve_s1 };
}
