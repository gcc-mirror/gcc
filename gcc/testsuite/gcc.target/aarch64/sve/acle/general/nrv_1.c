/* { dg-options "-O -msve-vector-bits=256" } */

#include <arm_sve.h>

typedef uint8_t v32qi __attribute__((vector_size (32)));

struct triple { v32qi v0, v1, v2; };

struct triple f (uint8_t *ptr)
{
  svuint8x3_t data = svld3 (svptrue_b8 (), ptr);
  struct triple res;
  res.v0 = svget3 (data, 0);
  res.v1 = svget3 (data, 1);
  res.v2 = svget3 (data, 2);
  return res;
}
