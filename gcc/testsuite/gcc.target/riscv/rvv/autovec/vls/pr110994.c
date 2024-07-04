/* { dg-do compile } */
/* { dg-options "-march=rv64gc -mabi=lp64d -mrvv-vector-bits=scalable -O2" } */

#include "def.h"

void foo (int8_t *in, int8_t *out)
{
  v4qi v = *(v4qi*)in;
  *(v4qi*)out = v;
}
