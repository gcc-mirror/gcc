/* { dg-do compile } */
/* { dg-additional-options "-std=c99 -O3 -march=rv64gcv_zvl128b -mabi=lp64d -fno-vect-cost-model -mrvv-vector-bits=zvl" } */

#include <stdint-gcc.h>

/* This would cause us to emit a vl1r.v for VNx4HImode even when
   the hardware vector size vl > 64.  */

extern int16_t test_element (int16_t);

typedef int16_t V __attribute__((vector_size (128)));

int main ()
{
  V v;
  for (int i = 0; i < sizeof (v) / sizeof (v[0]); i++)
    (v)[i] = i;

  V res = v;
  for (int i = 0; i < sizeof (v) / sizeof (v[0]); i++)
    test_element (res[i]);
}

/* { dg-final { scan-assembler-not {vl[1248]r.v} } }  */
/* { dg-final { scan-assembler-times {vl[1248]re16.v} 1 } }  */
