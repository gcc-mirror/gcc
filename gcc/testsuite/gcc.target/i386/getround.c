/* { dg-do compile } */
/* { dg-options "-O -msse" } */

#include <xmmintrin.h>

unsigned save;

void f(unsigned mode){
  unsigned tmp = _MM_GET_ROUNDING_MODE();
  _MM_SET_ROUNDING_MODE(mode);
  save = tmp;
}

/* { dg-final { scan-assembler-times "stmxcsr" 1 } } */
