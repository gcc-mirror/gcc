/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model --param=riscv-autovec-preference=fixed-vlmax -ffast-math" } */

#include "vfsqrt-template.h"

#include <assert.h>

#define SZ 255

#define EPS 1e-5

#define RUN(TYPE)						\
  TYPE a##TYPE[SZ];					      	\
  for (int i = 0; i < SZ; i++)				      	\
  {                             			      	\
    a##TYPE[i] = (TYPE)i;             			      	\
  }                             			      	\
  vsqrt_##TYPE (a##TYPE, a##TYPE, SZ);	        	      	\
  for (int i = 0; i < SZ; i++)				      	\
    assert (__builtin_fabs				      	\
	    (a##TYPE[i] -  __builtin_sqrtf ((TYPE)i)) < EPS);	\

#define RUN_ALL()						\
 RUN(float)	                                	      	\
 RUN(double)	                                	      	\

int main ()
{
  RUN_ALL()
}
