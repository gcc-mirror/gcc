/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -mrvv-vector-bits=zvl -ffast-math" } */

#include "abs-template.h"

#include <assert.h>

#define SZ 128

#define RUN(TYPE)					  \
  TYPE a##TYPE[SZ];					  \
  for (int i = 0; i < SZ; i++)				  \
    {                             			  \
      if (i & 1)					  \
	a##TYPE[i] = i - 64;            		  \
      else						  \
	a##TYPE[i] = i;            			  \
    }                             			  \
  vabs_##TYPE (a##TYPE, a##TYPE, SZ);	        	  \
  for (int i = 0; i < SZ; i++)				  \
    {							  \
      if (i & 1)					  \
	assert (a##TYPE[i] == __builtin_abs (i - 64));	  \
      else						  \
	assert (a##TYPE[i] == i);			  \
    }


#define RUN_ALL()					  \
 RUN(int8_t)	                                	  \
 RUN(int16_t)	                                	  \
 RUN(int32_t)	                                	  \
 RUN(int64_t)						  \
 RUN(float)	                                	  \
 RUN(double)						  \

int main ()
{
  RUN_ALL()
}
