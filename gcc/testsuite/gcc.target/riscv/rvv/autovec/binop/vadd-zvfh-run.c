/* { dg-do run { target { riscv_v && riscv_zvfh } } } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -mrvv-vector-bits=zvl -ffast-math" } */

#include "vadd-template.h"

#include <assert.h>

#define SZ 512

#define RUN(TYPE,VAL)				\
  TYPE a##TYPE[SZ];				\
  TYPE b##TYPE[SZ];	  			\
  for (int i = 0; i < SZ; i++)			\
  {                             		\
    a##TYPE[i] = 0;             		\
    b##TYPE[i] = VAL;           		\
  }                             		\
  vadd_##TYPE (a##TYPE, a##TYPE, b##TYPE, SZ);	\
  for (int i = 0; i < SZ; i++)			\
    assert (a##TYPE[i] == VAL);

#define RUN2(TYPE,VAL)				\
  TYPE as##TYPE[SZ];				\
  for (int i = 0; i < SZ; i++)			\
    as##TYPE[i] = 0;            		\
  vadds_##TYPE (as##TYPE, as##TYPE, VAL, SZ);	\
  for (int i = 0; i < SZ; i++)			\
    assert (as##TYPE[i] == VAL);

#define RUN3(TYPE,VAL)				\
  TYPE ai##TYPE[SZ];	  	        	\
  for (int i = 0; i < SZ; i++)			\
    ai##TYPE[i] = VAL;				\
  vaddi_##TYPE (ai##TYPE, ai##TYPE, SZ);	\
  for (int i = 0; i < SZ; i++)			\
    assert (ai##TYPE[i] == VAL + 15);

#define RUN3M(TYPE,VAL)				\
  TYPE aim##TYPE[SZ];				\
  for (int i = 0; i < SZ; i++)			\
    aim##TYPE[i] = VAL;				\
  vaddim_##TYPE (aim##TYPE, aim##TYPE, SZ);	\
  for (int i = 0; i < SZ; i++)			\
    assert (aim##TYPE[i] == VAL - 16);

#define RUN_ALL()	\
 RUN(_Float16, 4)	\
 RUN2(_Float16, 10)	\
 RUN3M(_Float16, 17)	\

int main ()
{
  RUN_ALL()
}
