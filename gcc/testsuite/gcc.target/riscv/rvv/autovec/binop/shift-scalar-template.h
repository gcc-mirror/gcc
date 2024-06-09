/* Test shifts by scalar (immediate or register) amount.  */
/* { dg-do run } */
/* { dg-additional-options "-std=c99 -mrvv-vector-bits=scalable -fno-vect-cost-model --save-temps" } */

#include <stdint-gcc.h>

#define SHIFTL(TYPE,VAL)				\
  __attribute__ ((noipa))                               \
  void vsll_##TYPE_##VAL (TYPE *dst, int n)		\
  {                                                     \
    for (int i = 0; i < n; i++)                         \
      dst[i] <<= VAL;					\
  }

#define SHIFTR(TYPE,VAL)				\
  __attribute__ ((noipa))                               \
  void vsrx_##TYPE_##VAL (TYPE *dst, int n)		\
  {                                                     \
    for (int i = 0; i < n; i++)                         \
      dst[i] >>= VAL;					\
  }

#define TEST_ALL()	\
SHIFTL(uint32_t,1)    	\
SHIFTL(uint32_t,2)    	\
SHIFTL(uint32_t,3)    	\
SHIFTL(uint32_t,4)      \
SHIFTL(uint32_t,5)      \
SHIFTL(uint32_t,6)      \
SHIFTL(uint32_t,7)	\
SHIFTL(uint32_t,8)	\
SHIFTL(uint32_t,9)	\
SHIFTL(uint32_t,10)	\
SHIFTL(uint32_t,11)	\
SHIFTL(uint32_t,12)	\
SHIFTL(uint32_t,13)	\
SHIFTL(uint32_t,14)	\
SHIFTL(uint32_t,15)	\
SHIFTL(uint32_t,16)	\
SHIFTL(uint32_t,17)	\
SHIFTL(uint32_t,18)	\
SHIFTL(uint32_t,19)	\
SHIFTL(uint32_t,20)	\
SHIFTL(uint32_t,21)	\
SHIFTL(uint32_t,22)	\
SHIFTL(uint32_t,23)	\
SHIFTL(uint32_t,24)	\
SHIFTL(uint32_t,25)	\
SHIFTL(uint32_t,26)	\
SHIFTL(uint32_t,27)	\
SHIFTL(uint32_t,28)	\
SHIFTL(uint32_t,29)	\
SHIFTL(uint32_t,30)	\
SHIFTL(uint64_t,31)	\

TEST_ALL()

#define SZ 32

#define TEST_VSLL(TYPE,VAL)		\
  TYPE a##TYPE##VAL[SZ];	  	\
  for (int i = 0; i < SZ; i++)		\
    a##TYPE##VAL[i] = 2;		\
  vsll_##TYPE_##VAL (a##TYPE##VAL, SZ);	\
  for (int i = 0; i < SZ; i++)	  	\
    if (a##TYPE##VAL[i] != (2ll << VAL)) __builtin_abort ();

__attribute__((noipa))
void vsllvx (uint32_t *dst, int val, int n)
{
  for (int i = 0; i < n; i++)
    dst[i] <<= val;
}

#define TEST_VSLLVX		\
  uint32_t a[SZ];	  	\
  for (int i = 0; i < SZ; i++)		\
    a[i] = 2;		\
  vsllvx (a, 17, SZ);	\
  for (int i = 0; i < SZ; i++)	  	\
    if (a[i] != (2 << 17)) __builtin_abort ();

int main ()
{
  TEST_VSLL(uint32_t,1)
  TEST_VSLL(uint32_t,2)
  TEST_VSLL(uint32_t,3)
  TEST_VSLL(uint32_t,4)
  TEST_VSLL(uint32_t,5)
  TEST_VSLL(uint32_t,6)
  TEST_VSLL(uint32_t,7)
  TEST_VSLL(uint32_t,8)
  TEST_VSLL(uint32_t,9)
  TEST_VSLL(uint32_t,10)
  TEST_VSLL(uint32_t,11)
  TEST_VSLL(uint32_t,12)
  TEST_VSLL(uint32_t,13)
  TEST_VSLL(uint32_t,14)
  TEST_VSLL(uint32_t,15)
  TEST_VSLL(uint32_t,16)
  TEST_VSLL(uint32_t,17)
  TEST_VSLL(uint32_t,18)
  TEST_VSLL(uint32_t,19)
  TEST_VSLL(uint32_t,20)
  TEST_VSLL(uint32_t,21)
  TEST_VSLL(uint32_t,22)
  TEST_VSLL(uint32_t,23)
  TEST_VSLL(uint32_t,24)
  TEST_VSLL(uint32_t,25)
  TEST_VSLL(uint32_t,26)
  TEST_VSLL(uint32_t,27)
  TEST_VSLL(uint32_t,28)
  TEST_VSLL(uint32_t,29)
  TEST_VSLL(uint32_t,30)
  TEST_VSLL(uint64_t,31)

  TEST_VSLLVX
}
