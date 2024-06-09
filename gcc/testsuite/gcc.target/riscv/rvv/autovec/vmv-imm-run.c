/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -fno-vect-cost-model -mrvv-vector-bits=scalable -fno-builtin" } */

#include "vmv-imm-template.h"

#include <stdint-gcc.h>
#include <assert.h>

#define SZ 512

#define TEST_POS(TYPE,VAL)		\
  TYPE a##TYPE##VAL[SZ];	  	\
  vmv_##VAL (a##TYPE##VAL, SZ);	  	\
  for (int i = 0; i < SZ; i++)	  	\
    assert (a##TYPE##VAL[i] == VAL);

#define TEST_NEG(TYPE,VAL)		\
  TYPE am##TYPE##VAL[SZ];	  	\
  vmv_m##VAL (am##TYPE##VAL, SZ); 	\
  for (int i = 0; i < SZ; i++)	  	\
    assert (am##TYPE##VAL[i] == -VAL);

int main ()
{
  TEST_NEG(int8_t, 16)
  TEST_NEG(int8_t, 15)
  TEST_NEG(int8_t, 14)
  TEST_NEG(int8_t, 13)
  TEST_NEG(int16_t, 12)
  TEST_NEG(int16_t, 11)
  TEST_NEG(int16_t, 10)
  TEST_NEG(int16_t, 9)
  TEST_NEG(int32_t, 8)
  TEST_NEG(int32_t, 7)
  TEST_NEG(int32_t, 6)
  TEST_NEG(int32_t, 5)
  TEST_NEG(int64_t, 4)
  TEST_NEG(int64_t, 3)
  TEST_NEG(int64_t, 2)
  TEST_NEG(int64_t, 1)
  TEST_POS(uint8_t, 0)
  TEST_POS(uint8_t, 1)
  TEST_POS(uint8_t, 2)
  TEST_POS(uint8_t, 3)
  TEST_POS(uint16_t, 4)
  TEST_POS(uint16_t, 5)
  TEST_POS(uint16_t, 6)
  TEST_POS(uint16_t, 7)
  TEST_POS(uint32_t, 8)
  TEST_POS(uint32_t, 9)
  TEST_POS(uint32_t, 10)
  TEST_POS(uint32_t, 11)
  TEST_POS(uint64_t, 12)
  TEST_POS(uint64_t, 13)
  TEST_POS(uint64_t, 14)
  TEST_POS(uint64_t, 15)
  TEST_POS(uint32_t, 16)
  TEST_POS(uint32_t, 123)
  TEST_POS(uint32_t, 255)
  TEST_POS(uint32_t, 999)
  TEST_POS(uint32_t, 32701)
  TEST_POS(uint32_t, 65535)
  TEST_POS(uint32_t, 65536)
  TEST_POS(uint32_t, 923423)
}
