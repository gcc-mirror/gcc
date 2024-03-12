/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mcmodel=medany" } */

#include "strided_load-1.c"
#include <assert.h>

int
main (void)
{
  /* FIXME: The purpose of this assembly is to ensure that the vtype register is
     initialized befor instructions such as vmv1r.v are executed. Otherwise you
     will get illegal instruction errors when running with spike+pk. This is an
     interim solution for reduce unnecessary failures and a unified solution
     will come later. */
  asm volatile("vsetivli x0, 0, e8, m1, ta, ma");
#define RUN_LOOP(DATA_TYPE, BITS)                                              \
  DATA_TYPE dest_##DATA_TYPE##_##BITS[(BITS - 3) * (BITS + 13)];               \
  DATA_TYPE dest2_##DATA_TYPE##_##BITS[(BITS - 3) * (BITS + 13)];              \
  DATA_TYPE src_##DATA_TYPE##_##BITS[(BITS - 3) * (BITS + 13)];                \
  INDEX##BITS stride_##DATA_TYPE##_##BITS = (BITS - 3);                        \
  INDEX##BITS n_##DATA_TYPE##_##BITS = (BITS + 13);                            \
  for (INDEX##BITS i = 0;                                                      \
       i < stride_##DATA_TYPE##_##BITS * n_##DATA_TYPE##_##BITS; i++)          \
    {                                                                          \
      dest_##DATA_TYPE##_##BITS[i]                                             \
	= (DATA_TYPE) ((i * 81 + 735) & (BITS - 1));                           \
      dest2_##DATA_TYPE##_##BITS[i]                                            \
	= (DATA_TYPE) ((i * 81 + 735) & (BITS - 1));                           \
      src_##DATA_TYPE##_##BITS[i]                                              \
	= (DATA_TYPE) ((i * 13 + 9107) & (BITS - 1));                          \
    }                                                                          \
  f_##DATA_TYPE##_##BITS (dest_##DATA_TYPE##_##BITS, src_##DATA_TYPE##_##BITS, \
			  stride_##DATA_TYPE##_##BITS,                         \
			  n_##DATA_TYPE##_##BITS);                             \
  for (int i = 0; i < n_##DATA_TYPE##_##BITS; i++)                             \
    {                                                                          \
      assert (                                                                 \
	dest_##DATA_TYPE##_##BITS[i]                                           \
	== (dest2_##DATA_TYPE##_##BITS[i]                                      \
	    + src_##DATA_TYPE##_##BITS[i * stride_##DATA_TYPE##_##BITS]));     \
    }

  RUN_LOOP (int8_t, 8)
  RUN_LOOP (uint8_t, 8)
  RUN_LOOP (int16_t, 8)
  RUN_LOOP (uint16_t, 8)
  RUN_LOOP (_Float16, 8)
  RUN_LOOP (int32_t, 8)
  RUN_LOOP (uint32_t, 8)
  RUN_LOOP (float, 8)
  RUN_LOOP (int64_t, 8)
  RUN_LOOP (uint64_t, 8)
  RUN_LOOP (double, 8)

  RUN_LOOP (int8_t, 16)
  RUN_LOOP (uint8_t, 16)
  RUN_LOOP (int16_t, 16)
  RUN_LOOP (uint16_t, 16)
  RUN_LOOP (_Float16, 16)
  RUN_LOOP (int32_t, 16)
  RUN_LOOP (uint32_t, 16)
  RUN_LOOP (float, 16)
  RUN_LOOP (int64_t, 16)
  RUN_LOOP (uint64_t, 16)
  RUN_LOOP (double, 16)

  RUN_LOOP (int8_t, 32)
  RUN_LOOP (uint8_t, 32)
  RUN_LOOP (int16_t, 32)
  RUN_LOOP (uint16_t, 32)
  RUN_LOOP (_Float16, 32)
  RUN_LOOP (int32_t, 32)
  RUN_LOOP (uint32_t, 32)
  RUN_LOOP (float, 32)
  RUN_LOOP (int64_t, 32)
  RUN_LOOP (uint64_t, 32)
  RUN_LOOP (double, 32)

  RUN_LOOP (int8_t, 64)
  RUN_LOOP (uint8_t, 64)
  RUN_LOOP (int16_t, 64)
  RUN_LOOP (uint16_t, 64)
  RUN_LOOP (_Float16, 64)
  RUN_LOOP (int32_t, 64)
  RUN_LOOP (uint32_t, 64)
  RUN_LOOP (float, 64)
  RUN_LOOP (int64_t, 64)
  RUN_LOOP (uint64_t, 64)
  RUN_LOOP (double, 64)
  return 0;
}
