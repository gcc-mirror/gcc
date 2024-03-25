/* { dg-do run { target { riscv_v } } } */
/* For some reason we exceed
   the default code model's +-2 GiB limits.  We should investigate why and
   add a proper description here.  For now just make sure the test case
   compiles properly.  */
/* { dg-additional-options "-mcmodel=medany" } */

#include "scatter_store_64-7.c"
#include <assert.h>

int
main (void)
{
#define RUN_LOOP(DATA_TYPE, BITS)                                              \
  DATA_TYPE dest_##DATA_TYPE[128];                                             \
  DATA_TYPE dest2_##DATA_TYPE[128];                                            \
  DATA_TYPE src_##DATA_TYPE[128];                                              \
  INDEX##BITS indices_##DATA_TYPE##_##BITS[128];                               \
  for (int i = 0; i < 128; i++)                                                \
    {                                                                          \
      dest_##DATA_TYPE[i] = (DATA_TYPE) ((i * 81 + 735) & (BITS - 1));         \
      dest2_##DATA_TYPE[i] = (DATA_TYPE) ((i * 81 + 735) & (BITS - 1));        \
      src_##DATA_TYPE[i] = (DATA_TYPE) ((i * 13 + 9107) & (BITS - 1));         \
      indices_##DATA_TYPE##_##BITS[i] = (DATA_TYPE) ((i * 3 + 67) % 128);      \
    }                                                                          \
  f_##DATA_TYPE (dest_##DATA_TYPE, src_##DATA_TYPE,                            \
		 indices_##DATA_TYPE##_##BITS);                                \
  for (int i = 0; i < 128; i++)                                                \
    assert (dest_##DATA_TYPE[indices_##DATA_TYPE##_##BITS[i]]                  \
	    == (src_##DATA_TYPE[i] + 1));

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
  return 0;
}
