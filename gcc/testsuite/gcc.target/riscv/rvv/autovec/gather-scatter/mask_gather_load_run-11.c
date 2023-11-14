/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-mcmodel=medany" } */

#include "mask_gather_load_64-11.c"
#include <assert.h>

int
main (void)
{
#define RUN_LOOP(DATA_TYPE, INDEX_TYPE)                                        \
  DATA_TYPE dest_##DATA_TYPE##_##INDEX_TYPE[202] = {0};                        \
  DATA_TYPE dest2_##DATA_TYPE##_##INDEX_TYPE[202] = {0};                       \
  DATA_TYPE src_##DATA_TYPE##_##INDEX_TYPE[202] = {0};                         \
  INDEX_TYPE index_##DATA_TYPE##_##INDEX_TYPE[202] = {0};                      \
  INDEX_TYPE cond_##DATA_TYPE##_##INDEX_TYPE[202] = {0};                       \
  for (int i = 0; i < 202; i++)                                                \
    {                                                                          \
      src_##DATA_TYPE##_##INDEX_TYPE[i]                                        \
	= (DATA_TYPE) ((i * 19 + 735) & (sizeof (DATA_TYPE) * 7 - 1));         \
      dest_##DATA_TYPE##_##INDEX_TYPE[i]                                       \
	= (DATA_TYPE) ((i * 7 + 666) & (sizeof (DATA_TYPE) * 5 - 1));          \
      dest2_##DATA_TYPE##_##INDEX_TYPE[i]                                      \
	= (DATA_TYPE) ((i * 7 + 666) & (sizeof (DATA_TYPE) * 5 - 1));          \
      index_##DATA_TYPE##_##INDEX_TYPE[i] = (i * 7) % (55);                    \
      cond_##DATA_TYPE##_##INDEX_TYPE[i] = (INDEX_TYPE) ((i & 0x3) == 3);      \
    }                                                                          \
  f_##DATA_TYPE##_##INDEX_TYPE (dest_##DATA_TYPE##_##INDEX_TYPE,               \
				src_##DATA_TYPE##_##INDEX_TYPE,                \
				index_##DATA_TYPE##_##INDEX_TYPE,              \
				cond_##DATA_TYPE##_##INDEX_TYPE);              \
  for (int i = 0; i < 100; i++)                                                \
    {                                                                          \
      if (cond_##DATA_TYPE##_##INDEX_TYPE[i * 2])                              \
	assert (dest_##DATA_TYPE##_##INDEX_TYPE[i * 2]                         \
		== (src_##DATA_TYPE##_##INDEX_TYPE                             \
		      [index_##DATA_TYPE##_##INDEX_TYPE[i * 2]]                \
		    + 1));                                                     \
      else                                                                     \
	assert (dest_##DATA_TYPE##_##INDEX_TYPE[i * 2]                         \
		== dest2_##DATA_TYPE##_##INDEX_TYPE[i * 2]);                   \
      if (cond_##DATA_TYPE##_##INDEX_TYPE[i * 2 + 1])                          \
	assert (dest_##DATA_TYPE##_##INDEX_TYPE[i * 2 + 1]                     \
		== (src_##DATA_TYPE##_##INDEX_TYPE                             \
		      [index_##DATA_TYPE##_##INDEX_TYPE[i * 2 + 1]]            \
		    + 2));                                                     \
      else                                                                     \
	assert (dest_##DATA_TYPE##_##INDEX_TYPE[i * 2 + 1]                     \
		== dest2_##DATA_TYPE##_##INDEX_TYPE[i * 2 + 1]);               \
    }

  RUN_LOOP (int8_t, int8_t)
  RUN_LOOP (uint8_t, int8_t)
  RUN_LOOP (int16_t, int8_t)
  RUN_LOOP (uint16_t, int8_t)
  RUN_LOOP (int32_t, int8_t)
  RUN_LOOP (uint32_t, int8_t)
  RUN_LOOP (int64_t, int8_t)
  RUN_LOOP (uint64_t, int8_t)
  RUN_LOOP (_Float16, int8_t)
  RUN_LOOP (float, int8_t)
  RUN_LOOP (double, int8_t)
  RUN_LOOP (int8_t, int16_t)
  RUN_LOOP (uint8_t, int16_t)
  RUN_LOOP (int16_t, int16_t)
  RUN_LOOP (uint16_t, int16_t)
  RUN_LOOP (int32_t, int16_t)
  RUN_LOOP (uint32_t, int16_t)
  RUN_LOOP (int64_t, int16_t)
  RUN_LOOP (uint64_t, int16_t)
  RUN_LOOP (_Float16, int16_t)
  RUN_LOOP (float, int16_t)
  RUN_LOOP (double, int16_t)
  RUN_LOOP (int8_t, int32_t)
  RUN_LOOP (uint8_t, int32_t)
  RUN_LOOP (int16_t, int32_t)
  RUN_LOOP (uint16_t, int32_t)
  RUN_LOOP (int32_t, int32_t)
  RUN_LOOP (uint32_t, int32_t)
  RUN_LOOP (int64_t, int32_t)
  RUN_LOOP (uint64_t, int32_t)
  RUN_LOOP (_Float16, int32_t)
  RUN_LOOP (float, int32_t)
  RUN_LOOP (double, int32_t)
  RUN_LOOP (int8_t, int64_t)
  RUN_LOOP (uint8_t, int64_t)
  RUN_LOOP (int16_t, int64_t)
  RUN_LOOP (uint16_t, int64_t)
  RUN_LOOP (int32_t, int64_t)
  RUN_LOOP (uint32_t, int64_t)
  RUN_LOOP (int64_t, int64_t)
  RUN_LOOP (uint64_t, int64_t)
  RUN_LOOP (_Float16, int64_t)
  RUN_LOOP (float, int64_t)
  RUN_LOOP (double, int64_t)
  RUN_LOOP (int8_t, uint8_t)
  RUN_LOOP (uint8_t, uint8_t)
  RUN_LOOP (int16_t, uint8_t)
  RUN_LOOP (uint16_t, uint8_t)
  RUN_LOOP (int32_t, uint8_t)
  RUN_LOOP (uint32_t, uint8_t)
  RUN_LOOP (int64_t, uint8_t)
  RUN_LOOP (uint64_t, uint8_t)
  RUN_LOOP (_Float16, uint8_t)
  RUN_LOOP (float, uint8_t)
  RUN_LOOP (double, uint8_t)
  RUN_LOOP (int8_t, uint16_t)
  RUN_LOOP (uint8_t, uint16_t)
  RUN_LOOP (int16_t, uint16_t)
  RUN_LOOP (uint16_t, uint16_t)
  RUN_LOOP (int32_t, uint16_t)
  RUN_LOOP (uint32_t, uint16_t)
  RUN_LOOP (int64_t, uint16_t)
  RUN_LOOP (uint64_t, uint16_t)
  RUN_LOOP (_Float16, uint16_t)
  RUN_LOOP (float, uint16_t)
  RUN_LOOP (double, uint16_t)
  RUN_LOOP (int8_t, uint32_t)
  RUN_LOOP (uint8_t, uint32_t)
  RUN_LOOP (int16_t, uint32_t)
  RUN_LOOP (uint16_t, uint32_t)
  RUN_LOOP (int32_t, uint32_t)
  RUN_LOOP (uint32_t, uint32_t)
  RUN_LOOP (int64_t, uint32_t)
  RUN_LOOP (uint64_t, uint32_t)
  RUN_LOOP (_Float16, uint32_t)
  RUN_LOOP (float, uint32_t)
  RUN_LOOP (double, uint32_t)
  RUN_LOOP (int8_t, uint64_t)
  RUN_LOOP (uint8_t, uint64_t)
  RUN_LOOP (int16_t, uint64_t)
  RUN_LOOP (uint16_t, uint64_t)
  RUN_LOOP (int32_t, uint64_t)
  RUN_LOOP (uint32_t, uint64_t)
  RUN_LOOP (int64_t, uint64_t)
  RUN_LOOP (uint64_t, uint64_t)
  RUN_LOOP (_Float16, uint64_t)
  RUN_LOOP (float, uint64_t)
  RUN_LOOP (double, uint64_t)
  return 0;
}
