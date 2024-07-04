/* { dg-do run { target { riscv_v } } } */


#include "gather_load_64-12.c"
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
#define RUN_LOOP(DATA_TYPE, INDEX_TYPE)                                        \
  DATA_TYPE dest_##DATA_TYPE##_##INDEX_TYPE[202] = {0};                        \
  DATA_TYPE src_##DATA_TYPE##_##INDEX_TYPE[202] = {0};                         \
  INDEX_TYPE index_##DATA_TYPE##_##INDEX_TYPE[202] = {0};                      \
  for (int i = 0; i < 202; i++)                                                \
    {                                                                          \
      src_##DATA_TYPE##_##INDEX_TYPE[i]                                        \
	= (DATA_TYPE) ((i * 19 + 735) & (sizeof (DATA_TYPE) * 7 - 1));         \
      index_##DATA_TYPE##_##INDEX_TYPE[i] = (i * 7) % (55);                    \
    }                                                                          \
  f_##DATA_TYPE##_##INDEX_TYPE (dest_##DATA_TYPE##_##INDEX_TYPE,               \
				src_##DATA_TYPE##_##INDEX_TYPE,                \
				index_##DATA_TYPE##_##INDEX_TYPE);             \
  for (int i = 0; i < 100; i++)                                                \
    {                                                                          \
      assert (dest_##DATA_TYPE##_##INDEX_TYPE[i * 2]                           \
	      == (src_##DATA_TYPE##_##INDEX_TYPE                               \
		    [index_##DATA_TYPE##_##INDEX_TYPE[i * 2]]                  \
		  + 1));                                                       \
      assert (dest_##DATA_TYPE##_##INDEX_TYPE[i * 2 + 1]                       \
	      == (src_##DATA_TYPE##_##INDEX_TYPE                               \
		    [index_##DATA_TYPE##_##INDEX_TYPE[i * 2 + 1]]              \
		  + 2));                                                       \
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
