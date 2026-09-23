/* AArch64 wrong code at -O3.  An oversized vector
   (V16DI, 128 bytes, no register mode) is expanded into two distinct
   stack slots that shared the same MEM_EXPR.  The load/store pair fusion
   pass then fused stores belonging to different slots, leaving part of a
   slot uninitialized.  Self-checking: the checksum is target-independent.  */

#include <stdint.h>

#define BS_VEC(type, num) type __attribute__((vector_size(num * sizeof(type))))
uint64_t BS_CHECKSUM, g_284;
struct U0
{
  int16_t f0;
  int64_t f2;
} g_205;
int16_t g_8, func_2_BS_COND_1;
uint8_t g_9[2][1];
volatile struct U0 g_121[1];
int64_t *g_565 = &g_284;

int
main (void)
{
  BS_VEC (int64_t, 16) BS_VAR_3 = { 1, 8096386231136, 9039249955151 };
  uint64_t LOCAL_CHECKSUM = 0;
  switch (func_2_BS_COND_1)
    {
    case 2: goto BS_LABEL_0;
    case 4: goto BS_LABEL_0;
    }
  for (g_8 = 0; g_8 <= 0; g_8 += 1)
    for (g_205.f2 = 0; g_205.f2 <= 1; g_205.f2 += 1)
      {
	if (g_9[g_205.f2][0])
	BS_LABEL_0:
	  for (;;)
	    ;
	for (uint32_t BS_TEMP_371 = 0; BS_TEMP_371 < 16; BS_TEMP_371++)
	  LOCAL_CHECKSUM ^= BS_VAR_3[BS_TEMP_371] + 9
	    + (LOCAL_CHECKSUM << 6) + LOCAL_CHECKSUM
	    >> (*g_565 |= g_121[0].f0);
	BS_VAR_3 = (BS_VEC (int64_t, 16)){};
      }
  BS_CHECKSUM = LOCAL_CHECKSUM;
  if (BS_CHECKSUM != 0x7237237237237237ULL)
    __builtin_abort ();
  return 0;
}
