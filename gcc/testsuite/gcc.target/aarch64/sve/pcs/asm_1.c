/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O0 -ffixed-z0 -ffixed-p0" } */

#include <arm_sve.h>

#define ASM_FUNCTION(NAME, RET_TYPE, ARG_TYPE, INSN) \
extern RET_TYPE NAME (svbool_t, ARG_TYPE);	\
  asm(						\
"	.type	" #NAME ", %function\n"		\
#NAME ":\n"					\
"	" INSN "\n"				\
"	ret\n"					\
"	.size	" #NAME ", .-" #NAME "\n"	\
)

ASM_FUNCTION (u8_callee, uint64_t, svuint8_t,
	      "uaddv d0, p0, z0.b\n\tfmov x0, d0");
ASM_FUNCTION (u16_callee, uint64_t, svuint16_t,
	      "uaddv d0, p0, z0.h\n\tfmov x0, d0");
ASM_FUNCTION (u32_callee, uint64_t, svuint32_t,
	      "uaddv d0, p0, z0.s\n\tfmov x0, d0");
ASM_FUNCTION (u64_callee, uint64_t, svuint64_t,
	      "uaddv d0, p0, z0.d\n\tfmov x0, d0");

ASM_FUNCTION (s8_callee, int64_t, svint8_t,
	      "saddv d0, p0, z0.b\n\tfmov x0, d0");
ASM_FUNCTION (s16_callee, int64_t, svint16_t,
	      "saddv d0, p0, z0.h\n\tfmov x0, d0");
ASM_FUNCTION (s32_callee, int64_t, svint32_t,
	      "saddv d0, p0, z0.s\n\tfmov x0, d0");
ASM_FUNCTION (s64_callee, int64_t, svint64_t,
	      "uaddv d0, p0, z0.d\n\tfmov x0, d0");

ASM_FUNCTION (f16_callee, float16_t, svfloat16_t, "faddv\th0, p0, z0.h");
ASM_FUNCTION (f32_callee, float32_t, svfloat32_t, "faddv\ts0, p0, z0.s");
ASM_FUNCTION (f64_callee, float64_t, svfloat64_t, "faddv\td0, p0, z0.d");

int
main (void)
{
  if (u8_callee (svptrue_pat_b8 (SV_VL7), svdup_u8 (-1)) != 7 * 0xff)
    __builtin_abort ();
  if (u16_callee (svptrue_pat_b16 (SV_VL6), svdup_u16 (-1)) != 6 * 0xffff)
    __builtin_abort ();
  if (u32_callee (svptrue_pat_b32 (SV_VL3), svdup_u32 (-1))
      != 3 * (uint64_t) (uint32_t) -1)
    __builtin_abort ();
  if (u64_callee (svptrue_pat_b64 (SV_VL2), svdup_u64 ((uint64_t) 1 << 33))
      != (uint64_t) 1 << 34)
    __builtin_abort ();

  if (s8_callee (svptrue_pat_b8 (SV_VL7), svdup_s8 (-10)) != -70)
    __builtin_abort ();
  if (s16_callee (svptrue_pat_b16 (SV_VL6), svdup_s16 (-14)) != -84)
    __builtin_abort ();
  if (s32_callee (svptrue_pat_b32 (SV_VL3), svdup_s32 (-22)) != -66)
    __builtin_abort ();
  if (s64_callee (svptrue_pat_b64 (SV_VL2), svdup_s64 ((int64_t) 1 << 33))
      != (int64_t) 1 << 34)
    __builtin_abort ();

  if (f16_callee (svptrue_pat_b16 (SV_VL5), svdup_f16 (1.0)) != 5.0)
    __builtin_abort ();
  if (f32_callee (svptrue_b32 (), svdup_f32 (3.0)) != 3 * svcntw ())
    __builtin_abort ();
  if (f64_callee (svptrue_b64 (), svdup_f64 (11.0)) != 11 * svcntd ())
    __builtin_abort ();

  return 0;
}
