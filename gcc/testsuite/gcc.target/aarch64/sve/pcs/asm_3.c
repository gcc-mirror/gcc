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

ASM_FUNCTION (u8_callee, svuint8_t, svuint8x2_t,
	      "add z0.b, p0/m, z0.b, z1.b");
ASM_FUNCTION (u16_callee, svuint16_t, svuint16x2_t,
	      "add z0.h, p0/m, z0.h, z1.h");
ASM_FUNCTION (u32_callee, svuint32_t, svuint32x2_t,
	      "add z0.s, p0/m, z0.s, z1.s");
ASM_FUNCTION (u64_callee, svuint64_t, svuint64x2_t,
	      "add z0.d, p0/m, z0.d, z1.d");

ASM_FUNCTION (s8_callee, svint8_t, svint8x2_t,
	      "add z0.b, p0/m, z0.b, z1.b");
ASM_FUNCTION (s16_callee, svint16_t, svint16x2_t,
	      "add z0.h, p0/m, z0.h, z1.h");
ASM_FUNCTION (s32_callee, svint32_t, svint32x2_t,
	      "add z0.s, p0/m, z0.s, z1.s");
ASM_FUNCTION (s64_callee, svint64_t, svint64x2_t,
	      "add z0.d, p0/m, z0.d, z1.d");

ASM_FUNCTION (f16_callee, svfloat16_t, svfloat16x2_t,
	      "fadd z0.h, p0/m, z0.h, z1.h");
ASM_FUNCTION (f32_callee, svfloat32_t, svfloat32x2_t,
	      "fadd z0.s, p0/m, z0.s, z1.s");
ASM_FUNCTION (f64_callee, svfloat64_t, svfloat64x2_t,
	      "fadd z0.d, p0/m, z0.d, z1.d");

int
main (void)
{
#define CHECK(SUFFIX)							\
  if (svptest_any (svptrue_b8 (),					\
		   svcmpne (svptrue_b8 (),				\
			    SUFFIX##_callee (svptrue_b8 (),		\
					     svcreate2 (svdup_##SUFFIX (3), \
							svdup_##SUFFIX (6))), \
			    svdup_##SUFFIX (9))))			\
    __builtin_abort ()

  CHECK (u8);
  CHECK (u16);
  CHECK (u32);
  CHECK (u64);

  CHECK (s8);
  CHECK (s16);
  CHECK (s32);
  CHECK (s64);

  CHECK (f16);
  CHECK (f32);
  CHECK (f64);

  return 0;
}
