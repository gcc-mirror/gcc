#ifndef STRUCT_H
#define STRUCT_H 1

#include <arm_sve.h>

#ifndef __ARM_FEATURE_SVE_BITS
#error "__ARM_FEATURE_SVE_BITS should be defined"
#endif

#define FIXED_ATTR \
  __attribute__ ((arm_sve_vector_bits (__ARM_FEATURE_SVE_BITS)))

#define SVE_BYTES (__ARM_FEATURE_SVE_BITS / 8)

typedef __SVInt8_t fixed_int8_t FIXED_ATTR;
typedef __SVInt16_t fixed_int16_t FIXED_ATTR;
typedef __SVInt32_t fixed_int32_t FIXED_ATTR;
typedef __SVInt64_t fixed_int64_t FIXED_ATTR;

typedef __SVUint8_t fixed_uint8_t FIXED_ATTR;
typedef __SVUint16_t fixed_uint16_t FIXED_ATTR;
typedef __SVUint32_t fixed_uint32_t FIXED_ATTR;
typedef __SVUint64_t fixed_uint64_t FIXED_ATTR;

typedef __SVBfloat16_t fixed_bfloat16_t FIXED_ATTR;
typedef __SVFloat16_t fixed_float16_t FIXED_ATTR;
typedef __SVFloat32_t fixed_float32_t FIXED_ATTR;
typedef __SVFloat64_t fixed_float64_t FIXED_ATTR;

typedef __SVBool_t fixed_bool_t FIXED_ATTR;

/* Define an asm function called NAME with return type RET_TYPE and
   argument list ARG_TYPES.  INSNS contains the body of the function,
   except for the final "ret".

   Conservatively mark the function as a variant PCS function,
   since many uses are.  */
#define ASM_FUNCTION(NAME, RET_TYPE, ARG_TYPES, INSNS) \
extern RET_TYPE NAME ARG_TYPES;			\
  asm(						\
"	.type	" #NAME ", %function\n"		\
#NAME ":\n"					\
"	" INSNS "\n"				\
"	ret\n"					\
"	.size	" #NAME ", .-" #NAME "\n"	\
"	.variant_pcs " #NAME "\n"		\
)

/* Set the argument registers to fixed values.  */
#define CLEANSE								\
  asm volatile ("mov\tx0, #-1\n\t"					\
		"mov\tx1, #-1\n\t"					\
		"mov\tx2, #-1\n\t"					\
		"mov\tx3, #-1\n\t"					\
		"mov\tx4, #-1\n\t"					\
		"mov\tx5, #-1\n\t"					\
		"mov\tx6, #-1\n\t"					\
		"mov\tx7, #-1\n\t"					\
		"mov\tx8, #-1\n\t"					\
		"mov\tz0.b, #0xaf\n\t"					\
		"mov\tz1.b, #0xaf\n\t"					\
		"mov\tz2.b, #0xaf\n\t"					\
		"mov\tz3.b, #0xaf\n\t"					\
		"mov\tz4.b, #0xaf\n\t"					\
		"mov\tz5.b, #0xaf\n\t"					\
		"mov\tz6.b, #0xaf\n\t"					\
		"mov\tz7.b, #0xaf\n\t"					\
		"pfalse\tp0.b\n\t"					\
		"pfalse\tp1.b\n\t"					\
		"pfalse\tp2.b\n\t"					\
		"pfalse\tp3.b"						\
		:::							\
		"x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8",	\
		"z0", "z1", "z2", "z3", "z4", "z5", "z6", "z7",		\
		"p0", "p1", "p2", "p3")

#endif
