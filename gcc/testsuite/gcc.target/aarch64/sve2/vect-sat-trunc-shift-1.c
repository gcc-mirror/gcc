/* { dg-do compile } */
/* { dg-options "-O3 -mautovec-preference=sve-only" } */

typedef __INT8_TYPE__ int8_t;
typedef __INT16_TYPE__ int16_t;
typedef __INT32_TYPE__ int32_t;
typedef __INT64_TYPE__ int64_t;
typedef __UINT8_TYPE__ uint8_t;
typedef __UINT16_TYPE__ uint16_t;
typedef __UINT32_TYPE__ uint32_t;
typedef __UINT64_TYPE__ uint64_t;

/* The largest shift amounts for which the saturation survives value range
   propagation.  A larger amount makes the clamp dead, so the fused form is
   unreachable from C.  */
#define SHIFT_H 7
#define SHIFT_S 15
#define SHIFT_D 31

#define DEF_UNSIGNED(NAME, OUT, IN, SHIFT, MAX)			\
  void __attribute__((noipa))					\
  NAME (OUT *__restrict out, const IN *__restrict in, int n)	\
  {								\
    for (int i = 0; i < n; ++i)					\
      {								\
	IN x = in[i] >> SHIFT;					\
	out[i] = x > (IN) MAX ? (OUT) MAX : (OUT) x;		\
      }								\
  }

#define DEF_SIGNED(NAME, OUT, IN, SHIFT, MIN, MAX)		\
  void __attribute__((noipa))					\
  NAME (OUT *__restrict out, const IN *__restrict in, int n)	\
  {								\
    for (int i = 0; i < n; ++i)					\
      {								\
	IN x = in[i] >> SHIFT;					\
	OUT tmp = (OUT) x;					\
	out[i] = ((IN) MIN <= x && x <= (IN) MAX		\
		  ? tmp : x < 0 ? (OUT) MIN : (OUT) MAX);	\
      }								\
  }

DEF_UNSIGNED (u16_to_u8, uint8_t, uint16_t, SHIFT_H, 255)
DEF_UNSIGNED (u32_to_u16, uint16_t, uint32_t, SHIFT_S, 65535)
DEF_UNSIGNED (u64_to_u32, uint32_t, uint64_t, SHIFT_D, 4294967295ULL)
DEF_SIGNED (s16_to_s8, int8_t, int16_t, SHIFT_H, -128, 127)
DEF_SIGNED (s32_to_s16, int16_t, int32_t, SHIFT_S, -32768, 32767)
DEF_SIGNED (s64_to_s32, int32_t, int64_t, SHIFT_D, -2147483647 - 1, 2147483647)

/* { dg-final { scan-assembler-times {\tuqshrnb\tz[0-9]+\.b, z[0-9]+\.h, #7\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuqshrnb\tz[0-9]+\.h, z[0-9]+\.s, #15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuqshrnb\tz[0-9]+\.s, z[0-9]+\.d, #31\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsqshrnb\tz[0-9]+\.b, z[0-9]+\.h, #7\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsqshrnb\tz[0-9]+\.h, z[0-9]+\.s, #15\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsqshrnb\tz[0-9]+\.s, z[0-9]+\.d, #31\n} 1 } } */
/* { dg-final { scan-assembler-not {\t[lsa]sr\tz} } } */
