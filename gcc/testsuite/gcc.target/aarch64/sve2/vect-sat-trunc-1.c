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

#define DEF_UNSIGNED(NAME, OUT, IN, MAX)			\
  void __attribute__((noipa))					\
  NAME (OUT *__restrict out, const IN *__restrict in, int n)	\
  {								\
    for (int i = 0; i < n; ++i)					\
      {								\
	IN x = in[i];						\
	out[i] = x > (IN) MAX ? (OUT) MAX : (OUT) x;		\
      }								\
  }

#define DEF_SIGNED(NAME, OUT, IN, MIN, MAX)			\
  void __attribute__((noipa))					\
  NAME (OUT *__restrict out, const IN *__restrict in, int n)	\
  {								\
    for (int i = 0; i < n; ++i)					\
      {								\
	IN x = in[i];						\
	OUT tmp = (OUT) x;					\
	out[i] = ((IN) MIN <= x && x <= (IN) MAX		\
		  ? tmp : x < 0 ? (OUT) MIN : (OUT) MAX);	\
      }								\
  }

DEF_UNSIGNED (u16_to_u8, uint8_t, uint16_t, 255)
DEF_UNSIGNED (u32_to_u16, uint16_t, uint32_t, 65535)
DEF_UNSIGNED (u64_to_u32, uint32_t, uint64_t, 4294967295ULL)
DEF_SIGNED (s16_to_s8, int8_t, int16_t, -128, 127)
DEF_SIGNED (s32_to_s16, int16_t, int32_t, -32768, 32767)
DEF_SIGNED (s64_to_s32, int32_t, int64_t, -2147483647 - 1, 2147483647)

/* { dg-final { scan-assembler-times {\tuqxtnb\tz[0-9]+\.b, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuqxtnb\tz[0-9]+\.h, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tuqxtnb\tz[0-9]+\.s, z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsqxtnb\tz[0-9]+\.b, z[0-9]+\.h\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsqxtnb\tz[0-9]+\.h, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-times {\tsqxtnb\tz[0-9]+\.s, z[0-9]+\.d\n} 1 } } */
/* { dg-final { scan-assembler-not {\t[su]min\tz} } } */
