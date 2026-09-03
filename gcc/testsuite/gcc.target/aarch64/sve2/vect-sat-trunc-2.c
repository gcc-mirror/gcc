/* { dg-do compile } */
/* { dg-options "-O3 -mautovec-preference=sve-only" } */

typedef __UINT8_TYPE__ uint8_t;
typedef __UINT16_TYPE__ uint16_t;
typedef __UINT32_TYPE__ uint32_t;
typedef __UINT64_TYPE__ uint64_t;

/* The second store has a wider element than the saturating narrowing, so the
   prevailing vector mode makes the narrowing operate on unpacked modes.  */

#define DEF(NAME, OUT, IN, WIDE, MAX)					\
  void __attribute__((noipa))						\
  NAME (OUT *__restrict out, WIDE *__restrict wide,			\
	const IN *__restrict in, int n)					\
  {									\
    for (int i = 0; i < n; ++i)						\
      {									\
	IN x = in[i];							\
	out[i] = x > (IN) MAX ? (OUT) MAX : (OUT) x;			\
	wide[i] = (WIDE) x * 3;						\
      }									\
  }

DEF (u16_to_u8_in_s, uint8_t, uint16_t, uint32_t, 255)
DEF (u16_to_u8_in_d, uint8_t, uint16_t, uint64_t, 255)
DEF (u32_to_u16_in_d, uint16_t, uint32_t, uint64_t, 65535)

/* { dg-final { scan-assembler-times {\tuqxtnb\tz[0-9]+\.b, z[0-9]+\.h\n} 2 } } */
/* { dg-final { scan-assembler-times {\tuqxtnb\tz[0-9]+\.h, z[0-9]+\.s\n} 1 } } */
/* { dg-final { scan-assembler-not {\tumin\tz} } } */
