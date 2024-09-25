/* Test comparisons against constants that are a multiple of 256.  */
/* { dg-do run } */
/* { dg-additional-options { -std=c99 } } */

#define T u32

#ifdef __OPTIMIZE__

typedef __UINT64_TYPE__ u64;
typedef __UINT32_TYPE__ u32;
typedef __uint24 u24;
typedef __UINT16_TYPE__ u16;
typedef __UINT8_TYPE__ u8;

typedef __INT8_TYPE__ i8;

u8 volatile cc;

#define NI __attribute__((noipa))
#define AI static __inline__ __attribute__((always_inline))

#define MK_FUN(id, val)					\
NI void fun_geu_##id (T x)				\
{							\
  if (x >= val)						\
    cc = 0;						\
}							\
							\
NI T fun_ltu_##id (T x)					\
{							\
  if (x < val)						\
    cc = 0;						\
  return x;						\
}							\
							\
NI void test_##id (void)				\
{							\
  for (i8 v = -2; v <= 2; ++v)				\
    {							\
      const u8 lt0 = !! (v & 0x80);			\
      const T x = val + (T) v;				\
							\
      cc = 1;						\
      fun_geu_##id (x);					\
      if (cc != lt0)					\
	__builtin_exit (__LINE__);			\
							\
      cc = 1;						\
      T y = fun_ltu_##id (x);				\
      if (y != x)					\
	__builtin_exit (__LINE__);			\
      if (cc != ! lt0)					\
	__builtin_exit (__LINE__);			\
    }							\
}

MK_FUN (01, 0x100)
MK_FUN (02, 0x1200)
MK_FUN (03, 0x8000)
MK_FUN (04, 0x10000)
MK_FUN (05, 0x110000)
MK_FUN (06, 0x1000000)

#endif /* OPTIMIZE */

int main (void)
{
#ifdef __OPTIMIZE__
  test_01 ();
  test_02 ();
  test_03 ();
  test_04 ();
  test_05 ();
  test_06 ();
#endif /* OPTIMIZE */

  return 0;
}
