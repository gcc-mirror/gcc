/* { dg-do run } */
/* { dg-additional-options { -std=c99 -Os } } */

typedef __UINT8_TYPE__ u8;
typedef __INT8_TYPE__  i8;
typedef __UINT16_TYPE__ u16;
typedef __INT16_TYPE__  i16;
__extension__ typedef __uint24 u24;
__extension__ typedef __int24  i24;
typedef __UINT32_TYPE__ u32;
typedef __INT32_TYPE__  i32;

#define AI static inline __attribute__((always_inline))
#define NI __attribute__((noipa))

u8 volatile v;
u8 v1;

#define MK_FUN(A, B, OP, TST)		\
NI A sub1_##A##_##B##_##OP (A a, B b)	\
{					\
  a -= b;				\
  if (a TST)				\
    v = 0;				\
  return a;				\
}					\
					\
NI A sub2_##A##_##B##_##OP (A a, B b)	\
{					\
  a -= b;				\
  __asm ("" : "+r" (a));		\
  if (a TST)				\
    v = 0;				\
  return a;				\
}					\
					\
NI void test_##A##_##B##_##OP (A a, B b)\
{					\
  v = 1;				\
  A c1 = sub1_##A##_##B##_##OP (a, b);	\
  v1 = v;				\
  v = 1;				\
  A c2 = sub2_##A##_##B##_##OP (a, b);	\
  if (c1 != c2)				\
    __builtin_abort();			\
  if (v1 != v)				\
    __builtin_abort();			\
}

MK_FUN (u16, u8, ne, != 0)
MK_FUN (u24, u8, eq, == 0)
MK_FUN (u32, u16, ne, != 0)

MK_FUN (i16, i8, ge, >= 0)
MK_FUN (i24, i8, ge, >= 0)
MK_FUN (i32, i16, ge, >= 0)

MK_FUN (i16, i8, lt, < 0)
MK_FUN (i24, i8, lt, < 0)
MK_FUN (i32, i16, lt, < 0)

MK_FUN (i16, i8, le, <= 0)
MK_FUN (i24, i8, le, <= 0)
MK_FUN (i32, i16, le, <= 0)

MK_FUN (i16, i8, gt, > 0)
MK_FUN (i24, i8, gt, > 0)
MK_FUN (i32, i16, gt, > 0)


int main (void)
{
  for (i8 a = -5; a <= 5; ++a)
    {
      for (i8 b = -5; b <= 5; ++b)
	{
	  test_u16_u8_ne (a, b);
	  test_u24_u8_eq (a, b);
	  test_u32_u16_ne (a, b);

	  test_i16_i8_ge (a, b);
	  test_i24_i8_ge (a, b);
	  test_i32_i16_ge (a, b);

	  test_i16_i8_lt (a, b);
	  test_i24_i8_lt (a, b);
	  test_i32_i16_lt (a, b);

	  test_i16_i8_gt (a, b);
	  test_i24_i8_gt (a, b);
	  test_i32_i16_gt (a, b);

	  test_i16_i8_le (a, b);
	  test_i24_i8_le (a, b);
	  test_i32_i16_le (a, b);
	}
    }

  return 0;
}
