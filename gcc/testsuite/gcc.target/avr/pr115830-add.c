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

#define MK_FUN(TYP, OP, TST)		\
NI TYP add1_##TYP##_##OP (TYP a, TYP b)	\
{					\
  a += b;				\
  if (a TST)				\
    v = 0;				\
  return a;				\
}					\
					\
NI TYP add2_##TYP##_##OP (TYP a, TYP b)	\
{					\
  a += b;				\
  __asm ("" : "+r" (a));		\
  if (a TST)				\
    v = 0;				\
  return a;				\
}					\
					\
NI void test_##TYP##_##OP (TYP a, TYP b)\
{					\
  v = 1;				\
  TYP c1 = add1_##TYP##_##OP (a, b);	\
  v1 = v;				\
  v = 1;				\
  TYP c2 = add2_##TYP##_##OP (a, b);	\
  if (c1 != c2)				\
    __builtin_abort();			\
  if (v1 != v)				\
    __builtin_abort();			\
}

MK_FUN (i16, ge, >= 0)
MK_FUN (i24, ge, >= 0)
MK_FUN (i32, ge, >= 0)

MK_FUN (i16, lt, < 0)
MK_FUN (i24, lt, < 0)
MK_FUN (i32, lt, < 0)


int main (void)
{
  for (i8 a = -5; a <= 5; ++a)
    {
      for (i8 b = -5; b <= 5; ++b)
	{
	  test_i16_ge (a, b);
	  test_i24_ge (a, b);
	  test_i32_ge (a, b);

	  test_i16_lt (a, b);
	  test_i24_lt (a, b);
	  test_i32_lt (a, b);
	}
    }

  return 0;
}
