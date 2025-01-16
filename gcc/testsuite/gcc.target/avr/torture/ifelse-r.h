/* Testing pass avr-ifelse with REG-REG comparisons. */

#define V ((T) (W))

#ifdef __OPTIMIZE__

typedef __UINT32_TYPE__ u32;
__extension__ typedef __uint24 u24;
typedef __UINT16_TYPE__ u16;
typedef __UINT8_TYPE__ u8;

typedef __INT32_TYPE__ i32;
__extension__ typedef __int24 i24;
typedef __INT16_TYPE__ i16;
typedef __INT8_TYPE__ i8;

char volatile cc;

#define NI __attribute__((noipa))
#define AI static __inline__ __attribute__((always_inline))

NI
void f (char x)
{
  cc += x;
}

#define MK_FUN(id, cmp1, cmp2)			\
NI void fun_##id (T x, T y)			\
{						\
  if (x cmp1 y)					\
    goto a;					\
  if (x cmp2 y)					\
    goto b;					\
  f(1);						\
 a:						\
  f(2);						\
 b:						\
  f(4);						\
}						\
						\
NI char val_##id (T x, T y)			\
{						\
  char c = 0;					\
  if (x cmp1 y)					\
    goto a;					\
  __asm ("" : "+r" (x));			\
  __asm ("" : "+r" (y));			\
  if (x cmp2 y)					\
    goto b;					\
  c += 1;					\
 a:						\
  c += 2;					\
 b:						\
 c += 4;					\
						\
 return c;					\
}

MK_FUN (01, >, <)
MK_FUN (02, <, >)

MK_FUN (03, ==, <= )
MK_FUN (04, ==, >= )

MK_FUN (05, ==, <)
MK_FUN (06, ==, >)

MK_FUN (07, >, ==)
MK_FUN (08, <, ==)

MK_FUN (09, >, !=)
MK_FUN (10, <, !=)

void testA (void)
{
  for (T x = (T) (V - 2); x != (T) (V + 2); ++x)
    {
      cc = 0; fun_01 (x,V); if (cc != val_01 (x,V)) __builtin_exit (__LINE__);
      cc = 0; fun_02 (x,V); if (cc != val_02 (x,V)) __builtin_exit (__LINE__);
      cc = 0; fun_03 (x,V); if (cc != val_03 (x,V)) __builtin_exit (__LINE__);
      cc = 0; fun_04 (x,V); if (cc != val_04 (x,V)) __builtin_exit (__LINE__);
      cc = 0; fun_05 (x,V); if (cc != val_05 (x,V)) __builtin_exit (__LINE__);
      cc = 0; fun_06 (x,V); if (cc != val_06 (x,V)) __builtin_exit (__LINE__);
      cc = 0; fun_07 (x,V); if (cc != val_07 (x,V)) __builtin_exit (__LINE__);
      cc = 0; fun_08 (x,V); if (cc != val_08 (x,V)) __builtin_exit (__LINE__);
      cc = 0; fun_09 (x,V); if (cc != val_09 (x,V)) __builtin_exit (__LINE__);
      cc = 0; fun_10 (x,V); if (cc != val_10 (x,V)) __builtin_exit (__LINE__);
    }
}
#endif /* OPTIMIZE */

int main (void)
{
#ifdef __OPTIMIZE__
  testA();
#endif /* OPTIMIZE */

  return 0;
}
