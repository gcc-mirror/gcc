/* { dg-do run } */
/* { dg-additional-options { -std=c99 -fwrapv -Os } } */

typedef __UINT8_TYPE__ uint8_t;
typedef __INT8_TYPE__  int8_t;

#define AI static inline __attribute__((always_inline))
#define NI __attribute__((noipa))

#define TYP int8_t

TYP volatile v;

#define MK_FUN(ID, OP, TST)                     \
NI TYP func1_##ID (TYP c)                       \
{                                               \
  v = 42;                                       \
  c OP;                                         \
  if (c TST)                                    \
    v = c;                                      \
  return v;                                     \
}                                               \
                                                \
NI TYP func2_##ID (TYP c)                       \
{                                               \
  TYP v = 42;                                   \
  c OP;                                         \
  __asm ("" : "+r" (c));                        \
  if (c TST)                                    \
    v = c;                                      \
  return v;                                     \
}

MK_FUN (IOR_01, &= 0x8, != 0)
MK_FUN (IOR_02, &= 0x8, == 0)
MK_FUN (IOR_03, &= 0x8, >= 0)
MK_FUN (IOR_04, &= 0x8, <= 0)
MK_FUN (IOR_05, &= 0x8, > 0)
MK_FUN (IOR_06, &= 0x8, < 0)
MK_FUN (IOR_07, &= 0x7f, != 0)
MK_FUN (IOR_08, &= 0x7f, == 0)
MK_FUN (IOR_09, &= 0x7f, >= 0)
MK_FUN (IOR_10, &= 0x7f, <= 0)
MK_FUN (IOR_11, &= 0x7f, > 0)
MK_FUN (IOR_12, &= 0x7f, < 0)
	

int main (void)
{
  uint8_t c = 0;
  do {
    if (func1_IOR_01 (c) != func2_IOR_01 (c)) __builtin_exit (__LINE__);
    if (func1_IOR_02 (c) != func2_IOR_02 (c)) __builtin_exit (__LINE__);
    if (func1_IOR_03 (c) != func2_IOR_03 (c)) __builtin_exit (__LINE__);
    if (func1_IOR_04 (c) != func2_IOR_04 (c)) __builtin_exit (__LINE__);
    if (func1_IOR_05 (c) != func2_IOR_05 (c)) __builtin_exit (__LINE__);
    if (func1_IOR_06 (c) != func2_IOR_06 (c)) __builtin_exit (__LINE__);
    if (func1_IOR_07 (c) != func2_IOR_07 (c)) __builtin_exit (__LINE__);
    if (func1_IOR_08 (c) != func2_IOR_08 (c)) __builtin_exit (__LINE__);
    if (func1_IOR_09 (c) != func2_IOR_09 (c)) __builtin_exit (__LINE__);
    if (func1_IOR_10 (c) != func2_IOR_10 (c)) __builtin_exit (__LINE__);
    if (func1_IOR_11 (c) != func2_IOR_11 (c)) __builtin_exit (__LINE__);
    if (func1_IOR_12 (c) != func2_IOR_12 (c)) __builtin_exit (__LINE__);
  } while (++c);

  return 0;
}
