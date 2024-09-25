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

MK_FUN (ASL_01, <<= 1, != 0)
MK_FUN (ASL_02, <<= 2, != 0)
MK_FUN (ASL_03, <<= 3, != 0)
MK_FUN (ASL_04, <<= 1, == 0)
MK_FUN (ASL_05, <<= 2, == 0)
MK_FUN (ASL_06, <<= 3, == 0)
MK_FUN (ASL_07, <<= 1, >= 0)
MK_FUN (ASL_08, <<= 2, >= 0)
MK_FUN (ASL_09, <<= 3, >= 0)
MK_FUN (ASL_10, <<= 1, <= 0)
MK_FUN (ASL_11, <<= 2, <= 0)
MK_FUN (ASL_12, <<= 3, <= 0)
MK_FUN (ASL_13, <<= 1, > 0)
MK_FUN (ASL_14, <<= 2, > 0)
MK_FUN (ASL_15, <<= 3, > 0)
MK_FUN (ASL_16, <<= 1, < 0)
MK_FUN (ASL_17, <<= 2, < 0)
MK_FUN (ASL_18, <<= 3, < 0)

int main (void)
{
  uint8_t c = 0;
  do {
    if (func1_ASL_01 (c) != func2_ASL_01 (c)) __builtin_exit (__LINE__);
    if (func1_ASL_02 (c) != func2_ASL_02 (c)) __builtin_exit (__LINE__);
    if (func1_ASL_03 (c) != func2_ASL_03 (c)) __builtin_exit (__LINE__);
    if (func1_ASL_04 (c) != func2_ASL_04 (c)) __builtin_exit (__LINE__);
    if (func1_ASL_05 (c) != func2_ASL_05 (c)) __builtin_exit (__LINE__);
    if (func1_ASL_06 (c) != func2_ASL_06 (c)) __builtin_exit (__LINE__);
    if (func1_ASL_07 (c) != func2_ASL_07 (c)) __builtin_exit (__LINE__);
    if (func1_ASL_08 (c) != func2_ASL_08 (c)) __builtin_exit (__LINE__);
    if (func1_ASL_09 (c) != func2_ASL_09 (c)) __builtin_exit (__LINE__);
    if (func1_ASL_10 (c) != func2_ASL_10 (c)) __builtin_exit (__LINE__);
    if (func1_ASL_11 (c) != func2_ASL_11 (c)) __builtin_exit (__LINE__);
    if (func1_ASL_12 (c) != func2_ASL_12 (c)) __builtin_exit (__LINE__);
    if (func1_ASL_13 (c) != func2_ASL_13 (c)) __builtin_exit (__LINE__);
    if (func1_ASL_14 (c) != func2_ASL_14 (c)) __builtin_exit (__LINE__);
    if (func1_ASL_15 (c) != func2_ASL_15 (c)) __builtin_exit (__LINE__);
    if (func1_ASL_16 (c) != func2_ASL_16 (c)) __builtin_exit (__LINE__);
    if (func1_ASL_17 (c) != func2_ASL_17 (c)) __builtin_exit (__LINE__);
    if (func1_ASL_18 (c) != func2_ASL_18 (c)) __builtin_exit (__LINE__);
  } while (++c);

  return 0;
}
