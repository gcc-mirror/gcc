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

MK_FUN (ASR_01, >>= 1, != 0)
MK_FUN (ASR_02, >>= 2, != 0)
MK_FUN (ASR_03, >>= 3, != 0)
MK_FUN (ASR_04, >>= 1, == 0)
MK_FUN (ASR_05, >>= 2, == 0)
MK_FUN (ASR_06, >>= 3, == 0)
MK_FUN (ASR_07, >>= 1, >= 0)
MK_FUN (ASR_08, >>= 2, >= 0)
MK_FUN (ASR_09, >>= 3, >= 0)
MK_FUN (ASR_10, >>= 1, <= 0)
MK_FUN (ASR_11, >>= 2, <= 0)
MK_FUN (ASR_12, >>= 3, <= 0)
MK_FUN (ASR_13, >>= 1, > 0)
MK_FUN (ASR_14, >>= 2, > 0)
MK_FUN (ASR_15, >>= 3, > 0)
MK_FUN (ASR_16, >>= 1, < 0)
MK_FUN (ASR_17, >>= 2, < 0)
MK_FUN (ASR_18, >>= 3, < 0)

int main (void)
{
  uint8_t c = 0;
  do {
    if (func1_ASR_01 (c) != func2_ASR_01 (c)) __builtin_exit (__LINE__);
    if (func1_ASR_02 (c) != func2_ASR_02 (c)) __builtin_exit (__LINE__);
    if (func1_ASR_03 (c) != func2_ASR_03 (c)) __builtin_exit (__LINE__);
    if (func1_ASR_04 (c) != func2_ASR_04 (c)) __builtin_exit (__LINE__);
    if (func1_ASR_05 (c) != func2_ASR_05 (c)) __builtin_exit (__LINE__);
    if (func1_ASR_06 (c) != func2_ASR_06 (c)) __builtin_exit (__LINE__);
    if (func1_ASR_07 (c) != func2_ASR_07 (c)) __builtin_exit (__LINE__);
    if (func1_ASR_08 (c) != func2_ASR_08 (c)) __builtin_exit (__LINE__);
    if (func1_ASR_09 (c) != func2_ASR_09 (c)) __builtin_exit (__LINE__);
    if (func1_ASR_10 (c) != func2_ASR_10 (c)) __builtin_exit (__LINE__);
    if (func1_ASR_11 (c) != func2_ASR_11 (c)) __builtin_exit (__LINE__);
    if (func1_ASR_12 (c) != func2_ASR_12 (c)) __builtin_exit (__LINE__);
    if (func1_ASR_13 (c) != func2_ASR_13 (c)) __builtin_exit (__LINE__);
    if (func1_ASR_14 (c) != func2_ASR_14 (c)) __builtin_exit (__LINE__);
    if (func1_ASR_15 (c) != func2_ASR_15 (c)) __builtin_exit (__LINE__);
    if (func1_ASR_16 (c) != func2_ASR_16 (c)) __builtin_exit (__LINE__);
    if (func1_ASR_17 (c) != func2_ASR_17 (c)) __builtin_exit (__LINE__);
    if (func1_ASR_18 (c) != func2_ASR_18 (c)) __builtin_exit (__LINE__);
  } while (++c);

  return 0;
}
