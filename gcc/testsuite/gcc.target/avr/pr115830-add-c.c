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

MK_FUN (ADD_01, += 1, != 0)
MK_FUN (ADD_02, += 1, == 0)
MK_FUN (ADD_03, += 1, >= 0)
MK_FUN (ADD_04, += 1, <= 0)
MK_FUN (ADD_05, += 1, > 0)
MK_FUN (ADD_06, += 1, < 0)
MK_FUN (ADD_07, -= 2, != 0)
MK_FUN (ADD_08, -= 2, == 0)
MK_FUN (ADD_09, -= 2, >= 0)
MK_FUN (ADD_10, -= 2, <= 0)
MK_FUN (ADD_11, -= 2, > 0)
MK_FUN (ADD_12, -= 2, < 0)
MK_FUN (ADD_13, += 42, != 0)
MK_FUN (ADD_14, += 42, == 0)
MK_FUN (ADD_15, += 42, >= 0)
MK_FUN (ADD_16, += 42, <= 0)
MK_FUN (ADD_17, += 42, > 0)
MK_FUN (ADD_18, += 42, < 0)
	

int main (void)
{
  uint8_t c = 0;
  do {
    if (func1_ADD_01 (c) != func2_ADD_01 (c)) __builtin_exit (__LINE__);
    if (func1_ADD_02 (c) != func2_ADD_02 (c)) __builtin_exit (__LINE__);
    if (func1_ADD_03 (c) != func2_ADD_03 (c)) __builtin_exit (__LINE__);
    if (func1_ADD_04 (c) != func2_ADD_04 (c)) __builtin_exit (__LINE__);
    if (func1_ADD_05 (c) != func2_ADD_05 (c)) __builtin_exit (__LINE__);
    if (func1_ADD_06 (c) != func2_ADD_06 (c)) __builtin_exit (__LINE__);
    if (func1_ADD_07 (c) != func2_ADD_07 (c)) __builtin_exit (__LINE__);
    if (func1_ADD_08 (c) != func2_ADD_08 (c)) __builtin_exit (__LINE__);
    if (func1_ADD_09 (c) != func2_ADD_09 (c)) __builtin_exit (__LINE__);
    if (func1_ADD_10 (c) != func2_ADD_10 (c)) __builtin_exit (__LINE__);
    if (func1_ADD_11 (c) != func2_ADD_11 (c)) __builtin_exit (__LINE__);
    if (func1_ADD_12 (c) != func2_ADD_12 (c)) __builtin_exit (__LINE__);
    if (func1_ADD_13 (c) != func2_ADD_13 (c)) __builtin_exit (__LINE__);
    if (func1_ADD_14 (c) != func2_ADD_14 (c)) __builtin_exit (__LINE__);
    if (func1_ADD_15 (c) != func2_ADD_15 (c)) __builtin_exit (__LINE__);
    if (func1_ADD_16 (c) != func2_ADD_16 (c)) __builtin_exit (__LINE__);
    if (func1_ADD_17 (c) != func2_ADD_17 (c)) __builtin_exit (__LINE__);
    if (func1_ADD_18 (c) != func2_ADD_18 (c)) __builtin_exit (__LINE__);
  } while (++c);

  return 0;
}
