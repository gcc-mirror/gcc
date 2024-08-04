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

MK_FUN (AND_01, &= 0x80, != 0)
MK_FUN (AND_02, &= 0x80, == 0)
MK_FUN (AND_03, &= 0x80, >= 0)
MK_FUN (AND_04, &= 0x80, <= 0)
MK_FUN (AND_05, &= 0x80, > 0)
MK_FUN (AND_06, &= 0x80, < 0)
MK_FUN (AND_07, &= 0xef, != 0)
MK_FUN (AND_08, &= 0xef, == 0)
MK_FUN (AND_09, &= 0xef, >= 0)
MK_FUN (AND_10, &= 0xef, <= 0)
MK_FUN (AND_11, &= 0xef, > 0)
MK_FUN (AND_12, &= 0xef, < 0)
	

int main (void)
{
  uint8_t c = 0;
  do {
    if (func1_AND_01 (c) != func2_AND_01 (c)) __builtin_exit (__LINE__);
    if (func1_AND_02 (c) != func2_AND_02 (c)) __builtin_exit (__LINE__);
    if (func1_AND_03 (c) != func2_AND_03 (c)) __builtin_exit (__LINE__);
    if (func1_AND_04 (c) != func2_AND_04 (c)) __builtin_exit (__LINE__);
    if (func1_AND_05 (c) != func2_AND_05 (c)) __builtin_exit (__LINE__);
    if (func1_AND_06 (c) != func2_AND_06 (c)) __builtin_exit (__LINE__);
    if (func1_AND_07 (c) != func2_AND_07 (c)) __builtin_exit (__LINE__);
    if (func1_AND_08 (c) != func2_AND_08 (c)) __builtin_exit (__LINE__);
    if (func1_AND_09 (c) != func2_AND_09 (c)) __builtin_exit (__LINE__);
    if (func1_AND_10 (c) != func2_AND_10 (c)) __builtin_exit (__LINE__);
    if (func1_AND_11 (c) != func2_AND_11 (c)) __builtin_exit (__LINE__);
    if (func1_AND_12 (c) != func2_AND_12 (c)) __builtin_exit (__LINE__);
  } while (++c);

  return 0;
}
