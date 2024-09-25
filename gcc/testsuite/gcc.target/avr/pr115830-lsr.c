/* { dg-do run } */
/* { dg-additional-options { -std=c99 -fwrapv -Os } } */

typedef __UINT8_TYPE__ uint8_t;
typedef __INT8_TYPE__  int8_t;

#define AI static inline __attribute__((always_inline))
#define NI __attribute__((noipa))

#define TYP uint8_t

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

MK_FUN (LSR_01, >>= 1, != 0)
MK_FUN (LSR_02, >>= 2, != 0)
MK_FUN (LSR_03, >>= 3, != 0)
MK_FUN (LSR_04, >>= 1, == 0)
MK_FUN (LSR_05, >>= 2, == 0)
MK_FUN (LSR_06, >>= 3, == 0)
MK_FUN (LSR_13, >>= 1, > 0)
MK_FUN (LSR_14, >>= 2, > 0)
MK_FUN (LSR_15, >>= 3, > 0)

int main (void)
{
  uint8_t c = 0;
  do {
    if (func1_LSR_01 (c) != func2_LSR_01 (c)) __builtin_exit (__LINE__);
    if (func1_LSR_02 (c) != func2_LSR_02 (c)) __builtin_exit (__LINE__);
    if (func1_LSR_03 (c) != func2_LSR_03 (c)) __builtin_exit (__LINE__);
    if (func1_LSR_04 (c) != func2_LSR_04 (c)) __builtin_exit (__LINE__);
    if (func1_LSR_05 (c) != func2_LSR_05 (c)) __builtin_exit (__LINE__);
    if (func1_LSR_06 (c) != func2_LSR_06 (c)) __builtin_exit (__LINE__);
    if (func1_LSR_13 (c) != func2_LSR_13 (c)) __builtin_exit (__LINE__);
    if (func1_LSR_14 (c) != func2_LSR_14 (c)) __builtin_exit (__LINE__);
    if (func1_LSR_15 (c) != func2_LSR_15 (c)) __builtin_exit (__LINE__);
  } while (++c);

  return 0;
}
