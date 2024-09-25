/* { dg-do run } */
/* { dg-additional-options { -std=c99 -fwrapv -Os } } */

typedef __UINT32_TYPE__ uint32_t;
typedef __INT32_TYPE__  int32_t;

#define AI static inline __attribute__((always_inline))
#define NI __attribute__((noipa))

#define TYP int32_t

TYP volatile v;

#define MK_FUN(ID, OP, TST)                     \
NI TYP func1_##ID (TYP c)                       \
{                                               \
  v = 0x77665544;                               \
  c OP;                                         \
  if (c TST)                                    \
    v = c;                                      \
  return v;                                     \
}                                               \
                                                \
NI TYP func2_##ID (TYP c)                       \
{                                               \
  TYP v = 0x77665544;                           \
  c OP;                                         \
  __asm ("" : "+r" (c));                        \
  if (c TST)                                    \
    v = c;                                      \
  return v;                                     \
}

MK_FUN (ASL_03, <<= 1, >= 0)
MK_FUN (ASL_06, <<= 1, < 0)

NI void test_asl (uint32_t c)
{
    if (func1_ASL_03 (c) != func2_ASL_03 (c)) __builtin_exit (__LINE__);
    if (func1_ASL_06 (c) != func2_ASL_06 (c)) __builtin_exit (__LINE__);
}

int main (void)
{
  test_asl (0);
  test_asl (0x80000000);
  test_asl (0x80000001);
  test_asl (0xc0000000);
  test_asl (0xc0000001);
  test_asl (0);
  test_asl (0xff00ff00);
  test_asl (0x00ff00ff);
  test_asl (0xff00ff00 >> 1);
  test_asl (0x00ff00ff >> 1);

  return 0;
}
