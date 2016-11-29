/* { dg-do run } */
/* { dg-options "-Os -fno-tree-switch-conversion" } */

#define MK_FUN(NAME, TYP, V)                            \
  static __inline__ __attribute__((always_inline))      \
  unsigned char value_## NAME (TYP x, unsigned char y)  \
  {                                                     \
    switch (x)                                          \
      {                                                 \
      case V + 0: return 0 + y;                         \
      case V + 1: return 1;                             \
      case V + 2: return 2 + y;                         \
      case V + 3: return 3;                             \
      case V + 4: return 4 + y;                         \
      case V + 5: return 5;                             \
      case V + 6: return 6 + y;                         \
      case V + 7: return 7;                             \
      case V + 8: return 8 + y;                         \
      case V + 9: return 9;                             \
      case V + 10: return 10 + y;                       \
      case V + 11: return 11;                           \
      case V + 12: return 12 + y;                       \
      case V + 13: return 13;                           \
      case V + 14: return 14 + y;                       \
      case V + 15: return 15;                           \
      }                                                 \
    return x;                                           \
  }                                                     \
                                                        \
  __attribute__((noinline,noclone))                     \
  unsigned char select_## NAME (TYP x, unsigned char y) \
  {                                                     \
    return value_## NAME (x, y);                        \
  }                                                     \
                                                        \
  static __inline__ __attribute__((always_inline))      \
  void test1_## NAME (TYP x)                            \
  {                                                     \
    if (select_## NAME (x, 0) != value_## NAME (x, 0))  \
      __builtin_abort();                                \
  }                                                     \
                                                        \
  void test_## NAME (void)                              \
  {                                                     \
    test1_## NAME (V);                                  \
    test1_## NAME (V - 1);                              \
    test1_## NAME (V + 15);                             \
    test1_## NAME (V + 16);                             \
  }

MK_FUN (0_s8, signed char, 0)
MK_FUN (0_u8, unsigned char, 0)
MK_FUN (0_s16, signed int, 0)
MK_FUN (0_u16, unsigned int, 0)

MK_FUN (m4_s8, signed char, -4)
MK_FUN (m4_s16, signed int, -4)

MK_FUN (4_s8, signed char, 4)
MK_FUN (4_u8, unsigned char, 4)
MK_FUN (4_s16, signed int, 4)
MK_FUN (4_u16, unsigned int, 4)

int main (void)
{
  test_0_s8();
  test_0_u8();
  test_0_s16();
  test_0_u16();

  test_m4_s8();
  test_m4_s16();

  test_4_s8();
  test_4_u8();
  test_4_s16();
  test_4_u16();

  return 0;
}
