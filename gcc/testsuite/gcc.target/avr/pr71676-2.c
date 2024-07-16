/* { dg-do compile } */
/* { dg-options "-dp -w -Os -fno-tree-switch-conversion" } */

#define MK_FUN(NAME, TYP, V)                            \
  unsigned char __attribute__((noipa))                  \
  select_## NAME (TYP x, unsigned char y)               \
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
  }

MK_FUN (0_s8, signed char, 0)
MK_FUN (0_u8, unsigned char, 0)
MK_FUN (0_s16, signed int, 0)
MK_FUN (0_u16, unsigned int, 0)

MK_FUN (m4_s8, signed char, -4)
MK_FUN (m4_u8, unsigned char, -4)
MK_FUN (m4_s16, signed int, -4)
MK_FUN (m4_u16, unsigned int, -4)

MK_FUN (4_s8, signed char, 4)
MK_FUN (4_u8, unsigned char, 4)
MK_FUN (4_s16, signed int, 4)
MK_FUN (4_u16, unsigned int, 4)

/* { dg-final { scan-assembler-not "extendqisi" } } */
/* { dg-final { scan-assembler-not "extendhisi" } } */
