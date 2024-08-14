/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mapx-features=ndd -march=x86-64 -O2" } */
/* { dg-final { scan-assembler-not "movz"} } */

#include <stdint.h>

#define F(TYPE1, TYPE2, OP_NAME, OP)                \
TYPE1                                               \
__attribute__ ((noipa))                             \
f_##OP_NAME##_##TYPE2##_##TYPE1 (unsigned TYPE2 b)  \
{                                                   \
  return (unsigned TYPE2) (200 OP b);               \
}                                                   \
TYPE1                                               \
__attribute__ ((noipa))                             \
f1_##OP_NAME##_##TYPE2##_##TYPE1                    \
(unsigned TYPE2 a, unsigned TYPE2 b)                \
{                                                   \
  return (unsigned TYPE2) (a OP b);                 \
}					

/* addqi_1_zext<mode> */
F (short, char, add, +)
F (int, char, add, +)
F (int64_t, char, add, +)
F (int, short, add, +)
F (int64_t, short, add, +)
/* subqi_1_zext<mode> */
F (short, char, sub, -)
F (int, char, sub, -)
F (int64_t, char, sub, -)
F (int, short, sub, -)
F (int64_t, short, sub, -)
