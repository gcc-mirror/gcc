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

#define F1(TYPE1, TYPE2,  OP_NAME, OP)                \
TYPE1                                                 \
__attribute__ ((noipa))                               \
f3_##OP_NAME##_##TYPE2##_##TYPE1 (unsigned TYPE2 *a)  \
{                                                     \
  unsigned TYPE2 b = OP*a;                            \
  return b;                                           \
}			
/* neghi_1_zext<mode> */
F1 (short, char, neg, -)
F1 (int, char, neg, -)
F1 (int64_t, char, neg, -)
F1 (int, short, neg, -)
F1 (int64_t, short, neg, -)
/* one_cmplqi2_1_zext<mode> */
F1 (short, char, not, ~)
F1 (int, char, not, ~)
F1 (int64_t, char, not, ~)
F1 (int, short, not, ~)
F1 (int64_t, short, not, ~)

/* andqi_1_zext<mode> */
F (short, char, and, &)
F (int, char, and, &)
F (int64_t, char, and, &)
F (int, short, and, &)
F (int64_t, short, and, &)
/* iorqi_1_zext<mode> */
F (short, char, or, |)
F (int, char, or, |)
F (int64_t, char, or, |)
F (int, short, or, |)
F (int64_t, short, or, |)
/* xorqi_1_zext<mode> */
F (short, char, xor, ^)
F (int, char, xor, ^)
F (int64_t, char, xor, ^)
F (int, short, xor, ^)
F (int64_t, short, xor, ^)
