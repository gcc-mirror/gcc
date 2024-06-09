/* PR target/113711 */
/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mapxf -O2" } */

#include <stdint.h>

#define FOO(TYPE, OP_NAME, OP, IMM)				\
extern TYPE foo_##OP_NAME##_##TYPE##_var;			\
TYPE								\
foo_##OP_NAME##_##TYPE (void)					\
{								\
  TYPE b = foo_##OP_NAME##_##TYPE##_var OP IMM;			\
  return b;							\
}			

#define BAR(TYPE, UTYPE, OP_NAME, OP, IMM)			\
extern UTYPE bar_##OP_NAME##_##TYPE##_var;			\
int64_t								\
bar_##OP_NAME##_##TYPE (void)					\
{								\
  int64_t b = bar_##OP_NAME##_##TYPE##_var OP IMM;		\
  return b;							\
}			

FOO (char, add, +, 0x7)
FOO (short, add, +, 0x2000)
FOO (int, add, +, 0x2000)
BAR (int, unsigned int, add, +, 0x2000)
FOO (int64_t, add, +, 0x2000)
BAR (int64_t, uint64_t, add, +, 0x2000)

FOO (char, sub, -, 0x7)
FOO (short, sub, -, 0x2000)
FOO (int, sub, -, 0x2000)
BAR (int, unsigned int, sub, -, 0x2000)
FOO (int64_t, sub, -, 0x2000)
BAR (int64_t, uint64_t, sub, -, 0x2000)

FOO (char, and, &, 0x7)
FOO (short, and, &, 0x2000)
FOO (int, and, &, 0x2000)
BAR (int, unsigned int, and, &, 0x2000)
FOO (int64_t, and, &, 0x2000)
BAR (int64_t, uint64_t, and, &, 0x2000)

FOO (char, or, |, 0x7)
FOO (short, or, |, 0x2000)
FOO (int, or, |, 0x2000)
BAR (int, unsigned int, or, |, 0x2000)
FOO (int64_t, or, |, 0x2000)
BAR (int64_t, uint64_t, or, |, 0x2000)

FOO (char, xor, ^, 0x7)
FOO (short, xor, ^, 0x2000)
FOO (int, xor, ^, 0x2000)
BAR (int, unsigned int, xor, ^, 0x2000)
FOO (int64_t, xor, ^, 0x2000)
BAR (int64_t, uint64_t, xor, ^, 0x2000)

FOO (char, shl, <<, 0x7)
FOO (short, shl, <<, 0x7)
FOO (int, shl, <<, 0x7)
BAR (int, unsigned int, shl, <<, 0x7)
FOO (int64_t, shl, <<, 0x7)
BAR (int64_t, uint64_t, shl, <<, 0x7)

FOO (char, sar, >>, 0x7)
FOO (short, sar, >>, 0x7)
FOO (int, sar, >>, 0x7)
BAR (int, unsigned int, sar, >>, 0x7)
FOO (int64_t, sar, >>, 0x7)
BAR (int64_t, uint64_t, sar, >>, 0x7)

/* { dg-final { scan-assembler-not "mov"} } */
