#ifndef LONG_DOUBLE_WF_H
#define LONG_DOUBLE_WF_H 1

#include <math.h>

#define ADD(x, y, z) ((x) + (z))
#define DIV(x, y, z) ((x) / (z))
#define FABSL(x, y, z) (fabsl (y))
#define ISINFL(x, y, z) (isinfl (x) ? (y) : (z))
#define MUL(x, y, z) ((x) * (z))
#define MUL_ADD(x, y, z) ((x) * (y) + (z))
#define MUL_SUB(x, y, z) ((x) * (y) - (z))
#define NEG(x, y, z)                                                          \
  ({                                                                          \
    volatile long double r = -(y);                                            \
    r;                                                                        \
  })
#define NEG_MUL_ADD(x, y, z) NEG (0, MUL_ADD (x, y, z), 0)
#define NEG_MUL_SUB(x, y, z) NEG (0, MUL_SUB (x, y, z), 0)
#define QUIET_IFEQUAL(x, y, z) ((x) == (y) ? (z) : 0)
#define QUIET_IFGREATER(x, y, z) (__builtin_isgreater (x, y) ? (z) : 0)
#define QUIET_IFLESS(x, y, z) (__builtin_isless (x, y) ? (z) : 0)
#define QUIET_IFUNORDERED(x, y, z) (__builtin_isunordered (x, y) ? (z) : 0)
#define SIGNALING_IFEQUAL(x, y, z) (((x) >= (y) && (x) <= (y)) ? (z) : 0)
#define SIGNALING_IFGREATER(x, y, z) ((x) > (y) ? (z) : 0)
#define SIGNALING_IFLESS(x, y, z) ((x) < (y) ? (z) : 0)
#define ROUNDL(x, y, z) (roundl (y))
#define SQRTL(x, y, z) (sqrtl (y))
#define SUB(x, y, z) ((x) - (z))

#define LONG_DOUBLE_WF(op)                                                    \
  long double test (                                                          \
      long double x0, long double x1, long double x2, long double x3,         \
      long double x4, long double x5, long double x6, long double x7,         \
      long double x8, long double x9, long double x10, long double x11,       \
      long double x12, long double x13, long double x14, long double x15)     \
  {                                                                           \
    while (x15 < 1E+30)                                                       \
      {                                                                       \
	x0 = op (x1, x2, x3);                                                 \
	x1 = op (x2, x3, x4) + 1;                                             \
	x2 = op (x3, x4, x5) + 2;                                             \
	x3 = op (x4, x5, x6) + 3;                                             \
	x4 = op (x5, x6, x7) + 4;                                             \
	x5 = op (x6, x7, x8) + 5;                                             \
	x6 = op (x7, x8, x9) + 6;                                             \
	x7 = op (x8, x9, x10) + 7;                                            \
	x8 = op (x9, x10, x11) + 8;                                           \
	x9 = op (x10, x11, x12) + 9;                                          \
	x10 = op (x11, x12, x13) + 10;                                        \
	x11 = op (x12, x13, x14) + 11;                                        \
	x12 = op (x13, x14, x15) + 12;                                        \
	x13 = op (x14, x15, x0) + 13;                                         \
	x14 = op (x15, x0, x1) + 14;                                          \
	x15 = op (x0, x1, x2) + 15;                                           \
      }                                                                       \
    return x15;                                                               \
  }

#endif
