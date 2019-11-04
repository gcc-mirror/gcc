#ifndef AUTOVEC_H
#define AUTOVEC_H 1

#define QUIET_EQ(x, y) ((x) == (y))
#define QUIET_GE __builtin_isgreaterequal
#define QUIET_GT __builtin_isgreater
#define QUIET_LE __builtin_islessequal
#define QUIET_LT __builtin_isless
#define QUIET_ORDERED(x, y) (!__builtin_isunordered ((x), (y)))
#define QUIET_UNEQ(x, y) (__builtin_isless ((x), (y)) \
                          || __builtin_isgreater ((x), (y)))
#define QUIET_UNORDERED __builtin_isunordered
#define SIGNALING_EQ(x, y) (((x) <= (y)) && ((x) >= (y)))
#define SIGNALING_GE(x, y) ((x) >= (y))
#define SIGNALING_GT(x, y) ((x) > (y))
#define SIGNALING_LE(x, y) ((x) <= (y))
#define SIGNALING_LT(x, y) ((x) < (y))
#define SIGNALING_LTGT(x, y) (((x) < (y)) || ((x) > (y)))

#define AUTOVEC(RESULT_TYPE, OP_TYPE, OP) void \
f (RESULT_TYPE *r, const OP_TYPE *x, const OP_TYPE *y) \
{ \
  int i; \
\
  for (i = 0; i < 1000000; i++) \
    { \
      OP_TYPE xi = x[i], yi = y[i]; \
\
      r[i] = OP (xi, yi); \
    } \
}

#define AUTOVEC_DOUBLE(OP) AUTOVEC (long long, double, OP)

#define AUTOVEC_FLOAT(OP) AUTOVEC (int, float, OP)

#ifdef __SIZEOF_INT128__
typedef __int128 v1ti __attribute__ ((vector_size (16)));
typedef long double v1tf __attribute__ ((vector_size (16)));
#define AUTOVEC_LONG_DOUBLE(OP) AUTOVEC (v1ti, v1tf, OP)
#endif

#endif
