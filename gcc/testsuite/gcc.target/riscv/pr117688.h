#ifndef HAVE_DEFINED_PR117688_H
#define HAVE_DEFINED_PR117688_H

#include <stdint.h>

#define DEFINE_SIGNED_SAT_ADD_RUN(T, MIN, MAX)            \
  T x, y, result;                                         \
                                                          \
  __attribute__ ((noipa)) void                            \
  foo ()                                                  \
  {                                                       \
    T sum;                                                \
    _Bool overflow = __builtin_add_overflow (x, y, &sum); \
    result = overflow ? (x < 0 ? MIN : MAX) : sum;        \
  }                                                       \
                                                          \
  int main ()                                             \
  {                                                       \
    x = MIN;                                              \
    y = -0x1;                                             \
    foo();                                                \
    if (result != (T)MIN)                                 \
      __builtin_abort ();                                 \
    return 0;                                             \
  }

#endif
