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

#define DEFINE_SIGNED_SAT_SUB_RUN(T, MIN, MAX)              \
  T x, y, result;                                           \
                                                            \
  __attribute__ ((noipa)) void                              \
  foo ()                                                    \
  {                                                         \
    T minus;                                                \
    _Bool overflow = __builtin_sub_overflow (x, y, &minus); \
    result = overflow ? (x < 0 ? MIN : MAX) : minus;        \
  }                                                         \
                                                            \
  int main ()                                               \
  {                                                         \
    x = MIN;                                                \
    y = 0x1;                                                \
    foo();                                                  \
    if (result != (T)MIN)                                   \
      __builtin_abort ();                                   \
    return 0;                                               \
  }

#define DEFINE_SIGNED_SAT_TRUNC_RUN(WT, NT, NT_MIN, NT_MAX) \
  WT x;                                                     \
  NT result;                                                \
                                                            \
  __attribute__ ((noipa)) void                              \
  foo ()                                                    \
  {                                                         \
    NT trunc = (NT)x;                                       \
    result = (WT)NT_MIN <= x && x <= (WT)NT_MAX             \
       ? trunc                                              \
       : x < 0 ? NT_MIN : NT_MAX;                           \
  }                                                         \
                                                            \
  int main ()                                               \
  {                                                         \
    x = (WT)NT_MIN - 1 ;                                    \
    foo();                                                  \
    if (result != (NT)NT_MIN)                               \
      __builtin_abort ();                                   \
    return 0;                                               \
  }

#endif
