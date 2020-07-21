#include "p9-vec-length-8.h"

#define run(TYPE)                                                              \
  {                                                                            \
    unsigned int i = 0;                                                        \
                                                                               \
    TYPE out_##TYPE[N];                                                        \
    TYPE in_##TYPE[N * 2];                                                     \
    for (int i = 0; i < N; ++i) {                                              \
      out_##TYPE[i] = i * 7 / 2;                                               \
    }                                                                          \
    for (int i = 0; i < N * 2; ++i) {                                          \
      in_##TYPE[i] = i * 9 / 2;                                                \
    }                                                                          \
                                                                               \
    test_##TYPE(out_##TYPE, in_##TYPE);                                        \
    for (int i = 0; i < N; ++i) {                                              \
      TYPE expected = i * 7 / 2 + in_##TYPE[i * 2];                            \
      if (out_##TYPE[i] != expected)                                           \
        __builtin_abort();                                                     \
    }                                                                          \
  }

int main(void) {
  TEST_ALL(run)
  return 0;
}
