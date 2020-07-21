#include "p9-vec-length-7.h"

#define run(TYPE)                                                              \
  {                                                                            \
    unsigned int i = 0;                                                        \
    test_npeel_##TYPE();                                                       \
    for (int i = 0; i < N; ++i) {                                              \
      if (x_##TYPE[i] != (i < START || i >= END ? 0 : (i - START)))            \
        __builtin_abort();                                                     \
    }                                                                          \
  }

int main() {
  TEST_ALL(run)
  return 0;
}
