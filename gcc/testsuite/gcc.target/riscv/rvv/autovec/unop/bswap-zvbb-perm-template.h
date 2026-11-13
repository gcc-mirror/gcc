#include <stdint-gcc.h>

typedef uint8_t v16qi __attribute__ ((vector_size (16)));
typedef uint16_t v8hi __attribute__ ((vector_size (16)));

#define BSWAP16_QI 1, 0, 3, 2, 5, 4, 7, 6, 9, 8, 11, 10, 13, 12, 15, 14
#define BSWAP32_QI 3, 2, 1, 0, 7, 6, 5, 4, 11, 10, 9, 8, 15, 14, 13, 12
#define BSWAP64_QI 7, 6, 5, 4, 3, 2, 1, 0, 15, 14, 13, 12, 11, 10, 9, 8
#define REVERSE_QI 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0
#define SWAPHI_QI 2, 3, 0, 1, 6, 7, 4, 5, 10, 11, 8, 9, 14, 15, 12, 13
#define SWAPPAIR_HI 1, 0, 3, 2, 5, 4, 7, 6

#define PERM(NAME, VT, IDX)                                                    \
  static const int idx_##NAME[] = { IDX };                                     \
  __attribute__ ((noipa)) VT perm_##NAME (VT x)                                \
  {                                                                            \
    return __builtin_shufflevector (x, x, IDX);                                \
  }

#define TEST_ALL()                                                             \
  PERM (bswap16, v16qi, BSWAP16_QI)                                            \
  PERM (bswap32, v16qi, BSWAP32_QI)                                            \
  PERM (bswap64, v16qi, BSWAP64_QI)                                            \
  PERM (reverse, v16qi, REVERSE_QI)                                            \
  PERM (swaphi, v16qi, SWAPHI_QI)                                              \
  PERM (swappair, v8hi, SWAPPAIR_HI)

TEST_ALL ()
