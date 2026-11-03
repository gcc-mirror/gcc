#include <stdint-gcc.h>

/* Every source/destination mode pair accepted by the ustrunc expanders.
   The HI/SI/DI sources are uint16_t/uint32_t/uint64_t; DI exists only on
   RV64.  */
#if __riscv_xlen == 64
#define USTRUNC_PAIRS(X)                                                \
  X (uint8_t, uint16_t)                                                 \
  X (uint16_t, uint32_t)                                                \
  X (uint8_t, uint32_t)                                                 \
  X (uint32_t, uint64_t)                                                \
  X (uint16_t, uint64_t)                                                \
  X (uint8_t, uint64_t)
#else
#define USTRUNC_PAIRS(X)                                                \
  X (uint8_t, uint16_t)                                                 \
  X (uint16_t, uint32_t)                                                \
  X (uint8_t, uint32_t)
#endif

/* The source is passed as its own type.  */
#define DEF_SAT(DST, SRC)                                               \
DST __attribute__((noinline))                                           \
sat_##SRC##_##DST (SRC x)                                               \
{                                                                       \
  return x > (SRC)(DST)(-1) ? (DST)(-1) : (DST)x;                       \
}

/* The source is built by truncating a 64-bit value, so the upper bits of its
   register are not guaranteed to be an extension of the value.  */
#define DEF_SAT_TRUNC(DST, SRC)                                         \
DST __attribute__((noinline))                                           \
sat_trunc_##SRC##_##DST (uint64_t y)                                    \
{                                                                       \
  SRC x = (SRC)y;                                                       \
  return x > (SRC)(DST)(-1) ? (DST)(-1) : (DST)x;                       \
}
