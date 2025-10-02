#include <stdint.h>

#if __riscv_xlen == 64
typedef unsigned __int128 uint128_t;
#endif

#define SAT_U_MUL_FMT_5(NT, WT)                 \
NT __attribute__((noinline))                    \
sat_u_mul_##NT##_from_##WT##_fmt_5 (NT a, NT b) \
{                                               \
  WT x = (WT)a * (WT)b;                         \
  NT hi = x >> (sizeof(NT) * 8);                \
  NT lo = (NT)x;                                \
  return lo | -!!hi;                            \
}
