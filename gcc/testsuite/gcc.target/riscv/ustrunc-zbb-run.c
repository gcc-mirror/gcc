/* { dg-do run { target { rv32 || rv64 } } } */
/* { dg-options "-march=rv64gc_zbb -mabi=lp64d -O2" { target { rv64 } } } */
/* { dg-options "-march=rv32gc_zbb -mabi=ilp32d -O2" { target { rv32 } } } */

#include "ustrunc-zbb.h"

USTRUNC_PAIRS (DEF_SAT)
USTRUNC_PAIRS (DEF_SAT_TRUNC)

/* Reference.  The volatile load keeps the compiler from recognizing the
   comparison as a saturation.  */
uint64_t __attribute__((noinline))
ref (uint64_t x, uint64_t max)
{
  volatile uint64_t m = max;
  return x > m ? m : x;
}

static const uint64_t vals[] = {
  0, 1, 2,
  0x7e, 0x7f, 0x80, 0x81, 0xfe, 0xff, 0x100, 0x101,
  0x7ffe, 0x7fff, 0x8000, 0x8001, 0xfffe, 0xffff, 0x10000, 0x10001,
  0x7ffffffe, 0x7fffffff, 0x80000000, 0x80000001, 0xfffffffe, 0xffffffff,
  0x100000000, 0x100000001, 0x1000000ff, 0x1ffffffff,
  0x7fffffffffffffff, 0x8000000000000000, 0x8000000000000001,
  0xfffffffffffffffe, 0xffffffffffffffff,
  0xffffffff80000000, 0xffffffff7fffffff, 0xffffffffffffff00,
  0xffffffff00000100, 0x12345678abcdef01
};

#define CHECK(DST, SRC)                                                 \
  {                                                                     \
    SRC s = (SRC)v;                                                     \
    DST e = (DST)ref ((uint64_t)s, (uint64_t)(DST)(-1));                \
    if (sat_##SRC##_##DST (s) != e)                                     \
      __builtin_abort ();                                               \
    if (sat_trunc_##SRC##_##DST (v) != e)                               \
      __builtin_abort ();                                               \
  }

int
main (void)
{
  for (unsigned i = 0; i < sizeof (vals) / sizeof (vals[0]); i++)
    {
      uint64_t v = vals[i];
      USTRUNC_PAIRS (CHECK)
    }
  return 0;
}
