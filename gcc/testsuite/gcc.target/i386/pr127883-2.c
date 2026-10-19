/* PR target/127883 */
/* { dg-do run { target { ! ia32 } } } */
/* { dg-options "-O2 -masm=att" } */

/* Runtime check for the *extzvdi_shr pattern.  Garbage in the upper
   32 bits of the source must not leak into the result, and the result
   must be zero-extended to 64 bits.  */

typedef unsigned long long u64;

#define EXTRACT(POS, LEN)					\
  __attribute__((noipa)) u64					\
  e##POS (u64 x)						\
  {								\
    return (x >> POS) & ((1ULL << LEN) - 1);			\
  }

EXTRACT (1, 31)
EXTRACT (8, 24)
EXTRACT (16, 16)
EXTRACT (24, 8)
EXTRACT (31, 1)

static const u64 vals[] =
{
  0x0000000000000000ULL, 0xffffffffffffffffULL,
  0xffffffff00000000ULL, 0x00000000ffffffffULL,
  0x123456789abcdef0ULL, 0xfedcba9876543210ULL,
  0x8000000080000000ULL, 0x0000000100000001ULL,
  0xdeadbeef80000001ULL
};

#define CHECK(POS, LEN, X)					\
  do {								\
    u64 ref = ((X) & 0xffffffffULL) >> POS;			\
    if (e##POS (X) != ref)					\
      __builtin_abort ();					\
  } while (0)

int
main (void)
{
  for (unsigned i = 0; i < sizeof (vals) / sizeof (vals[0]); i++)
    {
      u64 x = vals[i];
      CHECK (1, 31, x);
      CHECK (8, 24, x);
      CHECK (16, 16, x);
      CHECK (24, 8, x);
      CHECK (31, 1, x);
    }
  return 0;
}
