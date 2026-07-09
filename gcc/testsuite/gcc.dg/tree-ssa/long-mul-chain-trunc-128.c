/* { dg-do run { target int128 } } */
/* { dg-options "-O3" } */

/* A user cast of the recognized 128-bit high part merges with the
   chain's final truncation, so widening_mul lowering sees an outermost
   convert to a type narrower than 128 bits.  Checks it builds the
   longhand at narrow precision and converts to the lhs type.  */

typedef __uint128_t u128;
typedef unsigned long long u64;
typedef unsigned int u32;

static inline u128
mulh128 (u128 x, u128 y)
{
  u128 x_hi = x >> 64;
  u128 x_lo = x & (u128)0xFFFFFFFFFFFFFFFF;
  u128 y_hi = y >> 64;
  u128 y_lo = y & (u128)0xFFFFFFFFFFFFFFFF;
  u128 mulhilo = x_hi * y_lo;
  u128 mullohi = x_lo * y_hi;
  u128 cross_sum = mulhilo + mullohi;
  u128 mullolo = x_lo * y_lo;
  u128 shrlolo = mullolo >> 64;
  u128 add_cross_sum = cross_sum + shrlolo;
  int carry = add_cross_sum < mulhilo;
  u128 cond = ((u128) carry << 64) + x_hi * y_hi;
  return cond + (add_cross_sum >> 64);
}

__attribute__((noipa)) u64
trunc64 (u128 x, u128 y) { return (u64) mulh128 (x, y); }

__attribute__((noipa)) u32
trunc32 (u128 x, u128 y) { return (u32) mulh128 (x, y); }

__attribute__((noipa)) u128
mulh_reference (u128 x, u128 y)
{
  volatile u128 x_hi = x >> 64;
  volatile u128 x_lo = x & (u128)0xFFFFFFFFFFFFFFFF;
  volatile u128 y_hi = y >> 64;
  volatile u128 y_lo = y & (u128)0xFFFFFFFFFFFFFFFF;
  u128 mulhilo = x_hi * y_lo;
  u128 mullohi = x_lo * y_hi;
  u128 cross_sum = mulhilo + mullohi;
  u128 mullolo = x_lo * y_lo;
  u128 shrlolo = mullolo >> 64;
  u128 add_cross_sum = cross_sum + shrlolo;
  int carry = add_cross_sum < mulhilo;
  u128 cond = ((u128) carry << 64) + x_hi * y_hi;
  return cond + (add_cross_sum >> 64);
}

int
main (void)
{
  static const u128 vals[] = {
    0,
    1,
    (u128)0xFFFFFFFFFFFFFFFF,
    ((u128)1 << 64),
    ((u128)1 << 127),
    ~(u128)0,
    ((u128)0xDEADBEEFCAFEBABE << 64) | 0x0123456789ABCDEF,
    ((u128)0x8000000000000001 << 64) | 0xFFFFFFFFFFFFFFFE,
  };
  const unsigned n = sizeof (vals) / sizeof (vals[0]);

  for (unsigned i = 0; i < n; i++)
    for (unsigned j = 0; j < n; j++)
      {
	u128 ref = mulh_reference (vals[i], vals[j]);
	if (trunc64 (vals[i], vals[j]) != (u64) ref)
	  __builtin_abort ();
	if (trunc32 (vals[i], vals[j]) != (u32) ref)
	  __builtin_abort ();
      }
  return 0;
}
