/* { dg-do run { target int128 } } */

/* Runtime correctness for the full 128-bit pipeline: forwprop folds the
   longhand to (u256) x * (u256) y >> 128 and widening_mul lowers it back
   to a 128-bit longhand.  mulh_reference stays unfolded via volatiles.  */

typedef __uint128_t u128;

/* The recognized longhand high-part multiply, shared by the callers below.
   static inline so each caller inlines a copy, exposing its own chain to
   forwprop.  */
static inline u128
mulh_inline (u128 x, u128 y)
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

/* Standalone folded instance (noipa keeps it distinct from the inline
   copies), validated against the unfolded reference.  */
__attribute__((noipa)) u128
mulh_folded (u128 x, u128 y)
{
  return mulh_inline (x, y);
}

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

/* Two mulh calls sharing an operand: inlining exposes both chains and
   VN CSEs the shared (u256) cast.  Guard that lowering the first chain
   does not free a cast the second still references.  */
__attribute__((noipa)) u128
mulh_shared_xor (u128 x, u128 y, u128 z)
{
  return mulh_inline (x, y) ^ mulh_inline (x, z);
}

/* One chain's high half feeding the next, first high part also live.  After
   recognition the second chain multiplies by the first product shifted down;
   truncating that shift would leave the product live past its own lowering.  */
__attribute__((noipa)) u128
mulh_chain_high (u128 x, u128 y, u128 z, u128 *first)
{
  u128 h1 = mulh_inline (x, y);
  *first = h1;
  return mulh_inline (h1, z);
}

/* Squaring: VN CSEs the two (u256) casts of x, so lowering sees the
   same stmt on both operand-cast slots.  */
__attribute__((noipa)) u128
mulh_square (u128 x)
{
  return mulh_inline (x, x);
}

int
main (void)
{
  static const u128 vals[] = {
    0,
    1,
    (u128)0xFFFFFFFFFFFFFFFF,			/* low half all-ones */
    ((u128)1 << 64),				/* 2^64 */
    ((u128)1 << 127),				/* high bit */
    ~(u128)0,					/* all-ones */
    ((u128)0xDEADBEEFCAFEBABE << 64) | 0x0123456789ABCDEF,
    ((u128)0x8000000000000001 << 64) | 0xFFFFFFFFFFFFFFFE,
  };
  const unsigned n = sizeof (vals) / sizeof (vals[0]);

  for (unsigned i = 0; i < n; i++)
    for (unsigned j = 0; j < n; j++)
      {
	u128 x = vals[i], y = vals[j];
	if (mulh_folded (x, y) != mulh_reference (x, y))
	  __builtin_abort ();
	for (unsigned k = 0; k < n; k++)
	  {
	    u128 z = vals[k];
	    u128 want = mulh_reference (x, y) ^ mulh_reference (x, z);
	    if (mulh_shared_xor (x, y, z) != want)
	      __builtin_abort ();
	    u128 first = 0;
	    u128 h1 = mulh_reference (x, y);
	    if (mulh_chain_high (x, y, z, &first) != mulh_reference (h1, z)
		|| first != h1)
	      __builtin_abort ();
	  }
	if (mulh_square (x) != mulh_reference (x, x))
	  __builtin_abort ();
      }
  return 0;
}
