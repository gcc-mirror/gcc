/* { dg-do run { target int128 } } */

/* Runtime behavior of the recognizer on the longhand 64x64 high-part
   idiom.  Two groups: shapes that must fold, checked against a 128-bit
   reference multiply, and near misses that must not fold (each violates
   one recognizer guard: low mask value, carry shift tie, cross-half
   orientation, operand consistency), checked against their literal
   meaning computed behind volatiles.  Either way a misfold aborts.  */

typedef __UINT64_TYPE__ uint64_t;
typedef unsigned __int128 uint128_t;

/* The genuine idiom.  */
__attribute__((noipa)) uint64_t
mulh_good (uint64_t x, uint64_t y)
{
  uint64_t xl = x & 0xFFFFFFFF, xh = x >> 32;
  uint64_t yl = y & 0xFFFFFFFF, yh = y >> 32;
  uint64_t hilo = xh * yl;
  uint64_t lohi = xl * yh;
  uint64_t cross = hilo + lohi;
  uint64_t lolo = xl * yl;
  uint64_t low_sum = cross + (lolo >> 32);
  uint64_t carry = (uint64_t) (hilo > low_sum) << 32;
  return xh * yh + (low_sum >> 32) + carry;
}

/* The idiom with an extra addend appended after the full chain: the
   folded form must keep the addend on top of the wide multiply.  */
__attribute__((noipa)) uint64_t
mulh_acc (uint64_t x, uint64_t y, uint64_t acc)
{
  uint64_t xl = x & 0xFFFFFFFF, xh = x >> 32;
  uint64_t yl = y & 0xFFFFFFFF, yh = y >> 32;
  uint64_t hilo = xh * yl;
  uint64_t lohi = xl * yh;
  uint64_t cross = hilo + lohi;
  uint64_t lolo = xl * yl;
  uint64_t low_sum = cross + (lolo >> 32);
  int carry_out = hilo > low_sum;
  uint64_t carry = (uint64_t) carry_out << 32;
  return xh * yh + (low_sum >> 32) + carry + acc;
}

/* Same, with the extra addend interleaved into the middle of the
   chain.  */
__attribute__((noipa)) uint64_t
mulh_acc_interleaved (uint64_t x, uint64_t y, uint64_t acc)
{
  uint64_t xl = x & 0xFFFFFFFF, xh = x >> 32;
  uint64_t yl = y & 0xFFFFFFFF, yh = y >> 32;
  uint64_t hilo = xh * yl;
  uint64_t lohi = xl * yh;
  uint64_t cross = hilo + lohi;
  uint64_t lolo = xl * yl;
  uint64_t low_sum = cross + (lolo >> 32);
  int carry_out = hilo > low_sum;
  uint64_t carry = (uint64_t) carry_out << 32;
  return ((xh * yh + acc) + (low_sum >> 32)) + carry;
}

/* Wrong low mask (0xFFFF, not the half mask).  */
__attribute__((noipa)) uint64_t
mulh_wrong_mask (uint64_t x, uint64_t y)
{
  uint64_t xl = x & 0xFFFF, xh = x >> 32;
  uint64_t yl = y & 0xFFFF, yh = y >> 32;
  uint64_t hilo = xh * yl;
  uint64_t lohi = xl * yh;
  uint64_t cross = hilo + lohi;
  uint64_t lolo = xl * yl;
  uint64_t low_sum = cross + (lolo >> 32);
  uint64_t carry = (uint64_t) (hilo > low_sum) << 32;
  return xh * yh + (low_sum >> 32) + carry;
}

/* Wrong carry position (<< 16, not the half width).  */
__attribute__((noipa)) uint64_t
mulh_wrong_carry_shift (uint64_t x, uint64_t y)
{
  uint64_t xl = x & 0xFFFFFFFF, xh = x >> 32;
  uint64_t yl = y & 0xFFFFFFFF, yh = y >> 32;
  uint64_t hilo = xh * yl;
  uint64_t lohi = xl * yh;
  uint64_t cross = hilo + lohi;
  uint64_t lolo = xl * yl;
  uint64_t low_sum = cross + (lolo >> 32);
  uint64_t carry = (uint64_t) (hilo > low_sum) << 16;
  return xh * yh + (low_sum >> 32) + carry;
}

/* Doubled cross term (hilo + hilo, same orientation).  */
__attribute__((noipa)) uint64_t
mulh_doubled_cross (uint64_t x, uint64_t y)
{
  uint64_t xl = x & 0xFFFFFFFF, xh = x >> 32;
  uint64_t yl = y & 0xFFFFFFFF, yh = y >> 32;
  uint64_t hilo = xh * yl;
  uint64_t cross = hilo + hilo;
  uint64_t lolo = xl * yl;
  uint64_t low_sum = cross + (lolo >> 32);
  uint64_t carry = (uint64_t) (hilo > low_sum) << 32;
  return xh * yh + (low_sum >> 32) + carry;
}

/* Third operand sneaks into one cross term.  */
__attribute__((noipa)) uint64_t
mulh_mixed_ops (uint64_t x, uint64_t y, uint64_t z)
{
  uint64_t xl = x & 0xFFFFFFFF, xh = x >> 32;
  uint64_t yl = y & 0xFFFFFFFF, yh = y >> 32;
  uint64_t zh = z >> 32;
  uint64_t hilo = xh * yl;
  uint64_t lohi = xl * zh;
  uint64_t cross = hilo + lohi;
  uint64_t lolo = xl * yl;
  uint64_t low_sum = cross + (lolo >> 32);
  uint64_t carry = (uint64_t) (hilo > low_sum) << 32;
  return xh * yh + (low_sum >> 32) + carry;
}

/* What each (mis)shaped source literally means, computed behind
   volatiles so no folding applies.  */
__attribute__((noipa)) uint64_t
ref_eval (uint64_t x, uint64_t y, uint64_t z, int variant)
{
  volatile uint64_t vx = x, vy = y, vz = z;
  uint64_t xh = vx >> 32, yl0 = vy & 0xFFFFFFFF, yh = vy >> 32, zh = vz >> 32;
  uint64_t xl, yl;
  switch (variant)
    {
    case 1: xl = vx & 0xFFFF; yl = vy & 0xFFFF; break;
    default: xl = vx & 0xFFFFFFFF; yl = yl0; break;
    }
  uint64_t hilo = xh * yl;
  uint64_t lohi;
  switch (variant)
    {
    case 3: lohi = hilo; break;
    case 4: lohi = xl * zh; break;
    default: lohi = xl * yh; break;
    }
  uint64_t cross = hilo + lohi;
  uint64_t lolo = xl * yl;
  uint64_t low_sum = cross + (lolo >> 32);
  uint64_t shift = (variant == 2) ? 16 : 32;
  uint64_t carry = (uint64_t) (hilo > low_sum) << shift;
  return xh * yh + (low_sum >> 32) + carry;
}

int
main (void)
{
  static const uint64_t vals[] = {
    0, 1, 0xFFFFFFFFULL, 0x100000000ULL, 0xFFFFFFFFFFFFFFFFULL,
    0xDEADBEEFCAFEBABEULL, 0x8000000080000000ULL, 0x00000001FFFFFFFFULL
  };
  const int n = sizeof (vals) / sizeof (vals[0]);
  for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++)
      {
	uint64_t x = vals[i], y = vals[j], z = vals[(i + j) % n];
	uint64_t hi = (uint64_t) (((uint128_t) x * y) >> 64);
	if (mulh_good (x, y) != hi)
	  __builtin_abort ();
	if (mulh_acc (x, y, z) != hi + z)
	  __builtin_abort ();
	if (mulh_acc_interleaved (x, y, z) != hi + z)
	  __builtin_abort ();
	if (mulh_wrong_mask (x, y) != ref_eval (x, y, z, 1))
	  __builtin_abort ();
	if (mulh_wrong_carry_shift (x, y) != ref_eval (x, y, z, 2))
	  __builtin_abort ();
	if (mulh_doubled_cross (x, y) != ref_eval (x, y, z, 3))
	  __builtin_abort ();
	if (mulh_mixed_ops (x, y, z) != ref_eval (x, y, z, 4))
	  __builtin_abort ();
      }
  return 0;
}
