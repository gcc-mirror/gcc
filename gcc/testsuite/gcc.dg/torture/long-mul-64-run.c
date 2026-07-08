/* { dg-do run { target int128 } } */

/* Runtime behavior of the recognizer on the longhand 64x64 high-part
   idiom, checked against a 128-bit reference multiply.  Covers chains
   carrying an extra addend, where the folded form must keep the addend
   on top of the wide multiply.  */

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

/* Extra addend appended after the full chain.  */
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

/* Extra addend interleaved into the middle of the chain.  */
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
      }
  return 0;
}
