/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-forwprop-details" } */

/* Chains carrying addends that are not part of the long-multiply idiom
   (an accumulator merged in by reassociation, a foreign shifted term)
   must still fold, with the extra addends preserved on top of the wide
   multiply.  A leaf that classifies as a long-mul summand but breaks
   operand consistency kills the match instead (no subset retry).  */

typedef __UINT64_TYPE__ uint64_t;

/* Extra addend appended after the full high-part chain.  Folds.  */
uint64_t
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

/* Extra addend interleaved into the middle of the chain.  Folds.  */
uint64_t
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

/* Foreign leaf that classifies as a summand (a second high-high
   product of different operands): consumed by the multiset, operand
   consistency fails, no fold.  */
uint64_t
mulh_foreign_hihi (uint64_t x, uint64_t y, uint64_t a, uint64_t b)
{
  uint64_t xl = x & 0xFFFFFFFF, xh = x >> 32;
  uint64_t yl = y & 0xFFFFFFFF, yh = y >> 32;
  uint64_t ah = a >> 32, bh = b >> 32;
  uint64_t hilo = xh * yl;
  uint64_t lohi = xl * yh;
  uint64_t cross = hilo + lohi;
  uint64_t lolo = xl * yl;
  uint64_t low_sum = cross + (lolo >> 32);
  int carry_out = hilo > low_sum;
  uint64_t carry = (uint64_t) carry_out << 32;
  return xh * yh + (low_sum >> 32) + carry + ah * bh;
}

/* { dg-final { scan-tree-dump-times "Long multiplication high part folded" 2 "forwprop1" } } */
