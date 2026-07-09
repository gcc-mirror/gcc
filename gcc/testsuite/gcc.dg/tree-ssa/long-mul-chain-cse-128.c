/* { dg-do compile { target { oi_mode && int128 } } } */
/* { dg-options "-O3 -fdump-tree-forwprop1-details -fdump-tree-optimized" } */

/* Two differently-spelled high-part longhands of the same 128x128
   product both fold to the canonical (u256) x * (u256) y >> 128, so
   value numbering proves them equal and the function folds to 0.  */

typedef __uint128_t u128;

u128 both_spellings (u128 x, u128 y)
{
  /* Spelling 1: overflow-compare carry form.  */
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
  u128 h1 = cond + (add_cross_sum >> 64);

  /* Spelling 2: ladder form.  */
  u128 a_lo = x & (u128)0xFFFFFFFFFFFFFFFF;
  u128 b_lo = y & (u128)0xFFFFFFFFFFFFFFFF;
  u128 a_hi = x >> 64;
  u128 b_hi = y >> 64;
  u128 t0 = b_lo * a_lo;
  u128 t1 = b_lo * a_hi;
  u128 t2 = b_hi * a_lo;
  u128 t3 = b_hi * a_hi;
  u128 t0_hi = t0 >> 64;
  u128 u0 = t0_hi + t1;
  u128 u0_lo = u0 & (u128)0xFFFFFFFFFFFFFFFF;
  u128 u0_hi = u0 >> 64;
  u128 u1 = u0_lo + t2;
  u128 u1_hi = u1 >> 64;
  u128 u2 = u0_hi + t3;
  u128 h2 = u2 + u1_hi;

  return h1 ^ h2;
}

/* Both spellings are recognized.  */
/* { dg-final { scan-tree-dump-times "Long multiplication high part folded." 2 "forwprop1" } } */
/* Once canonical, VN proves them equal and the function folds to 0.  */
/* { dg-final { scan-tree-dump "return 0;" "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\* " "optimized" } } */
