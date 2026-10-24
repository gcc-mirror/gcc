/* PR tree-optimization/128117 */
/* { dg-do run } */
/* { dg-options "-O2 -fdump-tree-forwprop1-details" } */

typedef __UINT64_TYPE__ uint64_t;

/* Adds the high half of t2 where that of t1 is needed.
   Should NOT be folded.  */
__attribute__((noipa))
uint64_t mulh_ladder_wrong_hilo (uint64_t x, uint64_t y)
{
  uint64_t x_lo = x & 0xFFFFFFFF;
  uint64_t y_lo = y & 0xFFFFFFFF;
  uint64_t x_hi = x >> 32;
  uint64_t y_hi = y >> 32;
  uint64_t t0 = y_lo * x_lo;
  uint64_t t1 = y_lo * x_hi;
  uint64_t t2 = y_hi * x_lo;
  uint64_t t3 = y_hi * x_hi;
  uint64_t t0_hi = t0 >> 32;
  uint64_t t1_lo = t1 & 0xFFFFFFFF;
  uint64_t u0 = t1_lo + t2;
  uint64_t u1 = u0 + t0_hi;
  uint64_t u1_hi = u1 >> 32;
  uint64_t t2_hi = t2 >> 32;
  uint64_t u2 = t2_hi + t3;
  uint64_t hw64 = u2 + u1_hi;

  return hw64;
}

/* As above, but adding t0_hi before t2.
   Should NOT be folded.  */
__attribute__((noipa))
uint64_t mulh_ladder_wrong_hilo_assoc (uint64_t x, uint64_t y)
{
  uint64_t x_lo = x & 0xFFFFFFFF;
  uint64_t y_lo = y & 0xFFFFFFFF;
  uint64_t x_hi = x >> 32;
  uint64_t y_hi = y >> 32;
  uint64_t t0 = y_lo * x_lo;
  uint64_t t1 = y_lo * x_hi;
  uint64_t t2 = y_hi * x_lo;
  uint64_t t3 = y_hi * x_hi;
  uint64_t t0_hi = t0 >> 32;
  uint64_t t1_lo = t1 & 0xFFFFFFFF;
  uint64_t u0 = t1_lo + t0_hi;
  uint64_t u1 = u0 + t2;
  uint64_t u1_hi = u1 >> 32;
  uint64_t t2_hi = t2 >> 32;
  uint64_t u2 = t2_hi + t3;
  uint64_t hw64 = u2 + u1_hi;

  return hw64;
}

/* As mulh_ladder_wrong_hilo, with x and y swapped.
   Should NOT be folded.  */
__attribute__((noipa))
uint64_t mulh_ladder_wrong_hilo_comm (uint64_t x, uint64_t y)
{
  uint64_t x_lo = x & 0xFFFFFFFF;
  uint64_t y_lo = y & 0xFFFFFFFF;
  uint64_t x_hi = x >> 32;
  uint64_t y_hi = y >> 32;
  uint64_t t0 = x_lo * y_lo;
  uint64_t t1 = x_lo * y_hi;
  uint64_t t2 = x_hi * y_lo;
  uint64_t t3 = x_hi * y_hi;
  uint64_t t0_hi = t0 >> 32;
  uint64_t t1_lo = t1 & 0xFFFFFFFF;
  uint64_t u0 = t1_lo + t2;
  uint64_t u1 = u0 + t0_hi;
  uint64_t u1_hi = u1 >> 32;
  uint64_t t2_hi = t2 >> 32;
  uint64_t u2 = t2_hi + t3;
  uint64_t hw64 = u2 + u1_hi;

  return hw64;
}

/* Adds the high half of v0 where that of u0 is needed.
   Should NOT be folded.  */
__attribute__((noipa))
uint64_t mulh_ladder_wrong_part_sum (uint64_t x, uint64_t y)
{
  uint64_t x_lo = x & 0xFFFFFFFF;
  uint64_t y_lo = y & 0xFFFFFFFF;
  uint64_t x_hi = x >> 32;
  uint64_t y_hi = y >> 32;
  uint64_t t0 = y_lo * x_lo;
  uint64_t t1 = y_lo * x_hi;
  uint64_t t2 = y_hi * x_lo;
  uint64_t t3 = y_hi * x_hi;
  uint64_t t0_hi = t0 >> 32;
  uint64_t u0 = t0_hi + t1;
  uint64_t u0_lo = u0 & 0xFFFFFFFF;
  uint64_t u1 = u0_lo + t2;
  uint64_t u1_hi = u1 >> 32;
  uint64_t v0 = t0_hi + t2;
  uint64_t v0_hi = v0 >> 32;
  uint64_t u2 = v0_hi + t3;
  uint64_t hw64 = u2 + u1_hi;

  return hw64;
}

/* As mulh_ladder_wrong_part_sum, with x and y swapped.
   Should NOT be folded.  */
__attribute__((noipa))
uint64_t mulh_ladder_wrong_part_sum_comm (uint64_t x, uint64_t y)
{
  uint64_t x_lo = x & 0xFFFFFFFF;
  uint64_t y_lo = y & 0xFFFFFFFF;
  uint64_t x_hi = x >> 32;
  uint64_t y_hi = y >> 32;
  uint64_t t0 = x_lo * y_lo;
  uint64_t t1 = x_lo * y_hi;
  uint64_t t2 = x_hi * y_lo;
  uint64_t t3 = x_hi * y_hi;
  uint64_t t0_hi = t0 >> 32;
  uint64_t u0 = t0_hi + t1;
  uint64_t u0_lo = u0 & 0xFFFFFFFF;
  uint64_t u1 = u0_lo + t2;
  uint64_t u1_hi = u1 >> 32;
  uint64_t v0 = t0_hi + t2;
  uint64_t v0_hi = v0 >> 32;
  uint64_t u2 = v0_hi + t3;
  uint64_t hw64 = u2 + u1_hi;

  return hw64;
}

int main ()
{
  /* Each call returns 0.  The high part of x * y is 0xFFFFFFFE.  */
  uint64_t x = 0xFFFFFFFF00000000ULL;
  uint64_t y = 0xFFFFFFFFULL;

  if (mulh_ladder_wrong_hilo (x, y) != 0)
    __builtin_abort ();

  if (mulh_ladder_wrong_hilo_assoc (x, y) != 0)
    __builtin_abort ();

  if (mulh_ladder_wrong_hilo_comm (y, x) != 0)
    __builtin_abort ();

  if (mulh_ladder_wrong_part_sum (x, y) != 0)
    __builtin_abort ();

  if (mulh_ladder_wrong_part_sum_comm (y, x) != 0)
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-not "Long multiplication high part folded\\." "forwprop1" } } */
