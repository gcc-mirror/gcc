/* PR tree-optimization/115392 */
/* { dg-do run } */
/* { dg-options "-O2" } */

/* Run-time check of (X op A) cmp C -> X cmp C, op in { |, ^ }.

   Every function has a *_ref twin that computes the same thing, but gets
   A through a volatile variable, so that the compiler cannot see its
   value and cannot apply the fold.  The two must always agree.
*/

#define NOIPA __attribute__((noipa))

volatile int k7 = 7;
volatile int km8 = -8;
volatile int kmin = -__INT_MAX__ - 1;
volatile int kmax = __INT_MAX__;

/* Fold applies: A is a constant, low bits of C all ones (le, gt)
   or all zeros (lt, ge).  Includes negative C.  */
NOIPA int le_ior (int x)     { return (x | 7) <= 15; }
NOIPA int le_ior_ref (int x) { return (x | k7) <= 15; }
NOIPA int gt_xor (int x)     { return (x ^ 7) > -9; }
NOIPA int gt_xor_ref (int x) { return (x ^ k7) > -9; }
NOIPA int lt_ior (int x)     { return (x | 7) < 16; }
NOIPA int lt_ior_ref (int x) { return (x | k7) < 16; }
NOIPA int ge_xor (int x)     { return (x ^ 7) >= -8; }
NOIPA int ge_xor_ref (int x) { return (x ^ k7) >= -8; }

/* Same with unsigned arithmetic.  */
NOIPA int gt_xor_u (int x)
{ return ((unsigned) x ^ 7) > (unsigned) -9; }

NOIPA int gt_xor_u_ref (int x)
{ return ((unsigned) x ^ k7) > (unsigned) -9; }

NOIPA int lt_ior_u (int x)
{ return ((unsigned) x | 7) < (unsigned) -8; }

NOIPA int lt_ior_u_ref (int x)
{ return ((unsigned) x | k7) < (unsigned) -8; }

/* 'A' has every non-sign bit set.  */
NOIPA int lt_ior_max (int x)     { return (x | __INT_MAX__) < 0; }
NOIPA int lt_ior_max_ref (int x) { return (x | kmax) < 0; }
NOIPA int ge_xor_max (int x)     { return (x ^ __INT_MAX__) >= 0; }
NOIPA int ge_xor_max_ref (int x) { return (x ^ kmax) >= 0; }

/* Low bits of C dont satisfy fold condition.  */
NOIPA int no_le (int x)     { return (x | 7) <= 5; }
NOIPA int no_le_ref (int x) { return (x | k7) <= 5; }
NOIPA int no_ge (int x)     { return (x ^ 7) >= 9; }
NOIPA int no_ge_ref (int x) { return (x ^ k7) >= 9; }

/* 'A' has the sign bit set, the fold doesn't apply.  */
NOIPA int sign_min (int x)     { return (x ^ (-__INT_MAX__ - 1)) <= -1; }
NOIPA int sign_min_ref (int x) { return (x ^ kmin) <= -1; }
NOIPA int sign_m8 (int x)      { return (x | -8) <= -1; }
NOIPA int sign_m8_ref (int x)  { return (x | km8) <= -1; }

/* A is not constant but known to be in [0, 7], fold applies.  */
NOIPA int le_mask (int x, int a)     { return (x | (a & 7)) <= 15; }
NOIPA int le_mask_ref (int x, int a) { return (x | (a & k7)) <= 15; }
NOIPA int gt_mask (int x, int a)     { return (x ^ (a & 7)) > 7; }
NOIPA int gt_mask_ref (int x, int a) { return (x ^ (a & k7)) > 7; }
NOIPA int lt_mask (int x, int a)     { return (x | (a & 7)) < 8; }
NOIPA int lt_mask_ref (int x, int a) { return (x | (a & k7)) < 8; }
NOIPA int ge_mask (int x, int a)     { return (x ^ (a & 7)) >= -16; }
NOIPA int ge_mask_ref (int x, int a) { return (x ^ (a & k7)) >= -16; }

#define CHECK_X(F) \
if (F (x) != F##_ref (x)) \
  __builtin_abort ()

#define CHECK_X_A(F) \
if (F (x, a) != F##_ref (x, a)) \
  __builtin_abort ()

void check (int x)
{
  CHECK_X (gt_xor);
  CHECK_X (le_ior);
  CHECK_X (lt_ior);
  CHECK_X (ge_xor);
  CHECK_X (gt_xor_u);
  CHECK_X (lt_ior_u);
  CHECK_X (lt_ior_max);
  CHECK_X (ge_xor_max);
  CHECK_X (no_le);
  CHECK_X (no_ge);
  CHECK_X (sign_min);
  CHECK_X (sign_m8);

  for (int a = -8; a <= 8; a++)
  {
    CHECK_X_A (le_mask);
    CHECK_X_A (gt_mask);
    CHECK_X_A (lt_mask);
    CHECK_X_A (ge_mask);
  }
}

int main ()
{
  /* Small values, negative and positive. */
  for (int x = -32; x <= 32; x++)
    check (x);

  /* Large values, negative and positive. */
  check (-__INT_MAX__ - 1);
  check (-__INT_MAX__);
  check (-__INT_MAX__ / 2 - 1);
  check (-__INT_MAX__ / 2);
  check (__INT_MAX__ / 2);
  check (__INT_MAX__ / 2 + 1);
  check (__INT_MAX__ - 1);
  check (__INT_MAX__);

  return 0;
}
