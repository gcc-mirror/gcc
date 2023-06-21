/* { dg-do run } */
/* { dg-options "-O2 -g" } */

int
main ()
{
  unsigned int c;
  unsigned long cl;
  unsigned long long cll;
  if (__builtin_addc (1, 42, 0, &c) != 43 || c != 0)
    __builtin_abort ();
  if (__builtin_addc (1, 42, 15, &c) != 58 || c != 0)
    __builtin_abort ();
  if (__builtin_addc (-2U, -3U, -4U, &c) != -9U || c != 1)
    __builtin_abort ();
  if (__builtin_addc (-2U, 1, 0, &c) != -1U || c != 0)
    __builtin_abort ();
  if (__builtin_addc (-2U, 1, 1, &c) != 0 || c != 1)
    __builtin_abort ();
  if (__builtin_addc (-2U, 2, 0, &c) != 0 || c != 1)
    __builtin_abort ();
  if (__builtin_addc (-2U, 0, 2, &c) != 0 || c != 1)
    __builtin_abort ();
  if (__builtin_addcl (1L, 42L, 0L, &cl) != 43 || cl != 0L)
    __builtin_abort ();
  if (__builtin_addcl (1L, 42L, 15L, &cl) != 58 || cl != 0L)
    __builtin_abort ();
  if (__builtin_addcl (-2UL, -3UL, -4UL, &cl) != -9UL || cl != 1L)
    __builtin_abort ();
  if (__builtin_addcl (-2UL, 1L, 0L, &cl) != -1UL || cl != 0L)
    __builtin_abort ();
  if (__builtin_addcl (-2UL, 1L, 1L, &cl) != 0 || cl != 1L)
    __builtin_abort ();
  if (__builtin_addcl (-2UL, 2L, 0L, &cl) != 0 || cl != 1L)
    __builtin_abort ();
  if (__builtin_addcl (-2UL, 0L, 2L, &cl) != 0 || cl != 1L)
    __builtin_abort ();
  if (__builtin_addcll (1LL, 42LL, 0LL, &cll) != 43 || cll != 0LL)
    __builtin_abort ();
  if (__builtin_addcll (1LL, 42LL, 15LL, &cll) != 58 || cll != 0LL)
    __builtin_abort ();
  if (__builtin_addcll (-2ULL, -3ULL, -4ULL, &cll) != -9ULL || cll != 1LL)
    __builtin_abort ();
  if (__builtin_addcll (-2ULL, 1LL, 0LL, &cll) != -1ULL || cll != 0LL)
    __builtin_abort ();
  if (__builtin_addcll (-2ULL, 1LL, 1LL, &cll) != 0 || cll != 1LL)
    __builtin_abort ();
  if (__builtin_addcll (-2ULL, 2LL, 0LL, &cll) != 0 || cll != 1LL)
    __builtin_abort ();
  if (__builtin_addcll (-2ULL, 0LL, 2LL, &cll) != 0 || cll != 1LL)
    __builtin_abort ();
  if (__builtin_subc (42, 42, 0, &c) != 0 || c != 0)
    __builtin_abort ();
  if (__builtin_subc (42, 42, 1, &c) != -1U || c != 1)
    __builtin_abort ();
  if (__builtin_subc (1, -3U, -4U, &c) != 8 || c != 1)
    __builtin_abort ();
  if (__builtin_subc (-2U, 1, 0, &c) != -3U || c != 0)
    __builtin_abort ();
  if (__builtin_subc (-2U, -1U, 0, &c) != -1U || c != 1)
    __builtin_abort ();
  if (__builtin_subc (-2U, -2U, 0, &c) != 0 || c != 0)
    __builtin_abort ();
  if (__builtin_subc (-2U, -2U, 1, &c) != -1U || c != 1)
    __builtin_abort ();
  if (__builtin_subc (-2U, 1, -2U, &c) != -1U || c != 1)
    __builtin_abort ();
  if (__builtin_subcl (42L, 42L, 0L, &cl) != 0L || cl != 0L)
    __builtin_abort ();
  if (__builtin_subcl (42L, 42L, 1L, &cl) != -1UL || cl != 1L)
    __builtin_abort ();
  if (__builtin_subcl (1L, -3UL, -4UL, &cl) != 8L || cl != 1L)
    __builtin_abort ();
  if (__builtin_subcl (-2UL, 1L, 0L, &cl) != -3UL || cl != 0L)
    __builtin_abort ();
  if (__builtin_subcl (-2UL, -1UL, 0L, &cl) != -1UL || cl != 1L)
    __builtin_abort ();
  if (__builtin_subcl (-2UL, -2UL, 0L, &cl) != 0L || cl != 0L)
    __builtin_abort ();
  if (__builtin_subcl (-2UL, -2UL, 1L, &cl) != -1UL || cl != 1L)
    __builtin_abort ();
  if (__builtin_subcl (-2UL, 1L, -2UL, &cl) != -1UL || cl != 1L)
    __builtin_abort ();
  if (__builtin_subcll (42LL, 42LL, 0LL, &cll) != 0LL || cll != 0LL)
    __builtin_abort ();
  if (__builtin_subcll (42LL, 42LL, 1LL, &cll) != -1ULL || cll != 1LL)
    __builtin_abort ();
  if (__builtin_subcll (1LL, -3ULL, -4ULL, &cll) != 8LL || cll != 1LL)
    __builtin_abort ();
  if (__builtin_subcll (-2ULL, 1LL, 0LL, &cll) != -3ULL || cll != 0LL)
    __builtin_abort ();
  if (__builtin_subcll (-2ULL, -1ULL, 0LL, &cll) != -1ULL || cll != 1LL)
    __builtin_abort ();
  if (__builtin_subcll (-2ULL, -2ULL, 0LL, &cll) != 0LL || cll != 0LL)
    __builtin_abort ();
  if (__builtin_subcll (-2ULL, -2ULL, 1LL, &cll) != -1ULL || cll != 1LL)
    __builtin_abort ();
  if (__builtin_subcll (-2ULL, 1LL, -2ULL, &cll) != -1ULL || cll != 1LL)
    __builtin_abort ();
  return 0;
}
