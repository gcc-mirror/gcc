/* PR bootstrap/77676 - powerpc64 and powerpc64le stage2 bootstrap fail
   Test case derived from the one submitted in the bug.  It verifies
   that the sprintf return value (or value range) optimization is not
   performed for an unknown string.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Wall -Werror -fdump-tree-optimized -fprintf-return-value" } */

#define INT_MAX   __INT_MAX__
#define INT_MIN   (-INT_MAX - 1)

extern void string_eq_min_fail ();
extern void string_eq_max_fail ();

extern void string_lt_0_fail ();
extern void string_eq_0_fail ();
extern void string_gt_0_fail ();

void test_string (char *d, const char *s)
{
  int n = __builtin_sprintf (d, "%-s", s);

  /* Verify that the return value is NOT assumed NOT to be INT_MIN
     or INT_MAX.  (This is a white box test based on knowing that
     the optimization computes its own values of the two constants.)  */
  if (n == INT_MIN) string_eq_min_fail ();
  if (n == INT_MAX) string_eq_max_fail ();

  /* The return value could be negative when strlen(s) is in excess
     of 4095 (the maximum number of bytes a single directive is required
     to handle).  */
  if (n < 0) string_lt_0_fail ();
  if (n == 0) string_eq_0_fail ();
  if (n > 0) string_gt_0_fail ();
}

/* { dg-final { scan-tree-dump-times "string_eq_min_fail" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "string_eq_max_fail" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "string_lt_0_fail"   1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "string_eq_0_fail"   1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "string_gt_0_fail"   1 "optimized" } } */
