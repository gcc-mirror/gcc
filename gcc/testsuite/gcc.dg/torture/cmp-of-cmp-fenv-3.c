/* PR middle-end/127786 */
/* Combining two comparisons of the same operands must not change whether
   FE_INVALID is raised for a quiet NaN.  */
/* { dg-do run } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target fenv_exceptions_double } */

#include <fenv.h>

#define NOIPA __attribute__ ((noipa))

/* Signaling | signaling: LTGT, still signals.  */
NOIPA int lt_or_gt (double a, double b) { return (a < b) | (a > b); }
/* Signaling | signaling: always true for ordered operands.  */
NOIPA int lt_or_ge (double a, double b) { return (a < b) | (a >= b); }
/* Signaling ^ signaling: LTGT, still signals.  */
NOIPA int le_xor_ge (double a, double b) { return (a <= b) ^ (a >= b); }
/* Quiet & quiet: LT would signal.  */
NOIPA int ord_and_unlt (double a, double b)
{ return !__builtin_isunordered (a, b) & !__builtin_isgreaterequal (a, b); }
/* Quiet && quiet: LT would signal.  */
NOIPA int ord_andif_unlt (double a, double b)
{ return !__builtin_isunordered (a, b) && !__builtin_isgreaterequal (a, b); }
/* Quiet == quiet: GT would signal.  */
NOIPA int eq_eq_unlt (double a, double b)
{ return (a == b) == !__builtin_isgreaterequal (a, b); }
/* Quiet | quiet: UNEQ, still quiet.  */
NOIPA int uno_or_eq (double a, double b)
{ return __builtin_isunordered (a, b) | (a == b); }
/* Quiet || signaling: > is not evaluated for a NaN, so NE is fine.  */
NOIPA int unlt_orif_gt (double a, double b)
{ return !__builtin_isgreaterequal (a, b) || (a > b); }

static const struct
{
  int (*fn) (double, double);
  int signals;
  /* Results for (NaN, 1), (0, 1), (1, 1) and (2, 1).  */
  int res[4];
} tests[] = {
  { lt_or_gt,       1, { 0, 1, 0, 1 } },
  { lt_or_ge,       1, { 0, 1, 1, 1 } },
  { le_xor_ge,      1, { 0, 1, 0, 1 } },
  { ord_and_unlt,   0, { 0, 1, 0, 0 } },
  { ord_andif_unlt, 0, { 0, 1, 0, 0 } },
  { eq_eq_unlt,     0, { 0, 0, 0, 1 } },
  { uno_or_eq,      0, { 1, 0, 1, 0 } },
  { unlt_orif_gt,   0, { 1, 1, 0, 1 } },
};

int
main (void)
{
  static const double as[4] = { __builtin_nan (""), 0.0, 1.0, 2.0 };

  for (unsigned i = 0; i < sizeof (tests) / sizeof (tests[0]); i++)
    for (unsigned j = 0; j < 4; j++)
      {
	feclearexcept (FE_INVALID);
	int r = tests[i].fn (as[j], 1.0);
	int invalid = fetestexcept (FE_INVALID) != 0;
	if (r != tests[i].res[j])
	  __builtin_abort ();
	if (invalid != (j == 0 && tests[i].signals))
	  __builtin_abort ();
      }
  return 0;
}
