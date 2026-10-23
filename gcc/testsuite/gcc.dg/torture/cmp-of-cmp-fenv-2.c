/* PR middle-end/127786 */
/* Combining two comparisons that are both evaluated must not change
   whether FE_INVALID is raised for a quiet NaN.  */
/* { dg-do run } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target fenv_exceptions_double } */

#include <fenv.h>

#define NOIPA __attribute__ ((noipa))

/* Quiet | signaling: > is evaluated, so NE would not signal.  */
NOIPA int unlt_or_gt (double a, double b)
{ return !__builtin_isgreaterequal (a, b) | (a > b); }
/* Signaling & quiet: EQ would not signal.  */
NOIPA int le_and_eq (double a, double b) { return (a <= b) & (a == b); }
/* Signaling, then quiet in an inner if: EQ would not signal.  */
NOIPA int le_then_eq (double a, double b)
{
  if (a <= b)
    if (a == b)
      return 1;
  return 0;
}
/* Signaling || quiet: < is always evaluated, UNLT would not signal.  */
NOIPA int lt_orif_uno (double a, double b)
{ return a < b || __builtin_isunordered (a, b); }

static const struct
{
  int (*fn) (double, double);
  /* Results for (NaN, 1), (0, 1), (1, 1) and (2, 1).  */
  int res[4];
} tests[] = {
  { unlt_or_gt,  { 1, 1, 0, 1 } },
  { le_and_eq,   { 0, 0, 1, 0 } },
  { le_then_eq,  { 0, 0, 1, 0 } },
  { lt_orif_uno, { 1, 1, 0, 0 } },
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
	/* Each of them raises FE_INVALID for the NaN only.  */
	if (invalid != (j == 0))
	  __builtin_abort ();
      }
  return 0;
}
