/* { dg-do run { target powerpc*-*-darwin* } } */
/* { dg-options "" } */
/* No options so 'long long' can be used.  */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef unsigned long long uint64_t;
typedef uint64_t ldbits[2];

union ldu 
{
  ldbits lb;
  long double ld;
};

static const struct {
  ldbits a;
  ldbits b;
  ldbits result;
} single_tests[] = {
  /* Test of values that add to near +Inf.  */
  { { 0x7FEFFFFFFFFFFFFFLL, 0xFC88000000000000LL },
    { 0x7C94000000000000LL, 0x0000000000000000LL },
    { 0x7FEFFFFFFFFFFFFFLL, 0x7C80000000000000LL } },
  { { 0x7FEFFFFFFFFFFFFFLL, 0x7C8FFFFFFFFFFFFFLL },
    { 0x792FFFFFFFFFFFFFLL, 0x0000000000000000LL },
    { 0x7FEFFFFFFFFFFFFFLL, 0x7C8FFFFFFFFFFFFFLL } },
  { { 0x7FEFFFFFFFFFFFFFLL, 0x7C8FFFFFFFFFFFFFLL },
    { 0x7930000000000000LL, 0xF5DFFFFFFFFFFFFFLL },
    /* correct result is: { 0x7FEFFFFFFFFFFFFFLL, 0x7C8FFFFFFFFFFFFFLL } */
    { 0x7FF0000000000000LL, 0x0000000000000000LL } },
  /* Test of values that add to +Inf.  */
  { { 0x7FEFFFFFFFFFFFFFLL, 0x7C8FFFFFFFFFFFFFLL },
    { 0x7930000000000000LL, 0x0000000000000000LL },
    { 0x7FF0000000000000LL, 0x0000000000000000LL } },
  /* Tests of Inf addition.  */
  { { 0x7FF0000000000000LL, 0x0000000000000000LL },
    { 0x0000000000000000LL, 0x0000000000000000LL },
    { 0x7FF0000000000000LL, 0x0000000000000000LL } },
  { { 0x7FF0000000000000LL, 0x0000000000000000LL },
    { 0x7FF0000000000000LL, 0x0000000000000000LL },
    { 0x7FF0000000000000LL, 0x0000000000000000LL } },
  /* Test of Inf addition producing NaN.  */
  { { 0x7FF0000000000000LL, 0x0000000000000000LL },
    { 0xFFF0000000000000LL, 0x0000000000000000LL },
    { 0x7FF8000000000000LL, 0x0000000000000000LL } },
  /* Tests of NaN addition.  */
  { { 0x7FF8000000000000LL, 0x0000000000000000LL },
    { 0x0000000000000000LL, 0x0000000000000000LL },
    { 0x7FF8000000000000LL, 0x7FF8000000000000LL } },
  { { 0x7FF8000000000000LL, 0x0000000000000000LL },
    { 0x7FF0000000000000LL, 0x0000000000000000LL },
    { 0x7FF8000000000000LL, 0x7FF8000000000000LL } },
  /* Addition of positive integers, with interesting rounding properties.  */
  { { 0x4690000000000000LL, 0x4330000000000000LL },
    { 0x4650000000000009LL, 0xC2FFFFFFFFFFFFF2LL },
    /* correct result is: { 0x4691000000000001LL, 0xC32C000000000000LL } */
    { 0x4691000000000001LL, 0xc32bfffffffffffeLL } },
  { { 0x4690000000000000LL, 0x4330000000000000LL },
    { 0x4650000000000008LL, 0x42F0000000000010LL },
    { 0x4691000000000001LL, 0xC32E000000000000LL } },
  { { 0x469FFFFFFFFFFFFFLL, 0x433FFFFFFFFFFFFFLL },
    { 0x4340000000000000LL, 0x3FF0000000000000LL },
    { 0x46A0000000000000LL, 0x0000000000000000LL } },
  { { 0x469FFFFFFFFFFFFFLL, 0x433FFFFFFFFFFFFFLL },
    { 0x4340000000000000LL, 0x0000000000000000LL },
    { 0x46A0000000000000LL, 0xBFF0000000000000LL } },
  /* Subtraction of integers, with cancellation.  */
  { { 0x4690000000000000LL, 0x4330000000000000LL },
    { 0xC690000000000000LL, 0xC330000000000000LL },
    { 0x0000000000000000LL, 0x0000000000000000LL } },
  { { 0x4690000000000000LL, 0x4330000000000000LL },
    { 0xC330000000000000LL, 0x0000000000000000LL },
    { 0x4690000000000000LL, 0x0000000000000000LL } },
  { { 0x4690000000000000LL, 0x4330000000000000LL },
    { 0xC330000000000000LL, 0x3FA0000000000000LL },
    { 0x4690000000000000LL, 0x3FA0000000000000LL } },
  { { 0x4690000000000000LL, 0x4330000000000000LL },
    { 0xC690000000000000LL, 0x3FA0000000000000LL },
    /* correct result is: { 0x4330000000000000LL, 0x3FA0000000000000LL } */
    { 0x4330000000000000LL, 0x0000000000000000LL } }
};
    
static int fail = 0;

static void
run_single_tests (void)
{
  size_t i;
  for (i = 0; i < sizeof (single_tests) / sizeof (single_tests[0]); i++)
    {
      union ldu a, b, result, expected;
      memcpy (a.lb, single_tests[i].a, sizeof (ldbits));
      memcpy (b.lb, single_tests[i].b, sizeof (ldbits));
      memcpy (expected.lb, single_tests[i].result, sizeof (ldbits));
      result.ld = a.ld + b.ld;
      if (memcmp (result.lb, expected.lb,
		  result.ld == result.ld ? sizeof (ldbits) : sizeof (double))
	  != 0)
	{
	  printf ("FAIL: %016llx %016llx + %016llx %016llx\n",
		  a.lb[0], a.lb[1], b.lb[0], b.lb[1]);
	  printf (" = %016llx %016llx not %016llx %016llx\n",
		  result.lb[0], result.lb[1], expected.lb[0], expected.lb[1]);
	  fail = 1;
	}
    }
}

int main(void)
{
  run_single_tests();
  if (fail)
    abort ();
  else
    exit (0);
}
