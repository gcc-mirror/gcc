/* Test for constant expressions: broken optimization with const variables.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -O2" } */
/* Note: not using -pedantic since the -std option alone should be enough
   to give the correct behavior to conforming programs.  */

static const int ZERO = 0;
static const double DZERO = 0;

int *a;
int b;
long *c;

/* Assertion that n is a constant zero: so the conditional expression
   has type 'int *' instead of 'void *'.
*/
#define ASSERT_NPC(n)	(b = *(1 ? a : (n)))
/* Assertion that n is not a constant zero: so the conditional
   expressions has type 'void *' instead of 'int *'.
*/
#define ASSERT_NOT_NPC(n)	(c = (1 ? a : (void *)(__SIZE_TYPE__)(n)))

void
foo (void)
{
  ASSERT_NPC (0);
  ASSERT_NOT_NPC (ZERO);
  ASSERT_NPC (0 + 0);
  ASSERT_NOT_NPC (ZERO + 0); /* { dg-bogus "incompatible" "bogus null pointer constant" } */
  ASSERT_NOT_NPC (ZERO + ZERO); /* { dg-bogus "incompatible" "bogus null pointer constant" } */
  ASSERT_NPC (+0);
  ASSERT_NOT_NPC (+ZERO); /* { dg-bogus "incompatible" "bogus null pointer constant" } */
  ASSERT_NPC (-0);
  ASSERT_NOT_NPC (-ZERO); /* { dg-bogus "incompatible" "bogus null pointer constant" } */
  ASSERT_NPC ((char) 0);
  ASSERT_NOT_NPC ((char) ZERO);
  ASSERT_NPC ((int) 0);
  ASSERT_NOT_NPC ((int) ZERO);
  ASSERT_NPC ((int) 0.0);
  ASSERT_NOT_NPC ((int) DZERO);
  ASSERT_NOT_NPC ((int) +0.0); /* { dg-bogus "incompatible" "bogus null pointer constant" } */
  ASSERT_NOT_NPC ((int) (0.0+0.0)); /* { dg-bogus "incompatible" "bogus null pointer constant" } */
  ASSERT_NOT_NPC ((int) (double)0.0); /* { dg-bogus "incompatible" "bogus null pointer constant" } */
}
