/* Test for constant expressions: details of what is a null pointer
   constant.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990" } */
/* Note: not using -pedantic since the -std option alone should be enough
   to give the correct behavior to conforming programs.  If -pedantic is
   needed to make (say) (0, 0) not be a constant expression, this is a
   bug.
*/

int *a;
int b;
long *c;

/* Assertion that n is a null pointer constant: so the conditional expression
   has type 'int *' instead of 'void *'.
*/
#define ASSERT_NPC(n)	(b = *(1 ? a : (n)))
/* Assertion that n is not a null pointer constant: so the conditional
   expresions has type 'void *' instead of 'int *'.
*/
#define ASSERT_NOT_NPC(n)	(c = (1 ? a : (n)))

void
foo (void)
{
  ASSERT_NPC (0);
  ASSERT_NPC ((void *)0);
  ASSERT_NOT_NPC ((void *)(void *)0); /* { dg-bogus "incompatible" "bogus null pointer constant" { xfail *-*-* } } */
  ASSERT_NOT_NPC ((void *)(char *)0); /* { dg-bogus "incompatible" "bogus null pointer constant" { xfail *-*-* } } */
  ASSERT_NOT_NPC ((void *)(0, 0)); /* { dg-bogus "incompatible" "bogus null pointer constant" { xfail *-*-* } } */
  ASSERT_NOT_NPC ((void *)(&"Foobar"[0] - &"Foobar"[0])); /* { dg-bogus "incompatible" "bogus null pointer constant" { xfail *-*-* } } */
  /* This last one is a null pointer constant in C99 only.  */
  ASSERT_NOT_NPC ((void *)(1 ? 0 : (0, 0))); /* { dg-bogus "incompatible" "bogus null pointer constant" { xfail *-*-* } } */
}
