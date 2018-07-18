/* PR c/61854 */
/* { dg-do compile } */
/* { dg-options "-std=gnu89 -pedantic -w" } */

int
main (void)
{
  // Comment.
  /* { dg-bogus "C\\+\\+ style comments are not allowed in ISO C90" "comments"  { target *-*-*} .-1 } */
  /* { dg-bogus "note: \[^\n\r]*reported only once" ""  { target *-*-*} .-2 } */
  return 0;
}
