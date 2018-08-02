/* PR c/61854 */
/* { dg-do preprocess } */
/* { dg-options "-std=c89" } */

void
foo (void)
{
#if 0
  // Do not error here.
#endif
#if 1
  // But error here.
#endif
  /* { dg-error "C\\+\\+ style comments are not allowed in ISO C90" "comments"  { target *-*-*} 12 } */
  /* { dg-message "note: \[^\n\r]*reported only once" ""  { target *-*-*} 12 } */
}
