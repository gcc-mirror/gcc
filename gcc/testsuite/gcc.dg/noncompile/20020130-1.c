/* Test for ICE when using typedef for bad type.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk>.  */

void
foo (void)
{
  typedef int t[x]; /* { dg-error "undeclared|function" "x undeclared" } */
  t bar;
}
