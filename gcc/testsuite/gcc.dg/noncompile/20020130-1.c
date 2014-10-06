/* Test for ICE when using typedef for bad type.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk>.  */
/* { dg-options "-std=gnu89" } */

void
foo (void)
{
  typedef int t[x]; /* { dg-error "undeclared|function" "x undeclared" } */
  t bar;
}
