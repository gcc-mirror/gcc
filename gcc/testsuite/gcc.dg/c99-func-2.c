/* Test for C99 __func__: not a string constant.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

void
foo (void)
{
  __func__ "foo"; /* { dg-error "(parse|syntax) error" "before string constant" } */
}
