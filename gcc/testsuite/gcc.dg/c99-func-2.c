/* Test for C99 __func__: not a string constant.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

void
foo (void)
{
  __func__ "foo"; /* { dg-bogus "warning" "warning in place of error" } */
  /* { dg-error "parse error" "__func__ not string constant" { xfail *-*-* } 9 } */
}
