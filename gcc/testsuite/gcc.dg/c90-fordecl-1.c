/* Test for C99 declarations in for loops - rejection in C90 mode.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1990 -pedantic-errors" } */

void
foo (void)
{
  int j = 0;
  for (int i = 1; i <= 10; i++) /* { dg-bogus "warning" "warning in place of error" } */
    j += i;
  /* { dg-error "parse|decl" "declaration in for loop" { target *-*-* } 10 } */
}
