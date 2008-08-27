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
  /* { dg-error "'for' loop initial declarations are only allowed in C99 mode" "declaration in for loop" { target *-*-* } 10 } */
  /* { dg-message "note: use option -std=c99 or -std=gnu99 to compile your code" "note" { target *-*-* } 10 }} */
}
