/* Test for C99 declarations in for loops.  Test constraints: struct
   and union tags can't be declared there (affirmed in response to
   DR#277).  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

void
foo (void)
{
  for (struct s { int p; } *p = 0; ;) /* { dg-error "error: 'struct s' declared in 'for' loop initial declaration" } */
    ;
  for (union u { int p; } *p = 0; ;) /* { dg-error "error: 'union u' declared in 'for' loop initial declaration" } */
    ;
}
