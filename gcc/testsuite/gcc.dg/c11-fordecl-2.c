/* Test for C99 declarations in for loops.  Test constraints are diagnosed for
   C11: struct and union tags can't be declared there (affirmed in response to
   DR#277).  Based on c99-fordecl-3.c.  */
/* { dg-do compile } */
/* { dg-options "-std=c11 -pedantic-errors" } */

void
foo (void)
{
  for (struct s { int p; } *p = 0; ;) /* { dg-error "'struct s' declared in 'for' loop initial declaration" } */
    ;
  for (union u { int p; } *p = 0; ;) /* { dg-error "'union u' declared in 'for' loop initial declaration" } */
    ;
}
