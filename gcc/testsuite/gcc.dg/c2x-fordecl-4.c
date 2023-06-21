/* Test for C99 declarations in for loops.  Test constraints are diagnosed with
   -Wc11-c2x-compat for C2X: struct and union tags can't be declared there
   (affirmed in response to DR#277).  Based on c99-fordecl-3.c.  */
/* { dg-do compile } */
/* { dg-options "-std=c2x -pedantic-errors -Wc11-c2x-compat" } */

void
foo (void)
{
  for (struct s { int p; } *p = 0; ;) /* { dg-warning "'struct s' declared in 'for' loop initial declaration" } */
    ;
  for (union u { int p; } *p = 0; ;) /* { dg-warning "'union u' declared in 'for' loop initial declaration" } */
    ;
}
