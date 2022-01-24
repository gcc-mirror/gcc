/* Verify that notes after warnings for array and VLA parameters show
   the array form.
   { dg-do compile }
   { dg-options "-Wall" } */

void fia5 (int[5]);

void gia3_fia5 (void)
{
  int a[3];
  fia5 (a);             // { dg-warning "-Wstringop-overflow" }
                        // { dg-message "argument 1 of type 'int\\\[5]'" "note" { target *-*-* } .-1 }
}


/* The type of the argument would ideall be 'int[n]' but the variable
   bound is lost/cleared by free-lang-data and never makes it into
   the middle end.  An (inferior) alternative would be 'int[*]' but
   the pretty printer doesn't know how to format the star.  A better
   solution might be to introduce a new notation, like 'int[$1]',
   where the $1 refers to the VLA argument bound.  */
void fvla (int n, int[n]);

void gia3_fvla (void)
{
  int a[3];
  fvla (sizeof a, a);   // { dg-warning "-Wstringop-overflow" }
                        // { dg-message "argument 2 of type 'int\\\[]'" "note" { target *-*-* } .-1 }
}
