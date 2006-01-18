/* { dg-do compile } */
/* { dg-options "-Wattributes -Werror" } */
/* { dg-warning "warnings being treated as errors" "" {target "*-*-*"} 0 } */

/* Make sure #pragma can override -Werror.  */

#pragma GCC diagnostic warning "-Walways-true"

void __attribute__((dj)) bar() { }	/* { dg-warning "warning: .* attribute directive ignored" } */

int i;

void
foo ()
{
  if (&i)	/* { dg-warning "warning: .* will always evaluate as 'true'" } */
    grill ();
}
