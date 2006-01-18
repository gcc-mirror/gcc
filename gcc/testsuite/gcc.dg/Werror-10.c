/* { dg-do compile } */
/* { dg-options "-Wattributes -Werror" } */
/* { dg-warning "warnings being treated as errors" "" {target "*-*-*"} 0 } */

/* Make sure #pragma can work with -Werror.  */

#pragma GCC diagnostic error "-Walways-true"

void __attribute__((dj)) bar() { }	/* { dg-warning "warning: .* attribute directive ignored" } */

int i;

void
foo ()
{
  if (&i)	/* { dg-error "error: .* will always evaluate as 'true'" } */
    grill ();
}
