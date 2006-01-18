/* { dg-do compile } */
/* { dg-options "-Wattributes" } */

/* Make sure the pragma enables the error.  */

#pragma GCC diagnostic error "-Walways-true"

void __attribute__((dj)) bar() { }	/* { dg-warning "warning: .* attribute directive ignored" } */

int i;

void
foo ()
{
  if (&i)	/* { dg-error "error: .* will always evaluate as 'true'" } */
    grill ();
}
