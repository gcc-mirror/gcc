/* { dg-do compile } */
/* { dg-options "-Wattributes" } */

/* Make sure #pragma can enable a warning.  */

#pragma GCC diagnostic warning "-Waddress"

void __attribute__((dj)) bar() { }	/* { dg-warning "warning: .* attribute directive ignored" } */

int i;

void
foo ()
{
  if (&i)	/* { dg-warning "warning: .* will always evaluate as 'true'" } */
    grill ();
}
