/* { dg-do compile } */
/* { dg-options "-Walways-true -Wattributes" } */

/* Make sure the command line option enables the warning.  */

void __attribute__((dj)) bar() { }	/* { dg-warning "warning: .* attribute directive ignored" } */

int i;

void
foo ()
{
  if (&i)	/* { dg-warning "warning: .* will always evaluate as 'true'" } */
    grill ();
}
