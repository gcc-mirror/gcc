/* { dg-do compile } */
/* { dg-options "-Wattributes -Werror=always-true" } */

/* Make sure -Werror-foo emits an error and not a warning */

void __attribute__((dj)) bar() { }	/* { dg-warning "warning: .* attribute directive ignored" } */

int i;

void
foo ()
{
  if (&i)	/* { dg-error "error: .* will always evaluate as 'true'" } */
    grill ();
}
