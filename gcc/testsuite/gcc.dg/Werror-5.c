/* { dg-do compile } */
/* { dg-options "-Walways-true -Wattributes -Werror" } */
/* { dg-warning "warnings being treated as errors" "" {target "*-*-*"} 0 } */

/* Make sure -Werror turns warnings in to errors.  */

void __attribute__((dj)) bar() { }	/* { dg-warning "warning: .* attribute directive ignored" } */

int i;

void
foo ()
{
  if (&i)	/* { dg-warning "warning: .* will always evaluate as 'true'" } */
    grill ();
}
