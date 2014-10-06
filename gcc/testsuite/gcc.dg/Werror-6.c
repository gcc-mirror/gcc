/* { dg-do compile } */
/* { dg-options "-Wattributes -Werror=address" } */
/* { dg-message "some warnings being treated as errors" "" {target "*-*-*"} 0 } */
/* Make sure -Werror=foo emits an error and not a warning */

void grill ();
void __attribute__((dj)) bar() { }	/* { dg-warning ".* attribute directive ignored" } */

int i;

void
foo ()
{
  if (&i)	/* { dg-error ".* will always evaluate as 'true'" } */
    grill ();
}
