/* { dg-do compile } */
/* { dg-options "-Wattributes" } */
/* { dg-message "some warnings being treated as errors" "" {target "*-*-*"} 0 } */
/* Make sure the pragma enables the error.  */

#pragma GCC diagnostic error "-Waddress"

void grill ();
void __attribute__((dj)) bar() { }	/* { dg-warning "attribute directive ignored" } */

int i;

void
foo ()
{
  if (&i)	/* { dg-error "will always evaluate as 'true'" } */
    grill ();
}
