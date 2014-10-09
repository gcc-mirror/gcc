/* { dg-do compile } */
/* { dg-options "-Wattributes -Werror" } */
/* { dg-message "warnings being treated as errors" "" {target "*-*-*"} 0 } */

/* Make sure #pragma can override -Werror.  */

#pragma GCC diagnostic warning "-Waddress"

void grill ();
void __attribute__((dj)) bar() { }	/* { dg-error ".* attribute directive ignored" } */

int i;

void
foo ()
{
  if (&i)	/* { dg-warning ".* will always evaluate as 'true'" } */
    grill ();
}
