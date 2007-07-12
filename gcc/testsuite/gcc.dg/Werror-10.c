/* { dg-do compile } */
/* { dg-options "-Wattributes -Werror" } */
/* { dg-message "warnings being treated as errors" "" {target "*-*-*"} 0 } */

/* Make sure #pragma can work with -Werror.  */

#pragma GCC diagnostic error "-Waddress"

void __attribute__((dj)) bar() { }	/* { dg-error ".* attribute directive ignored" } */

int i;

void
foo ()
{
  if (&i)	/* { dg-error ".* will always evaluate as 'true'" } */
    grill ();
}
