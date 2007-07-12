/* { dg-do compile } */
/* { dg-options "-Waddress -Wattributes -Werror" } */
/* { dg-message "warnings being treated as errors" "" {target "*-*-*"} 0 } */

/* Make sure -Werror turns warnings in to errors.  */

void __attribute__((dj)) bar() { }	/* { dg-error ".* attribute directive ignored" } */

int i;

void
foo ()
{
  if (&i)	/* { dg-error ".* will always evaluate as 'true'" } */
    grill ();
}
