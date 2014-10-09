/* { dg-do compile } */
/* { dg-options "-Waddress -Wattributes" } */

/* Make sure the command line option enables the warning.  */

void grill ();
void __attribute__((dj)) bar() { }	/* { dg-warning ".* attribute directive ignored" } */

int i;

void
foo ()
{
  if (&i)	/* { dg-warning ".* will always evaluate as 'true'" } */
    grill ();
}
