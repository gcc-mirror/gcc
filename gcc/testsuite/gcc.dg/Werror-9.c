/* { dg-do compile } */
/* { dg-options "-Wattributes" } */

/* Make sure #pragma can enable a warning as an error.  */

#pragma GCC diagnostic error "-Waddress"

void __attribute__((dj)) bar() { }	/* { dg-warning ".* attribute directive ignored" } */

int i;

void
foo ()
{
  if (&i)	/* { dg-error ".* will always evaluate as 'true'" } */
    grill ();
}
