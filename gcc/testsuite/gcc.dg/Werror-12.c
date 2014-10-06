/* { dg-do compile } */
/* { dg-options "-Wattributes -Waddress" } */

/* Make sure #pragma can override -Wfoo.  */

#pragma GCC diagnostic ignored "-Waddress"

void grill ();
void __attribute__((dj)) bar() { }	/* { dg-warning "attribute directive ignored" } */

int i;

void
foo ()
{
  if (&i)	/* { dg-bogus "true" } */
    grill ();
}
