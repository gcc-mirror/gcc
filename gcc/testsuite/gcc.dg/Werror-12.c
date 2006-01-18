/* { dg-do compile } */
/* { dg-options "-Wattributes -Walways-true" } */

/* Make sure #pragma can override -Wfoo.  */

#pragma GCC diagnostic ignored "-Walways-true"

void __attribute__((dj)) bar() { }	/* { dg-warning "attribute directive ignored" } */

int i;

void
foo ()
{
  if (&i)	/* { dg-bogus "true" } */
    grill ();
}
