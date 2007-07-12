/* { dg-do compile } */
/* { dg-options "" } */

/* This one is the baseline.  Make sure with no option we get no
   warnings.  */

void __attribute__((dj)) bar() { }	/* { dg-warning ".* attribute directive ignored" } */

int i;

void
foo ()
{
  if (&i)
    grill ();
}
