/* Check that arc profiling instrumentation code does not cause problems for
   a program that calls functions that are likely to be in a shared library.
   This was added to check the fix for PR target/5469, which prevents arc
   profiling code from being inserted between a call and the restore of the
   call-clobbered global pointer.  */

/* { dg-options "-fprofile-arcs" } */
/* { dg-do run { target native } } */

int rand (void);
void srand (unsigned int seed);

int globvar;

void
leave (int i)
{
  if (i != 0)
    abort ();
  exit (0);
}

void
doit ()
{
  srand (12);
  globvar = rand ();
  if (rand () > 0)
    globvar = 0;
  leave (globvar);
}

int
main ()
{
  doit ();
}
