extern void abort (void);
extern int abs_called;
extern int inside_main;

/* The ulabs call should have been optimized, but the uabs call
   shouldn't have been.  */

unsigned int
uabs (int x)
{
  if (inside_main)
    abs_called = 1;
  return (x < 0 ? -(unsigned int) x : x);
}

unsigned long
ulabs (long x)
{
  if (inside_main)
    abort ();
  return (x < 0 ? -(unsigned long) x : x);
}
