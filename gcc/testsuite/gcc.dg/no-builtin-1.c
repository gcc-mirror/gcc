/* Test for -fno-builtin-FUNCTION.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk>.  */
/* { dg-do run } */
/* { dg-options "-fno-builtin-abs" } */

/* GCC normally handles abs and labs as built-in functions even without
   optimization.  So test that with -fno-builtin-abs, labs is so handled
   but abs isn't.  */

int abs_called = 0;

extern int abs (int);
extern long labs (long);
extern void abort (void);
extern void exit (int);

int
main (void)
{
  if (labs (0) != 0)
    abort ();
  if (abs (0) != 0)
    abort ();
  if (!abs_called)
    abort ();
  exit (0);
}

/* The labs call above should have been optimized, but the abs call
   shouldn't have been.  */

static int
abs (int x)
{ /* { dg-warning "static" "static decl warning" } */
  abs_called = 1;
  return (x < 0 ? -1 : x);
}

static long
labs (long x)
{
  abort ();
}
