/* Test for -fno-builtin-FUNCTION.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk>.  */
/* GCC normally handles abs and labs as built-in functions even without
   optimization.  So test that with -fno-builtin-abs, labs is so handled
   but abs isn't.  */

int abs_called = 0;

extern int abs (int);
extern long labs (long);
extern void abort (void);

void
main_test (void)
{
  if (labs (0) != 0)
    abort ();
  if (abs (0) != 0)
    abort ();
  if (!abs_called)
    abort ();
}
