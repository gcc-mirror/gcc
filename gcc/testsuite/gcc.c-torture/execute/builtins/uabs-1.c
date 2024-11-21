/* Test for -fno-builtin-FUNCTION.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk>.  */
/* GCC normally handles uabs and ulabs as built-in functions even without
   optimization.  So test that with -fno-builtin-uabs, ulabs is so handled
   but uabs isn't.  */

int abs_called = 0;

extern unsigned int uabs (int);
extern unsigned long ulabs (long);
extern void abort (void);

void
main_test (void)
{
  if (ulabs (0) != 0)
    abort ();
  if (uabs (0) != 0)
    abort ();
  if (!abs_called)
    abort ();
}
