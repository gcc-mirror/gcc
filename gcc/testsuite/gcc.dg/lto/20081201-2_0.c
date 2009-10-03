/* { dg-lto-options {{-O3 -fwhopr}} } */

/* Test that cross-TU inlining works.  */

extern void abort ();
extern void exit (int);
extern void *foo (void);

int
main ()
{
  if (foo () != __builtin_return_address (0))
    abort ();

  exit (0);
}
