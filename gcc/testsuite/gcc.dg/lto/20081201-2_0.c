/* { dg-lto-options {{-O3 -flto -flto-partition=1to1}} } */

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
