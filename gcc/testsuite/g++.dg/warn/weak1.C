// { dg-do run }
// { dg-require-weak "" }

extern void foo (void) __attribute__ ((weak));

int
main ()
{
  if (&foo)
    foo ();

  return 0;
}
