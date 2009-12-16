/* Test that optimized out __thread var doesn't have its location
   referenced in debug info.  */
/* { dg-do link } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target tls } */

static __thread int vara;

int
foo (int b)
{
  return vara + b;
}

int
main (void)
{
  return foo (0);
}
