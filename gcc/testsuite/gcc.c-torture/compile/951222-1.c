/* { dg-require-stack-size "10000+3*8" } */
/* { dg-additional-options "-std=gnu89" } */

extern long long foo ();

long long
sub1 ()
{
  char junk[10000];
  register long long a, b, c;

  b = foo ();

  setjmp ();
  a = foo ();
  c = a - b;
  return c;
}
