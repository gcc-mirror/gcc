/* { dg-do compile { target { ia32 } } } */
/* { dg-options "-O2 -mpreferred-stack-boundary=2" } */

extern void fn2 ();
long long a, b;

void fn1 ()
{
  long long c = a;
  a = b ^ a;
  fn2 ();
  a = c;
}
