/* { dg-do run { target i?86-*-* x86_64-*-* } } */
/* { dg-require-effective-target cet } */
/* { dg-additional-options "-fcf-protection" } */

#define DEPTH 1000

int
x(int a)
{
  __label__ xlab;
  void y(int a)
    {
      if (a==0)
	goto xlab;
      y (a-1);
    }
  y (a);
 xlab:;
  return a;
}

int
main ()
{
  if (x (DEPTH) != DEPTH)
    __builtin_abort ();

  return 0;
}
