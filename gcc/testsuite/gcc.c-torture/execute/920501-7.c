/* { dg-require-effective-target label_values } */
/* { dg-require-effective-target trampolines } */
/* { dg-additional-options "-DSTACK_SIZE=[dg-effective-target-value stack_size]" { target { stack_size } } } */

#ifdef STACK_SIZE
#define DEPTH ((STACK_SIZE) / 512 + 1)
#else
#define DEPTH 1000
#endif

x(a)
{
  __label__ xlab;
  void y(a)
    {
      if (a==0)
	goto xlab;
      y (a-1);
    }
  y (a);
 xlab:;
  return a;
}

main ()
{
  if (x (DEPTH) != DEPTH)
    abort ();

  exit (0);
}
