#ifdef STACK_SIZE
#define DEPTH ((STACK_SIZE) / 512 + 1)
#else
#define DEPTH 1000
#endif

#if ! defined (NO_LABEL_VALUES) && !defined (NO_TRAMPOLINES)
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
#endif

main ()
{
#if ! defined (NO_LABEL_VALUES) && !defined (NO_TRAMPOLINES)
  if (x (DEPTH) != DEPTH)
    abort ();
#endif
  exit (0);
}
