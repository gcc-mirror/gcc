/* CYGNUS LOCAL -- meissner/no label values */
#ifdef STACK_SIZE
#define DEPTH ((STACK_SIZE) / 512 + 1)
#else
#define DEPTH 1000
#endif

#ifndef NO_LABEL_VALUES
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
#ifndef NO_LABEL_VALUES
  if (x (DEPTH) != DEPTH)
    abort ();
#endif
  exit (0);
}
/* END CYGNUS LOCAL -- meissner/no label values */
