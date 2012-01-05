/* PR middle-end/44777 */
/* { dg-options "-O0" } */
/* A variant of gcc.c-torture/execute/comp-goto-2.c.  */

extern void abort (void);
extern void exit (int);

#ifdef STACK_SIZE
#define DEPTH ((STACK_SIZE) / 512 + 1)
#else
#define DEPTH 1000
#endif

#if ! defined (NO_LABEL_VALUES) && !defined (NO_TRAMPOLINES)
int
x (int a)
{
  __label__ xlab;
  void y (int a)
    {
      void *x = &&llab;
      if (a==-1)
	goto *x;
      if (a==0)
	goto xlab;
    llab:
      y (a-1);
    }
  y (a);
 xlab:;
  return a;
}
#endif

int
main ()
{
#if ! defined (NO_LABEL_VALUES) && !defined (NO_TRAMPOLINES)
  if (x (DEPTH) != DEPTH)
    abort ();
#endif
  exit (0);
}
