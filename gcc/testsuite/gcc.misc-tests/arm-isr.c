#ifndef __thumb__
/* There used to be a couple of bugs in the ARM's prologue and epilogue
   generation for ISR routines.  The wrong epilogue instruction would be
   generated to restore the IP register if it had to be pushed onto the
   stack, and the wrong offset was being computed for local variables if
   r0 - r3 had to be saved.  This tests for both of these cases.  */

int z = 9;

int
bar (void)
{
  return z;
}

int
foo (int a, int b, int c, int d, int e, int f, int g, int h)
{
  volatile int i = (a + b) - (g + h) + bar ();
  volatile int j = (e + f) - (c + d);

  return a + b + c + d + e + f + g + h + i + j;
}

int foo1 (int a, int b, int c, int d, int e, int f, int g, int h) __attribute__ ((interrupt ("IRQ")));

int
foo1 (int a, int b, int c, int d, int e, int f, int g, int h)
{
  volatile int i = (a + b) - (g + h) + bar ();
  volatile int j = (e + f) - (c + d);

  return a + b + c + d + e + f + g + h + i + j;
}
#endif

int
main (void)
{
#ifndef __thumb__
  if (foo (1, 2, 3, 4, 5, 6, 7, 8) != 32)
    abort ();
    
  if (foo1 (1, 2, 3, 4, 5, 6, 7, 8) != 32)
    abort ();
#endif
  exit (0);
}
