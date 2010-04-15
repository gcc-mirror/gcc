/* { dg-do run } */
/* { dg-options "-O2 -fno-early-inlining -fipa-pta" } */

static void __attribute__((noinline,noclone))
clobber_me (int *p, int how)
{
  *p = how;
}

/* When foo is inlined into main we have to make sure to adjust
   main()s IPA CLOBBERED set according to the decl remappings
   inlining does.  */

static int
foo (void)
{
  int a = 0;
  clobber_me (&a, 1);
  return a;
}

extern void abort (void);

int main()
{
  if (foo () != 1)
    abort ();

  return 0;
}
