/* { dg-do run } */
/* { dg-options "-O2 -fno-early-inlining -fipa-pta" } */

static int *__attribute__((noinline,noclone))
pass_me (int *p)
{
  return p;
}

/* When foo is inlined into main we have to make sure to adjust
   main()s IPA CLOBBERED set according to the decl remappings
   inlining does.  */

static int
foo (void)
{
  int a = 0;
  int *p = pass_me (&a);
  *p = 1;
  return a;
}

extern void abort (void);

int main()
{
  if (foo () != 1)
    abort ();

  return 0;
}
