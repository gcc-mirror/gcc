/* { dg-require-effective-target lto } */
/* { dg-additional-sources "afdo-lto_priv-basic-1.c" } */
/* { dg-options "-O2 -flto -fdump-ipa-afdo" } */
/* { dg-require-profiling "-fauto-profile" } */ 

#define TRIP 1000000000

/* Check against exported symbols.  */
__attribute__ ((noinline, noipa)) void effect_1 () {}
__attribute__ ((noinline, noipa)) void effect_2 () {}
__attribute__ ((noinline, noipa)) static int foo () { return 5; }

/* Prevent GCC from optimizing the loop.  */
__attribute__ ((noinline, noipa)) int
use (int x)
{
  volatile int y = x;
  return x;
}

extern void global ();

int
main ()
{
  for (int i = 0; i < TRIP; i++)
    {
      /* Call only 50% of the time.  */
      if (use (i) < TRIP / 2)
	global ();

      if (foo () < 5)
	/* This function is never called.  */
	effect_1 ();
      else
	effect_2 ();
    }
}

/* Check that the annotation actually occurs.  */
/* { dg-final-use-autofdo { scan-ipa-dump-not "No afdo profile for main" afdo } } */
/* { dg-final-use-autofdo { scan-ipa-dump-not "No afdo profile for use" afdo } } */
/* { dg-final-use-autofdo { scan-ipa-dump-not "No afdo profile for foo" afdo } } */
/* { dg-final-use-autofdo { scan-ipa-dump-not "No afdo profile for effect_2" afdo } } */

/* There should be no collision with effect_1 from afdo-lto_priv-basic-1.c.  */
/* { dg-final-use-autofdo { scan-ipa-dump "No afdo profile for effect_1" afdo } } */
