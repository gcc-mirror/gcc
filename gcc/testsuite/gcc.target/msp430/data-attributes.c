/* { dg-do run } */
/* { dg-options "-O2" } */

/* This test checks that persistent and noinit data are handled correctly.  */

extern void __crt0_start (void) __attribute__ ((noreturn));
extern void abort (void) __attribute__ ((noreturn));
extern void exit (int) __attribute__ ((noreturn));

int a;
int b = 0;
int c = 1;
int __attribute__((noinit)) d;
int __attribute__((persistent)) e = 2;

int
main (void)
{
  /* Make sure that the C startup code has correctly initialised the ordinary variables.  */
  if (a != 0)
    abort ();

#ifndef __MSP430X_LARGE__
  /* For non-large targets we use the ordinary msp430-sim.ld linker script.
     This does not support FLASH, and as a side effect it does not support
     reinitialising initialised data.  Hence we only test b and c if this
     is the first time through this test, or large support has been enabled.  */
  if (e == 2)
#endif
  if (b != 0 || c != 1)
    abort ();
  
  switch (e)
    {
    case 2:
      /* First time through - change all the values.  */
      a = b = c = d = e = 3;
      break;

    case 3:
      /* Second time through - make sure that d has not been reset.  */
      if (d != 3)
	abort ();
      exit (0);

    default:
      /* Any other value for e is an error.  */
      abort ();
    }

  /* Simulate a processor reset by calling the C startup code.  */
  __crt0_start ();

  /* Should never reach here.  */
  abort ();
}
