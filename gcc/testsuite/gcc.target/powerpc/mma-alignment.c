/* { dg-do run } */
/* { dg-require-effective-target hard_float } */
/* { dg-options "-O2 -mhard-float" } */

#include <stdlib.h>

/* The MMA types below are enabled for pre-power10 compiles, because the
   built-ins that use them must always be initialized in case the user has
   a target attribute or pragma on a function that uses the MMA built-ins.
   Since the test below doesn't need any other MMA support, we can enable
   this test case on basically any cpu that has hard floating point
   registers.  */

struct
{
  int __attribute__ ((__aligned__)) ivar;
  __vector_pair pair;
  __vector_quad quad;
} s;

int
main (void)
{
  /* Verify default alignment is 16-byte aligned (BIGGEST_ALIGNMENT).
     This may change in the future, but that is an ABI break, so this
     hardcoded test case is here to be a noisy FAIL as a warning, in
     case the ABI change was unintended and unwanted.  An example of where
     this can break an ABI is in glibc's struct _Unwind_Exception.  */
  if (__alignof__ (s.ivar) != 16)
    abort ();

  /* Verify __vector_pair types are 32-byte aligned.  */
  if (__alignof__ (s.pair) != 32)
    abort ();

  /* Verify __vector_quad types are 64-byte aligned.  */
  if (__alignof__ (s.quad) != 64)
    abort ();

  return 0;
}
