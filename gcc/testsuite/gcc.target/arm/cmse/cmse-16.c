/* { dg-do run } */
/* { dg-options "-Os -mcmse" }  */

#include <arm_cmse.h>

int
foo (void)
{
  return cmse_nonsecure_caller ();
}

int
main (void)
{
  /* Return success (0) if main is secure, ie if cmse_nonsecure_caller/foo
     returns false (0).  */
  return foo ();
}
