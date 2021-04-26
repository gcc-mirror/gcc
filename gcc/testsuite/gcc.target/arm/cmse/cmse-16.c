/* This test is executed only if the execution engine supports CMSE instructions.  */
/* Note that it "passes" on qemu-arm while it shouldn't because that simulator
   does not model secure memory: it's better to skip it in this case, and rely
   on do-what-default as set in cmse.exp. (It works as expected with
   qemu-system-mode).  */
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
