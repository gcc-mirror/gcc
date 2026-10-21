/* { dg-do compile { target { lp64 } } } */
/* { dg-options "-pg -mfentry" } */

void
profileme (void)
{
  /* { dg-final { scan-assembler "brasl\t0,__fentry__" } } */
}

/* During the removal of -m31, the else branch for TARGET_64BIT in function
   s390_function_profiler() was rewritten incorrectly, causing to emit two
   __fentry__ calls.  Verify that with -mfentry exactly one call to __fentry__
   is emitted and that -mno-nop-mcount doesn't lead to a sequence which saves
   %r14 and a call.  */
/* { dg-final { scan-assembler-times "__fentry__" 1 } } */
/* { dg-final { scan-assembler-times "brasl\t" 1 } } */
/* { dg-final { scan-assembler-not "stg\t%r14,8\\(%r15\\)" } } */
